package com.monicahq.mcp.registry;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Utility class providing common schema creation patterns for MCP tool registries.
 *
 * This class extracts reusable schema building methods from McpToolRegistry,
 * allowing all domain registries to share consistent schema patterns for:
 * - ID parameters
 * - Pagination (list) parameters
 * - Common field types (strings, integers, booleans, dates)
 * - Update schemas (ID + create schema properties)
 *
 * All methods are static for easy reuse across domain registries.
 */
public final class SchemaBuilders {

    private SchemaBuilders() {
        // Utility class - prevent instantiation
    }

    /**
     * Creates a schema for tools that take only an ID parameter.
     *
     * @param description description of what the ID represents (e.g., "Contact ID")
     * @return schema Map with required id property
     */
    public static Map<String, Object> createIdSchema(String description) {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "id", Map.of(
                    "type", "integer",
                    "description", description
                )
            ),
            "required", List.of("id")
        );
    }

    /**
     * Creates a schema for list operations with pagination support.
     * Includes page, limit, and optional search parameters.
     *
     * @return schema Map with pagination properties
     */
    public static Map<String, Object> createListSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "page", Map.of(
                    "type", "integer",
                    "description", "Page number (starting from 1)",
                    "default", 1
                ),
                "limit", Map.of(
                    "type", "integer",
                    "description", "Number of items per page",
                    "default", 10,
                    "maximum", 100
                ),
                "search", Map.of(
                    "type", "string",
                    "description", "Search query"
                )
            )
        );
    }

    /**
     * Creates a schema for discovery tools that take no parameters.
     * These tools simply list available options from the Monica API.
     *
     * @return schema Map with no properties
     */
    public static Map<String, Object> createListOnlySchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(),
            "additionalProperties", false,
            "description", "No parameters required - this discovery tool lists available options"
        );
    }

    /**
     * Creates an update schema by combining an ID field with properties from a create schema.
     * Removes the "required" constraint since updates typically allow partial updates.
     *
     * @param createSchema the create schema to base the update schema on
     * @return schema Map with id and all properties from createSchema
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> createUpdateSchema(Map<String, Object> createSchema) {
        Map<String, Object> result = new HashMap<>();
        result.put("type", "object");

        Map<String, Object> properties = new HashMap<>();
        properties.put("id", Map.of(
            "type", "integer",
            "description", "The ID of the item to update"
        ));

        // Add all properties from the create schema
        Object createProperties = createSchema.get("properties");
        if (createProperties instanceof Map) {
            properties.putAll((Map<String, Object>) createProperties);
        }

        result.put("properties", properties);
        result.put("required", List.of("id"));

        return result;
    }

    // ========== Field Builder Methods ==========

    /**
     * Creates a string field schema.
     *
     * @param description field description
     * @return field schema Map
     */
    public static Map<String, Object> stringField(String description) {
        return Map.of(
            "type", "string",
            "description", description
        );
    }

    /**
     * Creates a string field schema with maximum length constraint.
     *
     * @param description field description
     * @param maxLength maximum string length
     * @return field schema Map
     */
    public static Map<String, Object> stringField(String description, int maxLength) {
        return Map.of(
            "type", "string",
            "description", description,
            "maxLength", maxLength
        );
    }

    /**
     * Creates an integer field schema.
     *
     * @param description field description
     * @return field schema Map
     */
    public static Map<String, Object> integerField(String description) {
        return Map.of(
            "type", "integer",
            "description", description
        );
    }

    /**
     * Creates a boolean field schema.
     *
     * @param description field description
     * @return field schema Map
     */
    public static Map<String, Object> booleanField(String description) {
        return Map.of(
            "type", "boolean",
            "description", description
        );
    }

    /**
     * Creates a boolean field schema with default value.
     *
     * @param description field description
     * @param defaultValue the default value
     * @return field schema Map
     */
    public static Map<String, Object> booleanField(String description, boolean defaultValue) {
        return Map.of(
            "type", "boolean",
            "description", description,
            "default", defaultValue
        );
    }

    /**
     * Creates a date field schema (YYYY-MM-DD format).
     *
     * @param description field description
     * @return field schema Map
     */
    public static Map<String, Object> dateField(String description) {
        return Map.of(
            "type", "string",
            "format", "date",
            "description", description
        );
    }

    /**
     * Creates a date-time field schema (ISO 8601 format).
     *
     * @param description field description
     * @return field schema Map
     */
    public static Map<String, Object> dateTimeField(String description) {
        return Map.of(
            "type", "string",
            "format", "date-time",
            "description", description
        );
    }

    /**
     * Creates an array field schema.
     *
     * @param description field description
     * @param itemType the type of array items (e.g., "integer", "string")
     * @return field schema Map
     */
    public static Map<String, Object> arrayField(String description, String itemType) {
        return Map.of(
            "type", "array",
            "description", description,
            "items", Map.of("type", itemType)
        );
    }

    /**
     * Creates an enum field schema.
     *
     * @param description field description
     * @param enumValues the allowed enum values
     * @return field schema Map
     */
    public static Map<String, Object> enumField(String description, List<String> enumValues) {
        return Map.of(
            "type", "string",
            "description", description,
            "enum", enumValues
        );
    }
}
