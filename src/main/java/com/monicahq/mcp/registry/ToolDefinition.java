package com.monicahq.mcp.registry;

import java.util.HashMap;
import java.util.Map;

/**
 * Immutable record class encapsulating MCP tool metadata.
 *
 * This record replaces the inner McpTool class from McpToolRegistry
 * and provides a clean, immutable representation of a tool definition
 * for the MCP protocol.
 *
 * @param name the unique identifier for the tool (e.g., "contact_create")
 * @param description a human-readable description of what the tool does
 * @param inputSchema the JSON Schema defining the tool's input parameters
 * @param category optional grouping category for client-side organization
 */
public record ToolDefinition(
    String name,
    String description,
    Map<String, Object> inputSchema,
    String category
) {

    /**
     * Creates a ToolDefinition without a category.
     *
     * @param name the tool name
     * @param description the tool description
     * @param inputSchema the input schema
     */
    public ToolDefinition(String name, String description, Map<String, Object> inputSchema) {
        this(name, description, inputSchema, null);
    }

    /**
     * Converts this ToolDefinition to a Map representation for the MCP protocol.
     * The resulting map contains name, description, and inputSchema fields.
     * If a category is present, it's added as x-category metadata in the schema.
     *
     * @return a Map suitable for serialization in MCP tool list responses
     */
    public Map<String, Object> toMap() {
        Map<String, Object> result = new HashMap<>();
        result.put("name", name);
        result.put("description", description);

        // Add category as custom metadata in schema if present
        if (category != null && !category.isEmpty()) {
            Map<String, Object> enhancedSchema = new HashMap<>(inputSchema);
            enhancedSchema.put("x-category", category);
            result.put("inputSchema", enhancedSchema);
        } else {
            result.put("inputSchema", inputSchema);
        }

        return result;
    }
}
