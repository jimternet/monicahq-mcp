package com.monicahq.mcp.registry;

import com.monicahq.mcp.service.RelationshipService;
import com.monicahq.mcp.service.RelationshipTypeGroupService;
import com.monicahq.mcp.service.RelationshipTypeService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Map;

/**
 * Domain-specific tool registry for Relationship operations.
 *
 * This registry handles relationship-related MCP tools:
 * - Relationship CRUD operations (create, get, update, delete, list)
 * - Relationship type discovery operations (get, list)
 * - Relationship type group discovery operations (get, list)
 *
 * Relationships connect contacts to each other with a specific type
 * (e.g., "mother", "friend", "colleague"). The type system is organized
 * into groups for easier discovery.
 *
 * The registry delegates execution to RelationshipService, RelationshipTypeService,
 * and RelationshipTypeGroupService.
 */
@Component
@Slf4j
public class RelationshipToolRegistry extends AbstractDomainToolRegistry {

    private static final String DOMAIN = "Relationship";
    private static final String CATEGORY = "Relationship Management";

    private final RelationshipService relationshipService;
    private final RelationshipTypeService relationshipTypeService;
    private final RelationshipTypeGroupService relationshipTypeGroupService;

    public RelationshipToolRegistry(
            RelationshipService relationshipService,
            RelationshipTypeService relationshipTypeService,
            RelationshipTypeGroupService relationshipTypeGroupService) {
        this.relationshipService = relationshipService;
        this.relationshipTypeService = relationshipTypeService;
        this.relationshipTypeGroupService = relationshipTypeGroupService;
    }

    @Override
    public String getDomain() {
        return DOMAIN;
    }

    @Override
    protected void initializeTools() {
        // Relationship CRUD operations (5)
        registerTool(
            "relationship_create",
            "[Relationship] Create a new relationship between contacts",
            createRelationshipSchema(),
            CATEGORY
        );

        registerTool(
            "relationship_get",
            "[Relationship] Get a relationship by ID",
            createIdSchema("Relationship ID"),
            CATEGORY
        );

        registerTool(
            "relationship_update",
            "[Relationship] Update an existing relationship",
            createRelationshipUpdateSchema(),
            CATEGORY
        );

        registerTool(
            "relationship_delete",
            "[Relationship] Delete a relationship",
            createIdSchema("Relationship ID"),
            CATEGORY
        );

        registerTool(
            "relationship_list",
            "[Relationship] List relationships with pagination",
            createListSchema(),
            CATEGORY
        );

        // Relationship type discovery operations (2)
        registerTool(
            "relationship_type_get",
            "[Discovery] Get a relationship type by ID",
            createIdSchema("Relationship Type ID"),
            CATEGORY
        );

        registerTool(
            "relationship_type_list",
            "[Discovery] List all available relationship types",
            createListSchema(),
            CATEGORY
        );

        // Relationship type group discovery operations (2)
        registerTool(
            "relationship_type_group_get",
            "[Discovery] Get a relationship type group by ID",
            createIdSchema("Relationship Type Group ID"),
            CATEGORY
        );

        registerTool(
            "relationship_type_group_list",
            "[Discovery] List all relationship type groups",
            createListSchema(),
            CATEGORY
        );
    }

    @Override
    protected Mono<Map<String, Object>> executeToolInternal(String toolName, Map<String, Object> arguments) {
        return switch (toolName) {
            // Relationship CRUD operations
            case "relationship_create" -> relationshipService.createRelationship(arguments);
            case "relationship_get" -> relationshipService.getRelationship(arguments);
            case "relationship_update" -> relationshipService.updateRelationship(arguments);
            case "relationship_delete" -> relationshipService.deleteRelationship(arguments);
            case "relationship_list" -> relationshipService.listRelationships(arguments);

            // Relationship type discovery operations
            case "relationship_type_get" -> relationshipTypeService.getRelationshipType(arguments);
            case "relationship_type_list" -> relationshipTypeService.listRelationshipTypes(arguments);

            // Relationship type group discovery operations
            case "relationship_type_group_get" -> relationshipTypeGroupService.getRelationshipTypeGroup(arguments);
            case "relationship_type_group_list" -> relationshipTypeGroupService.listRelationshipTypeGroups(arguments);

            default -> Mono.error(new UnsupportedOperationException(
                "Tool '" + toolName + "' is not implemented in " + DOMAIN + " domain registry"));
        };
    }

    // ========== Relationship Schema Methods ==========

    /**
     * Creates the schema for relationship creation.
     * Defines all fields for creating a new relationship between contacts.
     */
    private Map<String, Object> createRelationshipSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactIs", Map.of(
                    "type", "integer",
                    "description", "ID of the first contact in the relationship (required)"
                ),
                "ofContact", Map.of(
                    "type", "integer",
                    "description", "ID of the second contact in the relationship (required)"
                ),
                "relationshipTypeId", Map.of(
                    "type", "integer",
                    "description", "ID of the relationship type (required) - Use relationship_type_list to see available types"
                ),
                "notes", Map.of(
                    "type", "string",
                    "description", "Optional notes about this relationship"
                )
            ),
            "required", List.of("contactIs", "ofContact", "relationshipTypeId")
        );
    }

    /**
     * Creates the schema for relationship updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createRelationshipUpdateSchema() {
        return createUpdateSchema(createRelationshipSchema());
    }
}
