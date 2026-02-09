package com.monicahq.mcp.registry;

import com.monicahq.mcp.service.ContactFieldService;
import com.monicahq.mcp.service.ContactTagService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Map;

/**
 * Domain-specific tool registry for Contact Field and Contact Tag operations.
 *
 * This registry handles all contact field and contact tag-related MCP tools:
 * - Contact Field CRUD operations (create, get, update, delete, list)
 * - Contact Tag operations (add tag to contact, remove tag from contact)
 *
 * The registry delegates execution to ContactFieldService and ContactTagService.
 */
@Component
@Slf4j
public class ContactFieldToolRegistry extends AbstractDomainToolRegistry {

    private static final String DOMAIN = "ContactField";
    private static final String CATEGORY = "Contact Management";

    private final ContactFieldService contactFieldService;
    private final ContactTagService contactTagService;

    public ContactFieldToolRegistry(ContactFieldService contactFieldService, ContactTagService contactTagService) {
        this.contactFieldService = contactFieldService;
        this.contactTagService = contactTagService;
    }

    @Override
    public String getDomain() {
        return DOMAIN;
    }

    @Override
    protected void initializeTools() {
        // Contact Field CRUD operations (5)
        registerTool(
            "contact_field_create",
            "[Contact Field] Create a new contact field. PREREQUISITES: Use contact_list to get a valid contactId, and contact_field_type_list to get a valid contactFieldTypeId before calling this tool.",
            createContactFieldSchema(),
            CATEGORY
        );

        registerTool(
            "contact_field_get",
            "[Contact Field] Get a contact field by ID",
            createIdSchema("Contact Field ID"),
            CATEGORY
        );

        registerTool(
            "contact_field_update",
            "[Contact Field] Update an existing contact field",
            createContactFieldUpdateSchema(),
            CATEGORY
        );

        registerTool(
            "contact_field_delete",
            "[Contact Field] Delete a contact field",
            createIdSchema("Contact Field ID"),
            CATEGORY
        );

        registerTool(
            "contact_field_list",
            "[Contact Field] List contact fields for a contact",
            createContactFieldListSchema(),
            CATEGORY
        );

        registerTool(
            "contactfield_list_by_contact",
            "[Contact Field] List all contact fields for a specific contact with pagination",
            createContactFieldListByContactSchema(),
            CATEGORY
        );

        // Contact Tag operations (2)
        registerTool(
            "contacttag_add",
            "[Contact Tag] Add a tag to a contact",
            createContactTagSchema(),
            CATEGORY
        );

        registerTool(
            "contacttag_remove",
            "[Contact Tag] Remove a tag from a contact",
            createContactTagSchema(),
            CATEGORY
        );
    }

    @Override
    protected Mono<Map<String, Object>> executeToolInternal(String toolName, Map<String, Object> arguments) {
        return switch (toolName) {
            case "contact_field_create" -> contactFieldService.createContactField(arguments);
            case "contact_field_get" -> contactFieldService.getContactField(arguments);
            case "contact_field_update" -> contactFieldService.updateContactField(arguments);
            case "contact_field_delete" -> contactFieldService.deleteContactField(arguments);
            case "contact_field_list" -> contactFieldService.listContactFields(arguments);
            case "contactfield_list_by_contact" -> contactFieldService.listContactFieldsByContact(arguments);
            case "contacttag_add" -> contactTagService.attachTag(arguments);
            case "contacttag_remove" -> contactTagService.detachTag(arguments);
            default -> Mono.error(new UnsupportedOperationException(
                "Tool '" + toolName + "' is not implemented in " + DOMAIN + " domain registry"));
        };
    }

    // ========== Schema Creation Methods ==========

    /**
     * Creates the schema for contact field creation.
     * Defines the required fields for creating a new contact field.
     */
    private Map<String, Object> createContactFieldSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of(
                    "type", "integer",
                    "description", "Associated contact ID - use contact_list to find valid IDs"
                ),
                "contactFieldTypeId", Map.of(
                    "type", "integer",
                    "description", "Field type ID (e.g., Email, Phone, Twitter). IMPORTANT: Use contact_field_type_list tool first to get valid IDs from your Monica instance."
                ),
                "data", Map.of(
                    "type", "string",
                    "description", "Field data (the actual value like email address, phone number, etc.)"
                )
            ),
            "required", List.of("contactId", "contactFieldTypeId", "data")
        );
    }

    /**
     * Creates the schema for contact field updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createContactFieldUpdateSchema() {
        return createUpdateSchema(createContactFieldSchema());
    }

    /**
     * Creates the schema for listing contact fields.
     * Supports pagination with a required contact ID.
     */
    private Map<String, Object> createContactFieldListSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of(
                    "type", "integer",
                    "description", "Contact ID"
                ),
                "page", Map.of(
                    "type", "integer",
                    "description", "Page number",
                    "default", 1
                ),
                "limit", Map.of(
                    "type", "integer",
                    "description", "Items per page",
                    "default", 10
                )
            ),
            "required", List.of("contactId")
        );
    }

    /**
     * Creates the schema for listing contact fields by contact (consistent naming).
     * Identical to createContactFieldListSchema() but provides consistent naming
     * with other contact-scoped list operations.
     */
    private Map<String, Object> createContactFieldListByContactSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of(
                    "type", "integer",
                    "description", "Contact ID to list fields for"
                ),
                "page", Map.of(
                    "type", "integer",
                    "description", "Page number (default: 1)",
                    "default", 1
                ),
                "limit", Map.of(
                    "type", "integer",
                    "description", "Items per page (default: 10)",
                    "default", 10
                )
            ),
            "required", List.of("contactId")
        );
    }

    /**
     * Creates the schema for contact tag operations.
     * Used by both add and remove tag operations.
     */
    private Map<String, Object> createContactTagSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "contactId", Map.of(
                    "type", "integer",
                    "description", "Contact ID"
                ),
                "tagId", Map.of(
                    "type", "integer",
                    "description", "Tag ID"
                )
            ),
            "required", List.of("contactId", "tagId")
        );
    }
}
