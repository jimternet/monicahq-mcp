package com.monicahq.mcp.registry;

import com.monicahq.mcp.service.ContactService;
import com.monicahq.mcp.service.TagService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Domain-specific tool registry for Contact operations.
 *
 * This registry handles all contact-related MCP tools:
 * - Core CRUD operations (create, get, update, delete, list)
 * - Search functionality
 * - Career/work information updates
 * - Audit log retrieval
 * - Tag-based contact filtering
 *
 * The registry delegates execution to ContactService and TagService.
 */
@Component
@Slf4j
public class ContactToolRegistry extends AbstractDomainToolRegistry {

    private static final String DOMAIN = "Contact";
    private static final String CATEGORY = "Contact Management";

    private final ContactService contactService;
    private final TagService tagService;

    public ContactToolRegistry(ContactService contactService, TagService tagService) {
        this.contactService = contactService;
        this.tagService = tagService;
    }

    @Override
    public String getDomain() {
        return DOMAIN;
    }

    @Override
    protected void initializeTools() {
        // Core contact CRUD operations (5)
        registerTool(
            "contact_create",
            "[Contact] Create a new contact in MonicaHQ",
            createContactSchema(),
            CATEGORY
        );

        registerTool(
            "contact_get",
            "[Contact] Get a contact by ID",
            createIdSchema("Contact ID"),
            CATEGORY
        );

        registerTool(
            "contact_update",
            "[Contact] Update an existing contact",
            createContactUpdateSchema(),
            CATEGORY
        );

        registerTool(
            "contact_delete",
            "[Contact] Delete a contact",
            createIdSchema("Contact ID"),
            CATEGORY
        );

        registerTool(
            "contact_list",
            "[Contact] List contacts with pagination",
            createListSchema(),
            CATEGORY
        );

        // Contact gap fix operations (4) - API Gap Implementation
        registerTool(
            "contact_search",
            "[Contact] Search contacts by query",
            createContactSearchSchema(),
            CATEGORY
        );

        registerTool(
            "contact_career_update",
            "[Contact] Update contact career/work information",
            createContactCareerSchema(),
            CATEGORY
        );

        registerTool(
            "contact_audit_logs",
            "[Contact] Get contact audit logs/history",
            createContactAuditLogSchema(),
            CATEGORY
        );

        registerTool(
            "contacts_by_tag",
            "[Contact] List contacts filtered by tag",
            createContactsByTagSchema(),
            CATEGORY
        );
    }

    @Override
    protected Mono<Map<String, Object>> executeToolInternal(String toolName, Map<String, Object> arguments) {
        return switch (toolName) {
            case "contact_create" -> contactService.createContact(arguments);
            case "contact_get" -> contactService.getContact(arguments);
            case "contact_update" -> contactService.updateContact(arguments);
            case "contact_delete" -> contactService.deleteContact(arguments);
            case "contact_list" -> contactService.listContacts(arguments);
            case "contact_search" -> contactService.searchContacts(arguments);
            case "contact_career_update" -> contactService.updateContactCareer(arguments);
            case "contact_audit_logs" -> contactService.getContactAuditLogs(arguments);
            case "contacts_by_tag" -> tagService.listContactsByTag(arguments);
            default -> Mono.error(new UnsupportedOperationException(
                "Tool '" + toolName + "' is not implemented in " + DOMAIN + " domain registry"));
        };
    }

    // ========== Schema Creation Methods ==========

    /**
     * Creates the schema for contact creation.
     * Defines all fields required and optional for creating a new contact.
     */
    private Map<String, Object> createContactSchema() {
        Map<String, Object> properties = new HashMap<>();

        properties.put("firstName", Map.of(
            "type", "string",
            "description", "Contact's first name (required)",
            "maxLength", 255
        ));

        properties.put("lastName", Map.of(
            "type", "string",
            "description", "Contact's last name (optional)",
            "maxLength", 255
        ));

        properties.put("genderId", Map.of(
            "type", "string",
            "description", "Gender ID (required) - Use gender_list tool to see available options. Note: Monica supports custom genders per account."
        ));

        properties.put("nickname", Map.of(
            "type", "string",
            "description", "Contact's nickname (optional)",
            "maxLength", 255
        ));

        properties.put("isBirthdateKnown", Map.of(
            "type", "boolean",
            "description", "Set to true if you know the birthdate (required, default: false)",
            "default", false
        ));

        properties.put("birthdate", Map.of(
            "type", "string",
            "format", "date",
            "description", "Birthdate in YYYY-MM-DD format (optional, only if isBirthdateKnown=true)"
        ));

        properties.put("isDeceased", Map.of(
            "type", "boolean",
            "description", "Set to true if person is deceased (required, default: false)",
            "default", false
        ));

        properties.put("isDeceasedDateKnown", Map.of(
            "type", "boolean",
            "description", "Set to true if you know the death date (required, default: false)",
            "default", false
        ));

        properties.put("deceasedDate", Map.of(
            "type", "string",
            "format", "date",
            "description", "Death date in YYYY-MM-DD format (optional, only if isDeceasedDateKnown=true)"
        ));

        properties.put("description", Map.of(
            "type", "string",
            "description", "Notes or description about the contact (optional)"
        ));

        return Map.of(
            "type", "object",
            "properties", properties,
            "required", List.of("firstName", "genderId", "isBirthdateKnown", "isDeceased", "isDeceasedDateKnown")
        );
    }

    /**
     * Creates the schema for contact updates.
     * Allows updating any contact field while requiring only the ID.
     */
    private Map<String, Object> createContactUpdateSchema() {
        Map<String, Object> properties = new HashMap<>();

        // Required ID field
        properties.put("id", Map.of(
            "type", "integer",
            "description", "Contact ID (required)"
        ));

        // Fields that can be updated
        properties.put("firstName", Map.of(
            "type", "string",
            "description", "Contact's first name (optional - if not provided, existing value will be kept)",
            "maxLength", 255
        ));

        properties.put("lastName", Map.of(
            "type", "string",
            "description", "Contact's last name (optional)",
            "maxLength", 255
        ));

        properties.put("genderId", Map.of(
            "type", "string",
            "description", "Gender ID (optional) - Use gender_list tool to see available options"
        ));

        properties.put("nickname", Map.of(
            "type", "string",
            "description", "Contact's nickname (optional)",
            "maxLength", 255
        ));

        properties.put("email", Map.of(
            "type", "string",
            "format", "email",
            "description", "Primary email address (optional)"
        ));

        properties.put("phone", Map.of(
            "type", "string",
            "description", "Primary phone number (optional)"
        ));

        properties.put("company", Map.of(
            "type", "string",
            "description", "Company or organization (optional)",
            "maxLength", 255
        ));

        properties.put("jobTitle", Map.of(
            "type", "string",
            "description", "Job title or position (optional)",
            "maxLength", 255
        ));

        properties.put("birthdate", Map.of(
            "type", "string",
            "format", "date",
            "description", "Birthdate in YYYY-MM-DD format (optional). When provided, isBirthdateKnown will automatically be set to true."
        ));

        properties.put("isBirthdateKnown", Map.of(
            "type", "boolean",
            "description", "Whether the birthdate is known (optional - defaults to existing value, automatically set to true when birthdate is provided)"
        ));

        properties.put("isDeceased", Map.of(
            "type", "boolean",
            "description", "Whether the contact is deceased (optional - defaults to existing value)"
        ));

        properties.put("isDeceasedDateKnown", Map.of(
            "type", "boolean",
            "description", "Whether the deceased date is known (optional - defaults to existing value)"
        ));

        return Map.of(
            "type", "object",
            "properties", properties,
            "required", List.of("id"),
            "additionalProperties", false,
            "description", "Update a contact. NOTE: MonicaHQ API will automatically fetch and preserve existing values for all fields not provided in the update."
        );
    }

    /**
     * Creates the schema for contact search.
     * Supports query-based searching with pagination.
     */
    private Map<String, Object> createContactSearchSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "query", Map.of(
                    "type", "string",
                    "description", "Search query for contacts (name, email, etc.)"
                ),
                "page", Map.of(
                    "type", "integer",
                    "description", "Page number (default: 1)",
                    "minimum", 1
                ),
                "limit", Map.of(
                    "type", "integer",
                    "description", "Number of results per page (default: 10, max: 100)",
                    "minimum", 1,
                    "maximum", 100
                )
            ),
            "required", List.of()
        );
    }

    /**
     * Creates the schema for career/work information updates.
     * Allows updating job-related fields for a contact.
     */
    private Map<String, Object> createContactCareerSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "id", Map.of(
                    "type", "integer",
                    "description", "Contact ID (required)"
                ),
                "jobTitle", Map.of(
                    "type", "string",
                    "description", "Job title"
                ),
                "company", Map.of(
                    "type", "string",
                    "description", "Company name"
                ),
                "startDate", Map.of(
                    "type", "string",
                    "format", "date",
                    "description", "Start date in YYYY-MM-DD format"
                ),
                "endDate", Map.of(
                    "type", "string",
                    "format", "date",
                    "description", "End date in YYYY-MM-DD format (optional)"
                ),
                "salary", Map.of(
                    "type", "string",
                    "description", "Salary information"
                )
            ),
            "required", List.of("id")
        );
    }

    /**
     * Creates the schema for contact audit log retrieval.
     * Supports pagination for viewing contact history.
     */
    private Map<String, Object> createContactAuditLogSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "id", Map.of(
                    "type", "integer",
                    "description", "Contact ID (required)"
                ),
                "page", Map.of(
                    "type", "integer",
                    "description", "Page number (default: 1)",
                    "minimum", 1
                ),
                "limit", Map.of(
                    "type", "integer",
                    "description", "Number of results per page (default: 20, max: 100)",
                    "minimum", 1,
                    "maximum", 100
                )
            ),
            "required", List.of("id")
        );
    }

    /**
     * Creates the schema for listing contacts by tag.
     * Filters contacts based on tag association.
     */
    private Map<String, Object> createContactsByTagSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "tagId", Map.of(
                    "type", "integer",
                    "description", "Tag ID (required)"
                ),
                "page", Map.of(
                    "type", "integer",
                    "description", "Page number (default: 1)",
                    "minimum", 1
                ),
                "limit", Map.of(
                    "type", "integer",
                    "description", "Number of results per page (default: 10, max: 100)",
                    "minimum", 1,
                    "maximum", 100
                )
            ),
            "required", List.of("tagId")
        );
    }
}
