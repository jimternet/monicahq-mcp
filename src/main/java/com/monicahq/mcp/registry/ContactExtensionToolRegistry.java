package com.monicahq.mcp.registry;

import com.monicahq.mcp.service.AddressService;
import com.monicahq.mcp.service.GroupService;
import com.monicahq.mcp.service.OccupationService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Domain-specific tool registry for Contact Extension operations.
 *
 * This registry handles address, group, and occupation-related MCP tools:
 * - Address CRUD operations (create, get, update, delete, list)
 * - Group CRUD operations (create, get, update, delete, list)
 * - Occupation CRUD operations (create, get, update, delete, list)
 *
 * These are extensions to the core contact functionality, providing
 * additional metadata and relationships for contacts.
 *
 * The registry delegates execution to AddressService, GroupService, and OccupationService.
 */
@Component
@Slf4j
public class ContactExtensionToolRegistry extends AbstractDomainToolRegistry {

    private static final String DOMAIN = "ContactExtension";
    private static final String CATEGORY = "Contact Management";

    private final AddressService addressService;
    private final GroupService groupService;
    private final OccupationService occupationService;

    public ContactExtensionToolRegistry(
            AddressService addressService,
            GroupService groupService,
            OccupationService occupationService) {
        this.addressService = addressService;
        this.groupService = groupService;
        this.occupationService = occupationService;
    }

    @Override
    public String getDomain() {
        return DOMAIN;
    }

    @Override
    protected void initializeTools() {
        // Address CRUD operations (5)
        registerTool(
            "address_create",
            "[Address] Create a new address for a contact",
            createAddressSchema(),
            CATEGORY
        );

        registerTool(
            "address_get",
            "[Address] Get an address by ID",
            createIdSchema("Address ID"),
            CATEGORY
        );

        registerTool(
            "address_update",
            "[Address] Update an existing address",
            createAddressUpdateSchema(),
            CATEGORY
        );

        registerTool(
            "address_delete",
            "[Address] Delete an address",
            createIdSchema("Address ID"),
            CATEGORY
        );

        registerTool(
            "address_list",
            "[Address] List addresses with pagination",
            createListSchema(),
            CATEGORY
        );

        // Group CRUD operations (5)
        registerTool(
            "group_create",
            "[Group] Create a new contact group",
            createGroupSchema(),
            CATEGORY
        );

        registerTool(
            "group_get",
            "[Group] Get a group by ID",
            createIdSchema("Group ID"),
            CATEGORY
        );

        registerTool(
            "group_update",
            "[Group] Update an existing group",
            createGroupUpdateSchema(),
            CATEGORY
        );

        registerTool(
            "group_delete",
            "[Group] Delete a group",
            createIdSchema("Group ID"),
            CATEGORY
        );

        registerTool(
            "group_list",
            "[Group] List groups with pagination",
            createListSchema(),
            CATEGORY
        );

        // Occupation CRUD operations (5)
        registerTool(
            "occupation_create",
            "[Occupation] Create a new occupation/job for a contact",
            createOccupationSchema(),
            CATEGORY
        );

        registerTool(
            "occupation_get",
            "[Occupation] Get an occupation by ID",
            createIdSchema("Occupation ID"),
            CATEGORY
        );

        registerTool(
            "occupation_update",
            "[Occupation] Update an existing occupation",
            createOccupationUpdateSchema(),
            CATEGORY
        );

        registerTool(
            "occupation_delete",
            "[Occupation] Delete an occupation",
            createIdSchema("Occupation ID"),
            CATEGORY
        );

        registerTool(
            "occupation_list",
            "[Occupation] List occupations with pagination",
            createListSchema(),
            CATEGORY
        );
    }

    @Override
    protected Mono<Map<String, Object>> executeToolInternal(String toolName, Map<String, Object> arguments) {
        return switch (toolName) {
            // Address operations
            case "address_create" -> addressService.createAddress(arguments);
            case "address_get" -> addressService.getAddress(arguments);
            case "address_update" -> addressService.updateAddress(arguments);
            case "address_delete" -> addressService.deleteAddress(arguments);
            case "address_list" -> addressService.listAddresses(arguments);

            // Group operations
            case "group_create" -> groupService.createGroup(arguments);
            case "group_get" -> groupService.getGroup(arguments);
            case "group_update" -> groupService.updateGroup(arguments);
            case "group_delete" -> groupService.deleteGroup(arguments);
            case "group_list" -> groupService.listGroups(arguments);

            // Occupation operations
            case "occupation_create" -> occupationService.createOccupation(arguments);
            case "occupation_get" -> occupationService.getOccupation(arguments);
            case "occupation_update" -> occupationService.updateOccupation(arguments);
            case "occupation_delete" -> occupationService.deleteOccupation(arguments);
            case "occupation_list" -> occupationService.listOccupations(arguments);

            default -> Mono.error(new UnsupportedOperationException(
                "Tool '" + toolName + "' is not implemented in " + DOMAIN + " domain registry"));
        };
    }

    // ========== Address Schema Methods ==========

    /**
     * Creates the schema for address creation.
     * Defines all fields for creating a new address associated with a contact.
     */
    private Map<String, Object> createAddressSchema() {
        Map<String, Object> properties = new HashMap<>();

        properties.put("contactId", Map.of(
            "type", "integer",
            "description", "Contact ID this address belongs to (required)"
        ));

        properties.put("name", Map.of(
            "type", "string",
            "description", "Address name/label (optional)",
            "maxLength", 255
        ));

        properties.put("street", Map.of(
            "type", "string",
            "description", "Street address (optional)",
            "maxLength", 255
        ));

        properties.put("city", Map.of(
            "type", "string",
            "description", "City (optional)",
            "maxLength", 255
        ));

        properties.put("province", Map.of(
            "type", "string",
            "description", "Province/State (optional)",
            "maxLength", 255
        ));

        properties.put("postalCode", Map.of(
            "type", "string",
            "description", "Postal/ZIP code (optional)",
            "maxLength", 255
        ));

        properties.put("country", Map.of(
            "type", "string",
            "description", "Country code (optional, max 3 characters)",
            "maxLength", 3
        ));

        properties.put("latitude", Map.of(
            "type", "number",
            "description", "Latitude coordinate (optional)"
        ));

        properties.put("longitude", Map.of(
            "type", "number",
            "description", "Longitude coordinate (optional)"
        ));

        return Map.of(
            "type", "object",
            "properties", properties,
            "required", List.of("contactId")
        );
    }

    /**
     * Creates the schema for address updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createAddressUpdateSchema() {
        return createUpdateSchema(createAddressSchema());
    }

    // ========== Group Schema Methods ==========

    /**
     * Creates the schema for group creation.
     * Defines fields for creating a new contact group.
     */
    private Map<String, Object> createGroupSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "name", Map.of(
                    "type", "string",
                    "description", "Group name (required)",
                    "maxLength", 255
                ),
                "description", Map.of(
                    "type", "string",
                    "description", "Group description (optional)",
                    "maxLength", 1000
                )
            ),
            "required", List.of("name")
        );
    }

    /**
     * Creates the schema for group updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createGroupUpdateSchema() {
        return createUpdateSchema(createGroupSchema());
    }

    // ========== Occupation Schema Methods ==========

    /**
     * Creates the schema for occupation creation.
     * Defines fields for creating a new occupation/job for a contact.
     */
    private Map<String, Object> createOccupationSchema() {
        Map<String, Object> properties = new HashMap<>();

        properties.put("contactId", Map.of(
            "type", "integer",
            "description", "Contact ID this occupation belongs to (required)"
        ));

        properties.put("companyId", Map.of(
            "type", "integer",
            "description", "Company ID where the person works (optional)"
        ));

        properties.put("title", Map.of(
            "type", "string",
            "description", "Job title (required)",
            "maxLength", 255
        ));

        properties.put("description", Map.of(
            "type", "string",
            "description", "Job description (optional)",
            "maxLength", 1000
        ));

        properties.put("salary", Map.of(
            "type", "string",
            "description", "Salary amount (optional)"
        ));

        properties.put("salaryUnit", Map.of(
            "type", "string",
            "description", "Salary unit (e.g., 'per year', 'per month', 'per hour') (optional)"
        ));

        properties.put("currentlyWorksHere", Map.of(
            "type", "boolean",
            "description", "Whether the person currently works at this job (optional, default: true)",
            "default", true
        ));

        properties.put("startDate", Map.of(
            "type", "string",
            "format", "date",
            "description", "Start date in YYYY-MM-DD format (optional)"
        ));

        properties.put("endDate", Map.of(
            "type", "string",
            "format", "date",
            "description", "End date in YYYY-MM-DD format (optional, only if currentlyWorksHere=false)"
        ));

        return Map.of(
            "type", "object",
            "properties", properties,
            "required", List.of("contactId", "title")
        );
    }

    /**
     * Creates the schema for occupation updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createOccupationUpdateSchema() {
        return createUpdateSchema(createOccupationSchema());
    }
}
