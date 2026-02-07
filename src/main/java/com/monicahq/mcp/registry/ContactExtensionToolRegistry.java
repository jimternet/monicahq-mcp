package com.monicahq.mcp.registry;

import com.monicahq.mcp.service.AddressService;
import com.monicahq.mcp.service.GroupService;
import com.monicahq.mcp.service.LifeEventService;
import com.monicahq.mcp.service.OccupationService;
import com.monicahq.mcp.service.PlaceService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Domain-specific tool registry for Contact Extension operations.
 *
 * This registry handles address, group, occupation, place, and life event-related MCP tools:
 * - Address CRUD operations (create, get, update, delete, list)
 * - Group CRUD operations (create, get, update, delete, list)
 * - Occupation CRUD operations (create, get, update, delete, list)
 * - Place CRUD operations (create, get, update, delete, list) - Gap Analysis Phase 1
 * - LifeEvent CRUD operations (create, get, update, delete) - Gap Analysis Phase 1
 *
 * These are extensions to the core contact functionality, providing
 * additional metadata, locations, milestones, and relationships for contacts.
 *
 * The registry delegates execution to AddressService, GroupService, OccupationService,
 * PlaceService, and LifeEventService.
 */
@Component
@Slf4j
public class ContactExtensionToolRegistry extends AbstractDomainToolRegistry {

    private static final String DOMAIN = "ContactExtension";
    private static final String CATEGORY = "Contact Management";

    private final AddressService addressService;
    private final GroupService groupService;
    private final OccupationService occupationService;
    private final PlaceService placeService;
    private final LifeEventService lifeEventService;

    public ContactExtensionToolRegistry(
            AddressService addressService,
            GroupService groupService,
            OccupationService occupationService,
            PlaceService placeService,
            LifeEventService lifeEventService) {
        this.addressService = addressService;
        this.groupService = groupService;
        this.occupationService = occupationService;
        this.placeService = placeService;
        this.lifeEventService = lifeEventService;
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

        registerTool(
            "address_list_by_contact",
            "[Address] List all addresses for a specific contact",
            createContactScopedListSchema("Contact ID to retrieve addresses for"),
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

        registerTool(
            "occupation_list_by_contact",
            "[Occupation] List all occupations for a specific contact",
            createContactScopedListSchema("Contact ID to retrieve occupations for"),
            CATEGORY
        );

        // Place CRUD operations (5) - Gap Analysis Phase 1
        registerTool(
            "place_create",
            "[Place] Create a new geographic place/location",
            createPlaceSchema(),
            CATEGORY
        );

        registerTool(
            "place_get",
            "[Place] Get a place by ID",
            createIdSchema("Place ID"),
            CATEGORY
        );

        registerTool(
            "place_update",
            "[Place] Update an existing place",
            createPlaceUpdateSchema(),
            CATEGORY
        );

        registerTool(
            "place_delete",
            "[Place] Delete a place",
            createIdSchema("Place ID"),
            CATEGORY
        );

        registerTool(
            "place_list",
            "[Place] List places with pagination",
            createListSchema(),
            CATEGORY
        );

        // LifeEvent CRUD operations (4) - Gap Analysis Phase 1
        registerTool(
            "lifeevent_create",
            "[LifeEvent] Create a new life event for a contact",
            createLifeEventSchema(),
            CATEGORY
        );

        registerTool(
            "lifeevent_get",
            "[LifeEvent] Get a life event by ID",
            createIdSchema("LifeEvent ID"),
            CATEGORY
        );

        registerTool(
            "lifeevent_update",
            "[LifeEvent] Update an existing life event",
            createLifeEventUpdateSchema(),
            CATEGORY
        );

        registerTool(
            "lifeevent_delete",
            "[LifeEvent] Delete a life event",
            createIdSchema("LifeEvent ID"),
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
            case "address_list_by_contact" -> addressService.listAddressesByContact(arguments);

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
            case "occupation_list_by_contact" -> occupationService.listOccupationsByContact(arguments);

            // Place operations (Gap Analysis Phase 1)
            case "place_create" -> placeService.createPlace(arguments);
            case "place_get" -> placeService.getPlace(arguments);
            case "place_update" -> placeService.updatePlace(arguments);
            case "place_delete" -> placeService.deletePlace(arguments);
            case "place_list" -> placeService.listPlaces(arguments);

            // LifeEvent operations (Gap Analysis Phase 1)
            case "lifeevent_create" -> lifeEventService.createLifeEvent(arguments);
            case "lifeevent_get" -> lifeEventService.getLifeEvent(arguments);
            case "lifeevent_update" -> lifeEventService.updateLifeEvent(arguments);
            case "lifeevent_delete" -> lifeEventService.deleteLifeEvent(arguments);

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

    // ========== Place Schema Methods (Gap Analysis Phase 1) ==========

    /**
     * Creates the schema for place creation.
     * Defines fields for creating a new geographic place/location.
     * All fields are optional - can create minimal places or detailed ones.
     */
    private Map<String, Object> createPlaceSchema() {
        Map<String, Object> properties = new HashMap<>();

        properties.put("street", Map.of(
            "type", "string",
            "description", "Street address (optional)",
            "maxLength", 255
        ));

        properties.put("city", Map.of(
            "type", "string",
            "description", "City name (optional)",
            "maxLength", 255
        ));

        properties.put("province", Map.of(
            "type", "string",
            "description", "Province or state (optional)",
            "maxLength", 255
        ));

        properties.put("postalCode", Map.of(
            "type", "string",
            "description", "Postal or ZIP code (optional)",
            "maxLength", 20
        ));

        properties.put("country", Map.of(
            "type", "string",
            "description", "Country name (optional)",
            "maxLength", 255
        ));

        properties.put("latitude", Map.of(
            "type", "number",
            "format", "float",
            "description", "Latitude coordinate for geocoding (optional)",
            "minimum", -90,
            "maximum", 90
        ));

        properties.put("longitude", Map.of(
            "type", "number",
            "format", "float",
            "description", "Longitude coordinate for geocoding (optional)",
            "minimum", -180,
            "maximum", 180
        ));

        return Map.of(
            "type", "object",
            "properties", properties,
            "required", List.of()  // All fields optional
        );
    }

    /**
     * Creates the schema for place updates.
     * Allows updating any place field.
     */
    private Map<String, Object> createPlaceUpdateSchema() {
        Map<String, Object> createSchema = createPlaceSchema();
        Map<String, Object> properties = new HashMap<>((Map<String, Object>) createSchema.get("properties"));

        properties.put("id", Map.of(
            "type", "integer",
            "description", "Place ID (required)"
        ));

        return Map.of(
            "type", "object",
            "properties", properties,
            "required", List.of("id")
        );
    }

    // ========== LifeEvent Schema Methods (Gap Analysis Phase 1) ==========

    /**
     * Creates the schema for life event creation.
     * Defines fields for creating a significant life event for a contact.
     */
    private Map<String, Object> createLifeEventSchema() {
        Map<String, Object> properties = new HashMap<>();

        properties.put("contactId", Map.of(
            "type", "integer",
            "description", "Contact ID this life event belongs to (required)"
        ));

        properties.put("lifeEventTypeId", Map.of(
            "type", "integer",
            "description", "Life event type ID (required) - e.g., birth, graduation, marriage, etc. Use lifeeventtype_list to see available types."
        ));

        properties.put("name", Map.of(
            "type", "string",
            "description", "Name/title of the life event (required)",
            "maxLength", 255
        ));

        properties.put("note", Map.of(
            "type", "string",
            "description", "Additional notes or details about the event (optional)",
            "maxLength", 1000
        ));

        properties.put("happenedAt", Map.of(
            "type", "string",
            "format", "date",
            "description", "Date when the event occurred in YYYY-MM-DD format (required)"
        ));

        properties.put("happenedAtMonthUnknown", Map.of(
            "type", "boolean",
            "description", "Set to true if the month is unknown (optional, default: false)",
            "default", false
        ));

        properties.put("happenedAtDayUnknown", Map.of(
            "type", "boolean",
            "description", "Set to true if the day is unknown (optional, default: false)",
            "default", false
        ));

        properties.put("reminderId", Map.of(
            "type", "integer",
            "description", "Associated reminder ID (optional)"
        ));

        return Map.of(
            "type", "object",
            "properties", properties,
            "required", List.of("contactId", "lifeEventTypeId", "name", "happenedAt")
        );
    }

    /**
     * Creates the schema for life event updates.
     * Allows updating any life event field.
     */
    private Map<String, Object> createLifeEventUpdateSchema() {
        Map<String, Object> createSchema = createLifeEventSchema();
        Map<String, Object> properties = new HashMap<>((Map<String, Object>) createSchema.get("properties"));

        properties.put("id", Map.of(
            "type", "integer",
            "description", "LifeEvent ID (required)"
        ));

        return Map.of(
            "type", "object",
            "properties", properties,
            "required", List.of("id")
        );
    }

    // ========== Contact-Scoped List Schema ==========

    /**
     * Creates the schema for contact-scoped list operations.
     * Includes contactId as required parameter with optional pagination.
     *
     * @param contactIdDescription description for the contactId parameter
     * @return schema Map with contactId and pagination properties
     */
    private Map<String, Object> createContactScopedListSchema(String contactIdDescription) {
        Map<String, Object> properties = new HashMap<>();
        properties.put("contactId", Map.of(
            "type", "integer",
            "description", contactIdDescription
        ));
        properties.put("page", Map.of(
            "type", "integer",
            "description", "Page number (starting from 1)",
            "default", 1
        ));
        properties.put("limit", Map.of(
            "type", "integer",
            "description", "Number of items per page",
            "default", 10,
            "maximum", 100
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("contactId"));

        return schema;
    }
}
