package com.monicahq.mcp.registry;

import com.monicahq.mcp.service.ActivityService;
import com.monicahq.mcp.service.ActivityTypeCategoryService;
import com.monicahq.mcp.service.ActivityTypeService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Domain-specific tool registry for Activity operations.
 *
 * This registry handles activity-related MCP tools:
 * - Activity CRUD operations (create, get, update, delete, list)
 * - Activity Type CRUD operations (create, get, update, delete, list)
 * - Activity Type Category CRUD operations (create, get, update, delete, list)
 *
 * Activities represent interactions or events involving contacts.
 * Activity Types categorize activities (e.g., "Coffee", "Meeting").
 * Activity Type Categories group activity types hierarchically.
 *
 * The registry delegates execution to:
 * - ActivityService for activity operations
 * - ActivityTypeService for activity type operations
 * - ActivityTypeCategoryService for activity type category operations
 */
@Component
@Slf4j
public class ActivityToolRegistry extends AbstractDomainToolRegistry {

    private static final String DOMAIN = "Activity";
    private static final String CATEGORY_ACTIVITY = "Activity & Communication";
    private static final String CATEGORY_ACTIVITY_MANAGEMENT = "Activity Management";

    private final ActivityService activityService;
    private final ActivityTypeService activityTypeService;
    private final ActivityTypeCategoryService activityTypeCategoryService;

    public ActivityToolRegistry(
            ActivityService activityService,
            ActivityTypeService activityTypeService,
            ActivityTypeCategoryService activityTypeCategoryService) {
        this.activityService = activityService;
        this.activityTypeService = activityTypeService;
        this.activityTypeCategoryService = activityTypeCategoryService;
    }

    @Override
    public String getDomain() {
        return DOMAIN;
    }

    @Override
    protected void initializeTools() {
        // Activity CRUD operations (5)
        registerTool(
            "activity_create",
            "[Activity] Create a new activity",
            createActivitySchema(),
            CATEGORY_ACTIVITY
        );

        registerTool(
            "activity_get",
            "[Activity] Get an activity by ID",
            createIdSchema("Activity ID"),
            CATEGORY_ACTIVITY
        );

        registerTool(
            "activity_update",
            "[Activity] Update an existing activity",
            createActivityUpdateSchema(),
            CATEGORY_ACTIVITY
        );

        registerTool(
            "activity_delete",
            "[Activity] Delete an activity",
            createIdSchema("Activity ID"),
            CATEGORY_ACTIVITY
        );

        registerTool(
            "activity_list",
            "[Activity] List activities with pagination",
            createListSchema(),
            CATEGORY_ACTIVITY
        );

        registerTool(
            "activity_list_by_contact",
            "[Activity] List all activities for a specific contact",
            createContactScopedListSchema("Contact ID to retrieve activities for"),
            CATEGORY_ACTIVITY
        );

        // Activity Type CRUD operations (5)
        registerTool(
            "activity_type_create",
            "[Activity Type] Create a new activity type",
            createActivityTypeSchema(),
            CATEGORY_ACTIVITY_MANAGEMENT
        );

        registerTool(
            "activity_type_get",
            "[Activity Type] Get activity type by ID",
            createIdSchema("Activity Type ID"),
            CATEGORY_ACTIVITY_MANAGEMENT
        );

        registerTool(
            "activity_type_update",
            "[Activity Type] Update existing activity type",
            createActivityTypeUpdateSchema(),
            CATEGORY_ACTIVITY_MANAGEMENT
        );

        registerTool(
            "activity_type_delete",
            "[Activity Type] Delete activity type",
            createIdSchema("Activity Type ID"),
            CATEGORY_ACTIVITY_MANAGEMENT
        );

        registerTool(
            "activity_type_list",
            "[Activity Type] List activity types with pagination",
            createListSchema(),
            CATEGORY_ACTIVITY_MANAGEMENT
        );

        // Activity Type Category CRUD operations (5)
        registerTool(
            "activity_type_category_create",
            "[Activity Category] Create a new activity type category",
            createActivityTypeCategorySchema(),
            CATEGORY_ACTIVITY_MANAGEMENT
        );

        registerTool(
            "activity_type_category_get",
            "[Activity Category] Get activity type category by ID",
            createIdSchema("Activity Type Category ID"),
            CATEGORY_ACTIVITY_MANAGEMENT
        );

        registerTool(
            "activity_type_category_update",
            "[Activity Category] Update existing activity type category",
            createActivityTypeCategoryUpdateSchema(),
            CATEGORY_ACTIVITY_MANAGEMENT
        );

        registerTool(
            "activity_type_category_delete",
            "[Activity Category] Delete activity type category",
            createIdSchema("Activity Type Category ID"),
            CATEGORY_ACTIVITY_MANAGEMENT
        );

        registerTool(
            "activity_type_category_list",
            "[Activity Category] List activity type categories with pagination",
            createListSchema(),
            CATEGORY_ACTIVITY_MANAGEMENT
        );
    }

    @Override
    protected Mono<Map<String, Object>> executeToolInternal(String toolName, Map<String, Object> arguments) {
        return switch (toolName) {
            // Activity operations
            case "activity_create" -> activityService.createActivity(arguments);
            case "activity_get" -> activityService.getActivity(arguments);
            case "activity_update" -> activityService.updateActivity(arguments);
            case "activity_delete" -> activityService.deleteActivity(arguments);
            case "activity_list" -> activityService.listActivities(arguments);
            case "activity_list_by_contact" -> activityService.listActivitiesByContact(arguments);

            // Activity type operations
            case "activity_type_create" -> activityTypeService.createActivityType(arguments);
            case "activity_type_get" -> activityTypeService.getActivityType(arguments);
            case "activity_type_update" -> activityTypeService.updateActivityType(arguments);
            case "activity_type_delete" -> activityTypeService.deleteActivityType(arguments);
            case "activity_type_list" -> activityTypeService.listActivityTypes(arguments);

            // Activity type category operations
            case "activity_type_category_create" -> activityTypeCategoryService.createActivityTypeCategory(arguments);
            case "activity_type_category_get" -> activityTypeCategoryService.getActivityTypeCategory(arguments);
            case "activity_type_category_update" -> activityTypeCategoryService.updateActivityTypeCategory(arguments);
            case "activity_type_category_delete" -> activityTypeCategoryService.deleteActivityTypeCategory(arguments);
            case "activity_type_category_list" -> activityTypeCategoryService.listActivityTypeCategories(arguments);

            default -> Mono.error(new UnsupportedOperationException(
                "Tool '" + toolName + "' is not implemented in " + DOMAIN + " domain registry"));
        };
    }

    // ========== Activity Schema Methods ==========

    /**
     * Creates the schema for activity creation.
     * Activities track interactions with contacts like meetings, calls, etc.
     */
    private Map<String, Object> createActivitySchema() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("contactId", Map.of(
            "type", "integer",
            "description", "ID of the contact associated with this activity (optional)"
        ));
        properties.put("activityTypeId", Map.of(
            "type", "integer",
            "description", "Activity type ID from Monica (optional). Common types: 1=Simple Activity"
        ));
        properties.put("summary", Map.of(
            "type", "string",
            "description", "Brief summary of the activity (required)",
            "maxLength", 255
        ));
        properties.put("description", Map.of(
            "type", "string",
            "description", "Detailed description of the activity (optional)"
        ));
        properties.put("happenedAt", Map.of(
            "type", "string",
            "format", "date-time",
            "description", "Date when activity happened in ISO 8601 format (required, e.g., '2025-01-15T10:00:00Z')"
        ));
        properties.put("attendees", Map.of(
            "type", "array",
            "description", "List of contact IDs for activity attendees (required). Use contact IDs as integers: [91, 73] or objects with contactId: [{'contactId': 91}]",
            "items", Map.of(
                "oneOf", List.of(
                    Map.of(
                        "type", "integer",
                        "description", "Contact ID as an integer"
                    ),
                    Map.of(
                        "type", "object",
                        "properties", Map.of(
                            "contactId", Map.of(
                                "type", "integer",
                                "description", "Contact ID"
                            )
                        ),
                        "required", List.of("contactId")
                    )
                )
            )
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("summary", "attendees", "happenedAt"));

        return schema;
    }

    /**
     * Creates the schema for activity updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createActivityUpdateSchema() {
        return createUpdateSchema(createActivitySchema());
    }

    // ========== Activity Type Schema Methods ==========

    /**
     * Creates the schema for activity type creation.
     * Activity types categorize activities (e.g., "Coffee", "Meeting", "Lunch").
     */
    private Map<String, Object> createActivityTypeSchema() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("name", Map.of(
            "type", "string",
            "description", "Activity type name (required)",
            "maxLength", 255
        ));
        properties.put("categoryId", Map.of(
            "type", "integer",
            "description", "Activity type category ID (optional)"
        ));
        properties.put("description", Map.of(
            "type", "string",
            "description", "Activity type description (optional)",
            "maxLength", 1000
        ));
        properties.put("icon", Map.of(
            "type", "string",
            "description", "Activity type icon (optional)"
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("name"));

        return schema;
    }

    /**
     * Creates the schema for activity type updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createActivityTypeUpdateSchema() {
        return createUpdateSchema(createActivityTypeSchema());
    }

    // ========== Activity Type Category Schema Methods ==========

    /**
     * Creates the schema for activity type category creation.
     * Categories group activity types hierarchically (e.g., "Food & Drink" contains "Coffee", "Lunch").
     */
    private Map<String, Object> createActivityTypeCategorySchema() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("name", Map.of(
            "type", "string",
            "description", "Activity type category name (required)",
            "maxLength", 255
        ));
        properties.put("parentId", Map.of(
            "type", "integer",
            "description", "Parent category ID for hierarchical structure (optional)"
        ));
        properties.put("description", Map.of(
            "type", "string",
            "description", "Category description (optional)",
            "maxLength", 1000
        ));
        properties.put("sortOrder", Map.of(
            "type", "integer",
            "description", "Sort order for display (optional)"
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("name"));

        return schema;
    }

    /**
     * Creates the schema for activity type category updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createActivityTypeCategoryUpdateSchema() {
        return createUpdateSchema(createActivityTypeCategorySchema());
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
