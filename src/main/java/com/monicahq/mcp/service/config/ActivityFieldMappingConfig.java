package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Activity entities.
 * <p>
 * Defines the field mappings, validation rules, and API endpoint for Activity CRUD operations.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>happenedAt (client) -> happened_at (API)</li>
 *   <li>activityTypeId (client) -> activity_type_id (API)</li>
 * </ul>
 * </p>
 * <p>
 * Note: Attendees array transformation is handled separately in ActivityService
 * because it requires complex nested object mapping (contactId -> contact_id within each attendee).
 * </p>
 * <p>
 * Required fields for creation: summary, attendees
 * </p>
 * <p>
 * Available list filters: contactId (mapped to 'contacts' query param)
 * </p>
 */
@Component
public class ActivityFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/activities";
    private static final String ENTITY_NAME = "Activity";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     * Note: 'attendees' and 'contactIds' both require custom transformation in ActivityService.
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.ofEntries(
        Map.entry("happenedAt", "happened_at"),
        Map.entry("activityTypeId", "activity_type_id"),
        Map.entry("attendees", "contacts"),
        Map.entry("contactIds", "contacts")
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     * Note: 'attendees' is not mapped here as it requires custom transformation.
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "happened_at", "happenedAt",
        "activity_type_id", "activityTypeId",
        "contact_id", "contactId",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Required fields for Activity creation.
     * Note: attendees has complex validation handled in ActivityService.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("summary", "attendees", "happenedAt");

    /**
     * Fields that can be used as filters in list operations.
     */
    private static final List<String> LIST_FILTER_FIELDS = List.of("contactId");

    /**
     * Query parameter mappings for list operations.
     * Maps client filter names to API query parameter names.
     * Note: contactId maps to 'contacts' (plural) in the API.
     */
    private static final Map<String, String> QUERY_PARAM_MAPPINGS = Map.of(
        "contactId", "contacts"
    );

    @Override
    public String getEndpointPath() {
        return ENDPOINT_PATH;
    }

    @Override
    public String getEntityName() {
        return ENTITY_NAME;
    }

    @Override
    public Map<String, String> getToApiMappings() {
        return TO_API_MAPPINGS;
    }

    @Override
    public Map<String, String> getFromApiMappings() {
        return FROM_API_MAPPINGS;
    }

    @Override
    public Set<String> getRequiredCreateFields() {
        return REQUIRED_CREATE_FIELDS;
    }

    @Override
    public List<String> getListFilterFields() {
        return LIST_FILTER_FIELDS;
    }

    @Override
    public Map<String, String> getQueryParamMappings() {
        return QUERY_PARAM_MAPPINGS;
    }
}
