package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Reminder entities.
 * <p>
 * Defines the field mappings, validation rules, and API endpoint for Reminder CRUD operations.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>contactId (client) -> contact_id (API)</li>
 *   <li>initialDate (client) -> initial_date (API)</li>
 *   <li>nextExpectedDate (client) -> next_expected_date (API)</li>
 *   <li>lastTriggered (client) -> last_triggered (API)</li>
 * </ul>
 * </p>
 * <p>
 * Required fields for creation: contactId, title, initialDate
 * </p>
 * <p>
 * Available list filters: contactId
 * </p>
 */
@Component
public class ReminderFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/reminders";
    private static final String ENTITY_NAME = "Reminder";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "contactId", "contact_id",
        "initialDate", "initial_date",
        "nextExpectedDate", "next_expected_date",
        "lastTriggered", "last_triggered"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "contact_id", "contactId",
        "initial_date", "initialDate",
        "next_expected_date", "nextExpectedDate",
        "last_triggered", "lastTriggered",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Required fields for Reminder creation.
     * Note: title requires additional non-empty string validation in the service.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("contactId", "title", "initialDate");

    /**
     * Fields that can be used as filters in list operations.
     */
    private static final List<String> LIST_FILTER_FIELDS = List.of("contactId");

    /**
     * Query parameter mappings for list operations.
     * Maps client filter names to API query parameter names.
     */
    private static final Map<String, String> QUERY_PARAM_MAPPINGS = Map.of(
        "contactId", "contact_id"
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
