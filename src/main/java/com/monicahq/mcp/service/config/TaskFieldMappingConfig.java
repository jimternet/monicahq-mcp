package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Task entities.
 * <p>
 * Defines the field mappings, validation rules, and API endpoint for Task CRUD operations.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>contactId (client) -> contact_id (API)</li>
 *   <li>completedAt (client) -> completed_at (API)</li>
 *   <li>dueDate (client) -> due_date (API)</li>
 * </ul>
 * </p>
 * <p>
 * Required fields for creation: contactId, title
 * </p>
 * <p>
 * Default values: completed -> false
 * </p>
 * <p>
 * Available list filters: contactId, completed
 * </p>
 */
@Component
public class TaskFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/tasks";
    private static final String ENTITY_NAME = "Task";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "contactId", "contact_id",
        "completedAt", "completed_at",
        "dueDate", "due_date"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "contact_id", "contactId",
        "completed_at", "completedAt",
        "due_date", "dueDate",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Required fields for Task creation.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("contactId", "title");

    /**
     * Fields that can be used as filters in list operations.
     */
    private static final List<String> LIST_FILTER_FIELDS = List.of("contactId", "completed");

    /**
     * Query parameter mappings for list operations.
     * Maps client filter names to API query parameter names.
     */
    private static final Map<String, String> QUERY_PARAM_MAPPINGS = Map.of(
        "contactId", "contact_id",
        "completed", "completed"
    );

    /**
     * Default values applied during Task creation.
     */
    private static final Map<String, Object> CREATE_DEFAULTS = Map.of(
        "completed", false
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

    @Override
    public Map<String, Object> getCreateDefaults() {
        return CREATE_DEFAULTS;
    }
}
