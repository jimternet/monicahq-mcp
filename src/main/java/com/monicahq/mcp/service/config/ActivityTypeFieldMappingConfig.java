package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Activity Type entities.
 * <p>
 * Defines the field mappings and API endpoint for Activity Type operations.
 * Activity Types define categories of activities that can be logged with contacts
 * (e.g., "Phone call", "Dinner", "Meeting").
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>categoryId (client) -> category_id (API)</li>
 * </ul>
 * </p>
 * <p>
 * Supported operations: create, get, update, delete, list.
 * Required create fields: name.
 * </p>
 */
@Component
public class ActivityTypeFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/activitytypes";
    private static final String ENTITY_NAME = "Activity Type";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "categoryId", "category_id"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "category_id", "categoryId",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Required fields for creating an Activity Type.
     * name must be a non-empty string.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("name");

    /**
     * Required fields for updating an Activity Type.
     * name must be a non-empty string.
     */
    private static final Set<String> REQUIRED_UPDATE_FIELDS = Set.of("name");

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
    public Set<String> getRequiredUpdateFields() {
        return REQUIRED_UPDATE_FIELDS;
    }

    @Override
    public List<String> getListFilterFields() {
        return Collections.emptyList();
    }
}
