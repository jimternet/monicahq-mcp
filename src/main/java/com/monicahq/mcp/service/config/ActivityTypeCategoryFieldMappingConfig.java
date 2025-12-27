package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Activity Type Category entities.
 * <p>
 * Defines the field mappings and API endpoint for Activity Type Category operations.
 * Activity Type Categories group related Activity Types together
 * (e.g., "Work", "Social", "Family").
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>parentId (client) -> parent_id (API)</li>
 *   <li>sortOrder (client) -> sort_order (API)</li>
 * </ul>
 * </p>
 * <p>
 * Supported operations: create, get, update, delete, list.
 * Required create fields: name.
 * </p>
 */
@Component
public class ActivityTypeCategoryFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/activitytypecategories";
    private static final String ENTITY_NAME = "Activity Type Category";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "parentId", "parent_id",
        "sortOrder", "sort_order"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "parent_id", "parentId",
        "sort_order", "sortOrder",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Required fields for creating an Activity Type Category.
     * name must be a non-empty string.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("name");

    /**
     * Required fields for updating an Activity Type Category.
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
