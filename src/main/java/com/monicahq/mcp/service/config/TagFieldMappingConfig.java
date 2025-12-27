package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Tag entities.
 * <p>
 * Defines the field mappings, validation rules, and API endpoint for Tag CRUD operations.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>nameSlug (client) -> name_slug (API)</li>
 *   <li>contactCount (client) -> contact_count (API)</li>
 * </ul>
 * </p>
 * <p>
 * Required fields for creation: name (must be non-empty string)
 * </p>
 * <p>
 * Available list filters: search (mapped to query)
 * </p>
 */
@Component
public class TagFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/tags";
    private static final String ENTITY_NAME = "Tag";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "nameSlug", "name_slug",
        "contactCount", "contact_count"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "name_slug", "nameSlug",
        "contact_count", "contactCount",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Required fields for Tag creation.
     * Note: name validation requires non-empty string check, done in service.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("name");

    /**
     * Fields that can be used as filters in list operations.
     */
    private static final List<String> LIST_FILTER_FIELDS = List.of("search");

    /**
     * Query parameter mappings for list operations.
     * Maps client filter names to API query parameter names.
     */
    private static final Map<String, String> QUERY_PARAM_MAPPINGS = Map.of(
        "search", "query"
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
