package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Country entities.
 * <p>
 * Defines the field mappings and API endpoint for Country operations.
 * Country entities are read-only lookup values used for addresses.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>countryCode (client) -> country_code (API)</li>
 * </ul>
 * </p>
 * <p>
 * This is a read-only entity - only get, list, and search operations are supported.
 * </p>
 */
@Component
public class CountryFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/countries";
    private static final String ENTITY_NAME = "Country";

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "country_code", "countryCode",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * List filter fields - search can be used to filter countries.
     */
    private static final List<String> LIST_FILTER_FIELDS = List.of("search");

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
        return Collections.emptyMap();
    }

    @Override
    public Map<String, String> getFromApiMappings() {
        return FROM_API_MAPPINGS;
    }

    @Override
    public Set<String> getRequiredCreateFields() {
        return Collections.emptySet();
    }

    @Override
    public List<String> getListFilterFields() {
        return LIST_FILTER_FIELDS;
    }

    /**
     * Country entities are read-only lookup values.
     * @return false - create is not supported
     */
    @Override
    public boolean supportsCreate() {
        return false;
    }

    /**
     * Country entities are read-only lookup values.
     * @return false - update is not supported
     */
    @Override
    public boolean supportsUpdate() {
        return false;
    }

    /**
     * Country entities are read-only lookup values.
     * @return false - delete is not supported
     */
    @Override
    public boolean supportsDelete() {
        return false;
    }
}
