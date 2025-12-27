package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Gender entities.
 * <p>
 * Defines the field mappings and API endpoint for Gender operations.
 * Gender entities are read-only lookup values used when creating contacts.
 * </p>
 * <p>
 * Implements Constitutional Principle VII: API Discovery and Completeness.
 * This discovery tool eliminates the need to hardcode gender values.
 * </p>
 * <p>
 * This is a read-only entity - only list operation is supported.
 * </p>
 */
@Component
public class GenderFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/genders";
    private static final String ENTITY_NAME = "Gender";

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "created_at", "createdAt",
        "updated_at", "updatedAt"
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
        return Collections.emptyList();
    }

    /**
     * Gender entities are read-only lookup values.
     * @return false - create is not supported
     */
    @Override
    public boolean supportsCreate() {
        return false;
    }

    /**
     * Gender entities are read-only lookup values.
     * @return false - update is not supported
     */
    @Override
    public boolean supportsUpdate() {
        return false;
    }

    /**
     * Gender entities are read-only lookup values.
     * @return false - delete is not supported
     */
    @Override
    public boolean supportsDelete() {
        return false;
    }
}
