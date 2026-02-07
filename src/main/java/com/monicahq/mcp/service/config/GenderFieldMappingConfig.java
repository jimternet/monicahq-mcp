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
 * Gender entities support full CRUD operations for managing custom gender types.
 * </p>
 * <p>
 * Implements Constitutional Principle VII: API Discovery and Completeness.
 * Enables users to create and manage custom gender definitions per account.
 * </p>
 * <p>
 * Gap Analysis Phase 1: Enabled CRUD operations (previously read-only).
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

    /**
     * Required fields for creating a new gender.
     * Only 'name' is required - the gender display name.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("name");

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
        return REQUIRED_CREATE_FIELDS;
    }

    @Override
    public List<String> getListFilterFields() {
        return Collections.emptyList();
    }

    /**
     * Gender entities support create operations.
     * Allows creation of custom gender types per account.
     * @return true - create is supported
     */
    @Override
    public boolean supportsCreate() {
        return true;
    }

    /**
     * Gender entities support update operations.
     * Allows modification of gender display names.
     * @return true - update is supported
     */
    @Override
    public boolean supportsUpdate() {
        return true;
    }

    /**
     * Gender entities support delete operations.
     * Allows removal of custom gender types.
     * @return true - delete is supported
     */
    @Override
    public boolean supportsDelete() {
        return true;
    }
}
