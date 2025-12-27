package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Group entities.
 * <p>
 * Defines the field mappings, validation rules, and API endpoint for Group CRUD operations.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>accountId (client) -> account_id (API)</li>
 * </ul>
 * </p>
 * <p>
 * Required fields for creation: name (must be non-empty string)
 * </p>
 * <p>
 * Required fields for update: name (must be non-empty string)
 * </p>
 */
@Component
public class GroupFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/groups";
    private static final String ENTITY_NAME = "Group";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "accountId", "account_id"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "account_id", "accountId",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Required fields for Group creation.
     * Note: name validation requires non-empty string check, done in service.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("name");

    /**
     * Required fields for Group update.
     * Note: name validation requires non-empty string check, done in service.
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
}
