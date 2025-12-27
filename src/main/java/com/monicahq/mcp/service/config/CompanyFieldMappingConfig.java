package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Company entities.
 * <p>
 * Defines the field mappings, validation rules, and API endpoint for Company CRUD operations.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>numberOfEmployees (client) -> number_of_employees (API)</li>
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
public class CompanyFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/companies";
    private static final String ENTITY_NAME = "Company";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "numberOfEmployees", "number_of_employees"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "number_of_employees", "numberOfEmployees",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Required fields for Company creation.
     * Note: name validation requires non-empty string check, done in service.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("name");

    /**
     * Required fields for Company update.
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
