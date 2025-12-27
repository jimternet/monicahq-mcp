package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Address entities.
 * <p>
 * Defines the field mappings, validation rules, and API endpoint for Address CRUD operations.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>contactId (client) -> contact_id (API)</li>
 *   <li>postalCode (client) -> postal_code (API)</li>
 * </ul>
 * </p>
 * <p>
 * Required fields for creation: contactId
 * </p>
 * <p>
 * Pass-through fields (not remapped): name, street, city, province, country, latitude, longitude
 * </p>
 */
@Component
public class AddressFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/addresses";
    private static final String ENTITY_NAME = "Address";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "contactId", "contact_id",
        "postalCode", "postal_code"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "contact_id", "contactId",
        "postal_code", "postalCode",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Required fields for Address creation.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("contactId");

    /**
     * No list filter fields for Address.
     */
    private static final List<String> LIST_FILTER_FIELDS = List.of();

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
}
