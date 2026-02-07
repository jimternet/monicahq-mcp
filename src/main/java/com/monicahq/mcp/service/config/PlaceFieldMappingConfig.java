package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Place entities.
 * <p>
 * Defines the field mappings and API endpoint for Place operations.
 * Places represent geographic locations with address details.
 * </p>
 * <p>
 * Gap Analysis Phase 1: New entity implementation.
 * Enables tracking of locations for activities, contacts, and events.
 * </p>
 * <p>
 * Place fields include:
 * - street (optional)
 * - city (optional)
 * - province/state (optional)
 * - postal_code (optional)
 * - country (optional)
 * - latitude/longitude (optional, for geocoding)
 * </p>
 */
@Component
public class PlaceFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/places";
    private static final String ENTITY_NAME = "Place";

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "postal_code", "postalCode",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "postalCode", "postal_code"
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
        // All fields are optional for Place creation
        return Collections.emptySet();
    }

    @Override
    public List<String> getListFilterFields() {
        return List.of("city", "country");
    }

    @Override
    public boolean supportsCreate() {
        return true;
    }

    @Override
    public boolean supportsUpdate() {
        return true;
    }

    @Override
    public boolean supportsDelete() {
        return true;
    }
}
