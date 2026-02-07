package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.PlaceFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing geographic places/locations in MonicaHQ API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link PlaceFieldMappingConfig} for Place-specific field mappings.
 * </p>
 * <p>
 * Gap Analysis Phase 1: New entity implementation.
 * Enables tracking of locations for activities, contacts, and events.
 * </p>
 * <p>
 * Places represent geographic locations with address details including:
 * - Street address
 * - City, province/state
 * - Postal code, country
 * - Latitude/longitude coordinates for geocoding
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createPlace - Create a new place/location</li>
 *   <li>getPlace - Retrieve a specific place by ID</li>
 *   <li>updatePlace - Modify place information</li>
 *   <li>deletePlace - Remove a place</li>
 *   <li>listPlaces - List all places with optional filtering</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class PlaceService extends AbstractCrudService<Object> {

    private final PlaceFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a PlaceService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Places
     */
    public PlaceService(MonicaHqClient monicaClient,
                        ContentFormatter contentFormatter,
                        PlaceFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new place/location.
     * <p>
     * All fields are optional. Can create minimal places with just a city,
     * or detailed places with full address and coordinates.
     * </p>
     *
     * @param arguments may contain street, city, province, postalCode, country, latitude, longitude
     * @return a Mono containing the created place with full Monica API data
     */
    public Mono<Map<String, Object>> createPlace(Map<String, Object> arguments) {
        return create(arguments);
    }

    /**
     * Retrieves a specific place by ID.
     * <p>
     * Provides complete place information including all address fields and coordinates.
     * </p>
     *
     * @param arguments must contain 'id' (the place ID)
     * @return a Mono containing the place details with full Monica API data
     */
    public Mono<Map<String, Object>> getPlace(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing place.
     * <p>
     * Allows modification of any place fields. Only provided fields will be updated.
     * </p>
     *
     * @param arguments must contain 'id' (the place ID) and fields to update
     * @return a Mono containing the updated place with full Monica API data
     */
    public Mono<Map<String, Object>> updatePlace(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes a place.
     * <p>
     * Removes a place from the account.
     * Note: May fail if the place is referenced by activities or other entities.
     * </p>
     *
     * @param arguments must contain 'id' (the place ID to delete)
     * @return a Mono containing the deletion confirmation
     */
    public Mono<Map<String, Object>> deletePlace(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists all places with optional filtering and pagination.
     * <p>
     * Supports filtering by city and country.
     * </p>
     *
     * @param arguments optional: page, limit, city, country filters
     * @return a Mono containing the list of places with full Monica API data
     */
    public Mono<Map<String, Object>> listPlaces(Map<String, Object> arguments) {
        return list(arguments);
    }
}
