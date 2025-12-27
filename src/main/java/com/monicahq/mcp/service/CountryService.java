package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.CountryFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Country entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard operation implementations.
 * Uses {@link CountryFieldMappingConfig} for Country-specific field mappings.
 * </p>
 * <p>
 * Countries are read-only lookup values used for addresses.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>getCountry - Retrieve a country by ID</li>
 *   <li>listCountries - List countries with optional pagination</li>
 *   <li>searchCountries - Search countries with optional filters</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class CountryService extends AbstractCrudService<Object> {

    private final CountryFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a CountryService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Countries
     */
    public CountryService(MonicaHqClient monicaClient,
                          ContentFormatter contentFormatter,
                          CountryFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Retrieves a country by its ID.
     *
     * @param arguments map containing "id" - the country ID to retrieve
     * @return a Mono containing the country data
     */
    public Mono<Map<String, Object>> getCountry(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Lists countries with optional pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional pagination
     * @return a Mono containing the list of countries and pagination metadata
     */
    public Mono<Map<String, Object>> listCountries(Map<String, Object> arguments) {
        return list(arguments);
    }

    /**
     * Searches countries with optional filters.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>search - Search term to filter countries</li>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the search arguments including optional filters and pagination
     * @return a Mono containing the list of matching countries and pagination metadata
     */
    public Mono<Map<String, Object>> searchCountries(Map<String, Object> arguments) {
        // Search uses the same list endpoint with search filter
        return list(arguments);
    }
}
