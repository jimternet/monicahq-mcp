package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.CurrencyFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Currency entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard operation implementations.
 * Uses {@link CurrencyFieldMappingConfig} for Currency-specific field mappings.
 * </p>
 * <p>
 * Currencies are read-only lookup values used for monetary amounts.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>getCurrency - Retrieve a currency by ID</li>
 *   <li>listCurrencies - List currencies with optional pagination</li>
 *   <li>searchCurrencies - Search currencies with optional filters</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class CurrencyService extends AbstractCrudService<Object> {

    private final CurrencyFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a CurrencyService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Currencies
     */
    public CurrencyService(MonicaHqClient monicaClient,
                           ContentFormatter contentFormatter,
                           CurrencyFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Retrieves a currency by its ID.
     *
     * @param arguments map containing "id" - the currency ID to retrieve
     * @return a Mono containing the currency data
     */
    public Mono<Map<String, Object>> getCurrency(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Lists currencies with optional pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional pagination
     * @return a Mono containing the list of currencies and pagination metadata
     */
    public Mono<Map<String, Object>> listCurrencies(Map<String, Object> arguments) {
        return list(arguments);
    }

    /**
     * Searches currencies with optional filters.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>search - Search term to filter currencies</li>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the search arguments including optional filters and pagination
     * @return a Mono containing the list of matching currencies and pagination metadata
     */
    public Mono<Map<String, Object>> searchCurrencies(Map<String, Object> arguments) {
        // Search uses the same list endpoint with search filter
        return list(arguments);
    }
}
