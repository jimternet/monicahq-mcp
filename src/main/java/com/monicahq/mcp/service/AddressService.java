package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.AddressFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Address entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link AddressFieldMappingConfig} for Address-specific field mappings and validation.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createAddress - Create a new address</li>
 *   <li>getAddress - Retrieve an address by ID</li>
 *   <li>updateAddress - Update an existing address</li>
 *   <li>deleteAddress - Delete an address by ID</li>
 *   <li>listAddresses - List addresses with optional pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class AddressService extends AbstractCrudService<Object> {

    private final AddressFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs an AddressService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Addresses
     */
    public AddressService(MonicaHqClient monicaClient,
                          ContentFormatter contentFormatter,
                          AddressFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new address.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The ID of the contact associated with the address</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>name - Label for the address (e.g., "Home", "Work")</li>
     *   <li>street - Street address</li>
     *   <li>city - City name</li>
     *   <li>province - State/province</li>
     *   <li>postalCode - Postal/ZIP code</li>
     *   <li>country - Country code or name</li>
     *   <li>latitude - GPS latitude</li>
     *   <li>longitude - GPS longitude</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created address data
     */
    public Mono<Map<String, Object>> createAddress(Map<String, Object> arguments) {
        return create(arguments);
    }

    /**
     * Retrieves an address by its ID.
     *
     * @param arguments map containing "id" - the address ID to retrieve
     * @return a Mono containing the address data
     */
    public Mono<Map<String, Object>> getAddress(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing address.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the address to update</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>contactId - New contact ID</li>
     *   <li>name - New label for the address</li>
     *   <li>street - New street address</li>
     *   <li>city - New city name</li>
     *   <li>province - New state/province</li>
     *   <li>postalCode - New postal/ZIP code</li>
     *   <li>country - New country code or name</li>
     *   <li>latitude - New GPS latitude</li>
     *   <li>longitude - New GPS longitude</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the address ID
     * @return a Mono containing the updated address data
     */
    public Mono<Map<String, Object>> updateAddress(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes an address by its ID.
     *
     * @param arguments map containing "id" - the address ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteAddress(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists addresses with optional pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional pagination
     * @return a Mono containing the list of addresses and pagination metadata
     */
    public Mono<Map<String, Object>> listAddresses(Map<String, Object> arguments) {
        return list(arguments);
    }
}
