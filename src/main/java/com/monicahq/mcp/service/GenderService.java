package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.GenderFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing gender types in MonicaHQ API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link GenderFieldMappingConfig} for Gender-specific field mappings.
 * </p>
 * <p>
 * Implements Constitutional Principle VII: API Discovery and Completeness.
 * Enables users to create and manage custom gender definitions per account.
 * </p>
 * <p>
 * Gap Analysis Phase 1: Extended from read-only to full CRUD support.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createGender - Create a new custom gender type</li>
 *   <li>getGender - Retrieve a specific gender by ID</li>
 *   <li>updateGender - Modify an existing gender</li>
 *   <li>deleteGender - Remove a gender type</li>
 *   <li>listGenders - List all available genders</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class GenderService extends AbstractCrudService<Object> {

    private final GenderFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a GenderService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Genders
     */
    public GenderService(MonicaHqClient monicaClient,
                         ContentFormatter contentFormatter,
                         GenderFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new custom gender type.
     * <p>
     * Enables creation of custom gender definitions for inclusive contact management.
     * </p>
     *
     * @param arguments must contain 'name' (the gender display name)
     * @return a Mono containing the created gender with full Monica API data
     */
    public Mono<Map<String, Object>> createGender(Map<String, Object> arguments) {
        return create(arguments);
    }

    /**
     * Retrieves a specific gender by ID.
     * <p>
     * Provides detailed information about a gender type including metadata.
     * </p>
     *
     * @param arguments must contain 'id' (the gender ID)
     * @return a Mono containing the gender details with full Monica API data
     */
    public Mono<Map<String, Object>> getGender(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing gender type.
     * <p>
     * Allows modification of gender display names and other properties.
     * </p>
     *
     * @param arguments must contain 'id' (the gender ID) and fields to update (e.g., 'name')
     * @return a Mono containing the updated gender with full Monica API data
     */
    public Mono<Map<String, Object>> updateGender(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes a gender type.
     * <p>
     * Removes a custom gender definition from the account.
     * Note: May fail if the gender is in use by existing contacts.
     * </p>
     *
     * @param arguments must contain 'id' (the gender ID to delete)
     * @return a Mono containing the deletion confirmation
     */
    public Mono<Map<String, Object>> deleteGender(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists all available genders from MonicaHQ API.
     * <p>
     * Includes both system-defined and custom gender types.
     * </p>
     *
     * @param arguments optional pagination arguments (page, limit)
     * @return a Mono containing the list of genders with full Monica API data
     */
    public Mono<Map<String, Object>> listGenders(Map<String, Object> arguments) {
        return list(arguments);
    }
}
