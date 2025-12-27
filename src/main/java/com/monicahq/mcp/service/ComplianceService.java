package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.ComplianceFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Compliance entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link ComplianceFieldMappingConfig} for Compliance-specific field mappings and validation.
 * </p>
 * <p>
 * Note: Compliance API endpoints may not be clearly defined in all Monica versions.
 * All operations include graceful error handling for API availability issues.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createCompliance - Create a new compliance record</li>
 *   <li>getCompliance - Retrieve a compliance record by ID</li>
 *   <li>updateCompliance - Update an existing compliance record</li>
 *   <li>deleteCompliance - Delete a compliance record by ID</li>
 *   <li>listCompliance - List compliance records with optional filtering and pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class ComplianceService extends AbstractCrudService<Object> {

    private final ComplianceFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a ComplianceService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Compliance
     */
    public ComplianceService(MonicaHqClient monicaClient,
                             ContentFormatter contentFormatter,
                             ComplianceFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new compliance record.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>type - The compliance type (must be non-empty)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>contactId - The ID of the contact this compliance applies to</li>
     *   <li>isActive - Whether the compliance is currently active</li>
     *   <li>dataRetentionDays - Number of days to retain data</li>
     *   <li>privacyLevel - The privacy level setting</li>
     *   <li>consentGiven - Whether consent has been given</li>
     *   <li>consentDate - Date consent was given</li>
     *   <li>auditRequired - Whether audit is required</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created compliance data
     */
    public Mono<Map<String, Object>> createCompliance(Map<String, Object> arguments) {
        // Validate type is non-empty string before delegating
        if (arguments != null && !arguments.isEmpty()) {
            validateRequiredString(arguments, "type");
        }
        return create(arguments)
            .onErrorResume(error -> {
                log.warn("Compliance API may not be available: {}", error.getMessage());
                return Mono.error(new IllegalStateException(
                    "Compliance API is not available - endpoints may not be implemented in this Monica version: "
                        + error.getMessage(), error));
            });
    }

    /**
     * Retrieves a compliance record by its ID.
     *
     * @param arguments map containing "id" - the compliance ID to retrieve
     * @return a Mono containing the compliance data
     */
    public Mono<Map<String, Object>> getCompliance(Map<String, Object> arguments) {
        return get(arguments)
            .onErrorResume(error -> {
                log.warn("Compliance API may not be available: {}", error.getMessage());
                return Mono.error(new IllegalStateException(
                    "Compliance API is not available: " + error.getMessage(), error));
            });
    }

    /**
     * Updates an existing compliance record.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the compliance record to update</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>type - New compliance type</li>
     *   <li>contactId - New contact association</li>
     *   <li>isActive - Update active status</li>
     *   <li>dataRetentionDays - Update data retention period</li>
     *   <li>privacyLevel - Update privacy level</li>
     *   <li>consentGiven - Update consent status</li>
     *   <li>consentDate - Update consent date</li>
     *   <li>auditRequired - Update audit requirement</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the compliance ID
     * @return a Mono containing the updated compliance data
     */
    public Mono<Map<String, Object>> updateCompliance(Map<String, Object> arguments) {
        return update(arguments)
            .onErrorResume(error -> {
                log.warn("Compliance API may not be available: {}", error.getMessage());
                return Mono.error(new IllegalStateException(
                    "Compliance API is not available: " + error.getMessage(), error));
            });
    }

    /**
     * Deletes a compliance record by its ID.
     *
     * @param arguments map containing "id" - the compliance ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteCompliance(Map<String, Object> arguments) {
        return delete(arguments)
            .onErrorResume(error -> {
                log.warn("Compliance API may not be available: {}", error.getMessage());
                return Mono.error(new IllegalStateException(
                    "Compliance API is not available: " + error.getMessage(), error));
            });
    }

    /**
     * Lists compliance records with optional filtering and pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     *   <li>contactId - Filter by contact ID</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional filters and pagination
     * @return a Mono containing the list of compliance records and pagination metadata
     */
    public Mono<Map<String, Object>> listCompliance(Map<String, Object> arguments) {
        return list(arguments)
            .onErrorResume(error -> {
                log.warn("Compliance API may not be available: {}", error.getMessage());
                return Mono.error(new IllegalStateException(
                    "Compliance API is not available: " + error.getMessage(), error));
            });
    }
}
