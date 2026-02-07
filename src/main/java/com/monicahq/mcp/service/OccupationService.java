package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.OccupationFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Occupation entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link OccupationFieldMappingConfig} for Occupation-specific field mappings and validation.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createOccupation - Create a new occupation</li>
 *   <li>getOccupation - Retrieve an occupation by ID</li>
 *   <li>updateOccupation - Update an existing occupation</li>
 *   <li>deleteOccupation - Delete an occupation by ID</li>
 *   <li>listOccupations - List occupations with optional pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class OccupationService extends AbstractCrudService<Object> {

    private final OccupationFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs an OccupationService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Occupations
     */
    public OccupationService(MonicaHqClient monicaClient,
                             ContentFormatter contentFormatter,
                             OccupationFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new occupation.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The ID of the contact this occupation belongs to</li>
     *   <li>title - The job title (must be non-empty)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>companyId - The ID of the company</li>
     *   <li>description - Description of the occupation</li>
     *   <li>salary - Salary amount</li>
     *   <li>salaryUnit - Unit for salary (e.g., "year", "month")</li>
     *   <li>currentlyWorksHere - Whether the contact currently works here</li>
     *   <li>startDate - Start date of the occupation</li>
     *   <li>endDate - End date of the occupation</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created occupation data
     */
    public Mono<Map<String, Object>> createOccupation(Map<String, Object> arguments) {
        // Validate title is non-empty string before delegating to base class
        if (arguments != null && !arguments.isEmpty()) {
            validateRequiredString(arguments, "title");
        }
        return create(arguments);
    }

    /**
     * Retrieves an occupation by its ID.
     *
     * @param arguments map containing "id" - the occupation ID to retrieve
     * @return a Mono containing the occupation data
     */
    public Mono<Map<String, Object>> getOccupation(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing occupation.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the occupation to update</li>
     * </ul>
     * Optional arguments (if title is provided, it must be non-empty):
     * <ul>
     *   <li>title - The job title</li>
     *   <li>companyId - The ID of the company</li>
     *   <li>description - Description of the occupation</li>
     *   <li>salary - Salary amount</li>
     *   <li>salaryUnit - Unit for salary</li>
     *   <li>currentlyWorksHere - Whether the contact currently works here</li>
     *   <li>startDate - Start date of the occupation</li>
     *   <li>endDate - End date of the occupation</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the occupation ID
     * @return a Mono containing the updated occupation data
     */
    public Mono<Map<String, Object>> updateOccupation(Map<String, Object> arguments) {
        // Validate title is non-empty if provided
        if (arguments != null && !arguments.isEmpty() && arguments.containsKey("title")) {
            Object titleValue = arguments.get("title");
            if (titleValue == null || titleValue.toString().trim().isEmpty()) {
                return Mono.error(new IllegalArgumentException("title cannot be empty"));
            }
        }
        return update(arguments);
    }

    /**
     * Deletes an occupation by its ID.
     *
     * @param arguments map containing "id" - the occupation ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteOccupation(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists occupations with optional pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional pagination
     * @return a Mono containing the list of occupations and pagination metadata
     */
    public Mono<Map<String, Object>> listOccupations(Map<String, Object> arguments) {
        return list(arguments);
    }

    public Mono<Map<String, Object>> listOccupationsByContact(Map<String, Object> arguments) {
        try {
            if (arguments == null || !arguments.containsKey("contactId")) {
                throw new IllegalArgumentException("contactId is required");
            }
            Object contactIdValue = arguments.get("contactId");
            if (contactIdValue == null) {
                throw new IllegalArgumentException("contactId is required");
            }
            Long contactId = contactIdValue instanceof Number
                ? ((Number) contactIdValue).longValue()
                : Long.parseLong(contactIdValue.toString().trim());

            int page = 1;
            int limit = 10;
            if (arguments.containsKey("page")) {
                page = Integer.parseInt(arguments.get("page").toString());
            }
            if (arguments.containsKey("limit")) {
                limit = parseLimit(arguments.get("limit"));
            }

            String endpoint = "/contacts/" + contactId + "/occupations";
            Map<String, String> queryParams = Map.of(
                "page", String.valueOf(page),
                "limit", String.valueOf(limit)
            );

            return monicaClient.get(endpoint, queryParams)
                .map(this::formatListResponse)
                .doOnSuccess(result -> log.info("Occupations for contact {} listed successfully", contactId))
                .doOnError(error -> log.error("Failed to list occupations for contact {}: {}", contactId, error.getMessage()));
        } catch (IllegalArgumentException e) {
            return Mono.error(new IllegalArgumentException("Invalid arguments: " + e.getMessage()));
        }
    }
}
