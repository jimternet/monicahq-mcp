package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.CallFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Call entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link CallFieldMappingConfig} for Call-specific field mappings and validation.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createCall - Create a new call record</li>
 *   <li>getCall - Retrieve a call by ID</li>
 *   <li>updateCall - Update an existing call</li>
 *   <li>deleteCall - Delete a call by ID</li>
 *   <li>listCalls - List calls with optional filtering and pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class CallService extends AbstractCrudService<Object> {

    private final CallFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a CallService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Calls
     */
    public CallService(MonicaHqClient monicaClient,
                       ContentFormatter contentFormatter,
                       CallFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new call record.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The ID of the contact associated with the call</li>
     *   <li>calledAt - The date/time of the call (must be non-empty)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>content - Notes about the call</li>
     *   <li>durationInMinutes - Duration of the call (0-1440 minutes)</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created call data
     */
    public Mono<Map<String, Object>> createCall(Map<String, Object> arguments) {
        // Custom validation for calledAt (must be non-empty string) and durationInMinutes
        if (arguments != null && !arguments.isEmpty()) {
            validateRequiredString(arguments, "calledAt");
            validateDuration(arguments);
        }
        return create(arguments);
    }

    /**
     * Retrieves a call by its ID.
     *
     * @param arguments map containing "id" - the call ID to retrieve
     * @return a Mono containing the call data
     */
    public Mono<Map<String, Object>> getCall(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing call.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the call to update</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>contactId - New contact ID</li>
     *   <li>calledAt - New call date/time</li>
     *   <li>content - New call notes</li>
     *   <li>durationInMinutes - New duration (0-1440 minutes)</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the call ID
     * @return a Mono containing the updated call data
     */
    public Mono<Map<String, Object>> updateCall(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes a call by its ID.
     *
     * @param arguments map containing "id" - the call ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteCall(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists calls with optional filtering and pagination.
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
     * @return a Mono containing the list of calls and pagination metadata
     */
    public Mono<Map<String, Object>> listCalls(Map<String, Object> arguments) {
        return list(arguments);
    }

    /**
     * Validates the duration field if present.
     * Duration must be between 0 and 1440 minutes (24 hours).
     *
     * @param arguments the arguments containing optional durationInMinutes
     * @throws IllegalArgumentException if duration is negative or exceeds 24 hours
     */
    private void validateDuration(Map<String, Object> arguments) {
        if (arguments.containsKey("durationInMinutes")) {
            Object durationObj = arguments.get("durationInMinutes");
            if (durationObj instanceof Number duration) {
                int durationMinutes = duration.intValue();
                if (durationMinutes < 0) {
                    throw new IllegalArgumentException("Duration cannot be negative");
                }
                if (durationMinutes > 1440) { // 24 hours = 1440 minutes
                    throw new IllegalArgumentException("Duration cannot exceed 24 hours (1440 minutes)");
                }
            }
        }
    }
}
