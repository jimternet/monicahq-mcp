package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.ActivityTypeFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Activity Type entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard operation implementations.
 * Uses {@link ActivityTypeFieldMappingConfig} for Activity Type-specific field mappings.
 * </p>
 * <p>
 * Activity Types define categories of activities that can be logged with contacts
 * (e.g., "Phone call", "Dinner", "Meeting").
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createActivityType - Create a new activity type</li>
 *   <li>getActivityType - Retrieve an activity type by ID</li>
 *   <li>updateActivityType - Update an existing activity type</li>
 *   <li>deleteActivityType - Delete an activity type</li>
 *   <li>listActivityTypes - List activity types with optional pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class ActivityTypeService extends AbstractCrudService<Object> {

    private final ActivityTypeFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs an ActivityTypeService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Activity Types
     */
    public ActivityTypeService(MonicaHqClient monicaClient,
                               ContentFormatter contentFormatter,
                               ActivityTypeFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new activity type.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>name - The name of the activity type (non-empty string)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>categoryId - The ID of the activity type category</li>
     *   <li>description - A description of the activity type</li>
     *   <li>icon - An icon identifier for the activity type</li>
     * </ul>
     * </p>
     *
     * @param arguments the create arguments containing activity type data
     * @return a Mono containing the created activity type
     */
    public Mono<Map<String, Object>> createActivityType(Map<String, Object> arguments) {
        // Validate name is a non-empty string before delegating to base
        if (arguments != null && !arguments.isEmpty()) {
            validateRequiredString(arguments, "name");
        }
        return create(arguments);
    }

    /**
     * Retrieves an activity type by its ID.
     *
     * @param arguments map containing "id" - the activity type ID to retrieve
     * @return a Mono containing the activity type data
     */
    public Mono<Map<String, Object>> getActivityType(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing activity type.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the activity type to update</li>
     *   <li>name - The name of the activity type (non-empty string)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>categoryId - The ID of the activity type category</li>
     *   <li>description - A description of the activity type</li>
     *   <li>icon - An icon identifier for the activity type</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments containing activity type ID and data
     * @return a Mono containing the updated activity type
     */
    public Mono<Map<String, Object>> updateActivityType(Map<String, Object> arguments) {
        // Validate name is a non-empty string before delegating to base
        if (arguments != null && !arguments.isEmpty()) {
            validateRequiredString(arguments, "name");
        }
        return update(arguments);
    }

    /**
     * Deletes an activity type by its ID.
     *
     * @param arguments map containing "id" - the activity type ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteActivityType(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists activity types with optional pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional pagination
     * @return a Mono containing the list of activity types and pagination metadata
     */
    public Mono<Map<String, Object>> listActivityTypes(Map<String, Object> arguments) {
        return list(arguments);
    }
}
