package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.ActivityTypeCategoryFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Activity Type Category entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard operation implementations.
 * Uses {@link ActivityTypeCategoryFieldMappingConfig} for Activity Type Category-specific field mappings.
 * </p>
 * <p>
 * Activity Type Categories group related Activity Types together
 * (e.g., "Work", "Social", "Family").
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createActivityTypeCategory - Create a new activity type category</li>
 *   <li>getActivityTypeCategory - Retrieve an activity type category by ID</li>
 *   <li>updateActivityTypeCategory - Update an existing activity type category</li>
 *   <li>deleteActivityTypeCategory - Delete an activity type category</li>
 *   <li>listActivityTypeCategories - List activity type categories with optional pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class ActivityTypeCategoryService extends AbstractCrudService<Object> {

    private final ActivityTypeCategoryFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs an ActivityTypeCategoryService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Activity Type Categories
     */
    public ActivityTypeCategoryService(MonicaHqClient monicaClient,
                                       ContentFormatter contentFormatter,
                                       ActivityTypeCategoryFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new activity type category.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>name - The name of the category (non-empty string)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>parentId - The ID of a parent category</li>
     *   <li>description - A description of the category</li>
     *   <li>sortOrder - The sort order for display</li>
     * </ul>
     * </p>
     *
     * @param arguments the create arguments containing category data
     * @return a Mono containing the created category
     */
    public Mono<Map<String, Object>> createActivityTypeCategory(Map<String, Object> arguments) {
        // Validate name is a non-empty string before delegating to base
        if (arguments != null && !arguments.isEmpty()) {
            validateRequiredString(arguments, "name");
        }
        return create(arguments);
    }

    /**
     * Retrieves an activity type category by its ID.
     *
     * @param arguments map containing "id" - the category ID to retrieve
     * @return a Mono containing the category data
     */
    public Mono<Map<String, Object>> getActivityTypeCategory(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing activity type category.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the category to update</li>
     *   <li>name - The name of the category (non-empty string)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>parentId - The ID of a parent category</li>
     *   <li>description - A description of the category</li>
     *   <li>sortOrder - The sort order for display</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments containing category ID and data
     * @return a Mono containing the updated category
     */
    public Mono<Map<String, Object>> updateActivityTypeCategory(Map<String, Object> arguments) {
        // Validate name is a non-empty string before delegating to base
        if (arguments != null && !arguments.isEmpty()) {
            validateRequiredString(arguments, "name");
        }
        return update(arguments);
    }

    /**
     * Deletes an activity type category by its ID.
     *
     * @param arguments map containing "id" - the category ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteActivityTypeCategory(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists activity type categories with optional pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional pagination
     * @return a Mono containing the list of categories and pagination metadata
     */
    public Mono<Map<String, Object>> listActivityTypeCategories(Map<String, Object> arguments) {
        return list(arguments);
    }
}
