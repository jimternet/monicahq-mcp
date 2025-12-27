package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.GroupFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Group entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link GroupFieldMappingConfig} for Group-specific field mappings and validation.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createGroup - Create a new group</li>
 *   <li>getGroup - Retrieve a group by ID</li>
 *   <li>updateGroup - Update an existing group</li>
 *   <li>deleteGroup - Delete a group by ID</li>
 *   <li>listGroups - List groups with optional pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class GroupService extends AbstractCrudService<Object> {

    private final GroupFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a GroupService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Groups
     */
    public GroupService(MonicaHqClient monicaClient,
                        ContentFormatter contentFormatter,
                        GroupFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new group.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>name - The name of the group (must be non-empty)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>description - Description of the group</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created group data
     */
    public Mono<Map<String, Object>> createGroup(Map<String, Object> arguments) {
        // Validate name is non-empty string before delegating to base class
        if (arguments != null && !arguments.isEmpty()) {
            validateRequiredString(arguments, "name");
        }
        return create(arguments);
    }

    /**
     * Retrieves a group by its ID.
     *
     * @param arguments map containing "id" - the group ID to retrieve
     * @return a Mono containing the group data
     */
    public Mono<Map<String, Object>> getGroup(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing group.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the group to update</li>
     *   <li>name - The name of the group (must be non-empty)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>description - Description of the group</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the group ID
     * @return a Mono containing the updated group data
     */
    public Mono<Map<String, Object>> updateGroup(Map<String, Object> arguments) {
        // Validate name is non-empty string before delegating to base class
        if (arguments != null && !arguments.isEmpty()) {
            validateRequiredString(arguments, "name");
        }
        return update(arguments);
    }

    /**
     * Deletes a group by its ID.
     *
     * @param arguments map containing "id" - the group ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteGroup(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists groups with optional pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional pagination
     * @return a Mono containing the list of groups and pagination metadata
     */
    public Mono<Map<String, Object>> listGroups(Map<String, Object> arguments) {
        return list(arguments);
    }
}
