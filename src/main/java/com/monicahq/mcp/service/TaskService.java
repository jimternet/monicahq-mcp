package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.TaskFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Task entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link TaskFieldMappingConfig} for Task-specific field mappings and validation.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createTask - Create a new task for a contact</li>
 *   <li>getTask - Retrieve a task by ID</li>
 *   <li>updateTask - Update an existing task</li>
 *   <li>deleteTask - Delete a task by ID</li>
 *   <li>listTasks - List tasks with optional filtering and pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class TaskService extends AbstractCrudService<Object> {

    private final TaskFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a TaskService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Tasks
     */
    public TaskService(MonicaHqClient monicaClient,
                       ContentFormatter contentFormatter,
                       TaskFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new task for a contact.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The ID of the contact to associate the task with</li>
     *   <li>title - The title of the task</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>description - Description of the task</li>
     *   <li>completed - Whether the task is completed (default: false)</li>
     *   <li>completedAt - Timestamp when the task was completed</li>
     *   <li>dueDate - Due date for the task</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created task data
     */
    public Mono<Map<String, Object>> createTask(Map<String, Object> arguments) {
        return create(arguments);
    }

    /**
     * Retrieves a task by its ID.
     *
     * @param arguments map containing "id" - the task ID to retrieve
     * @return a Mono containing the task data
     */
    public Mono<Map<String, Object>> getTask(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing task.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the task to update</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>title - New title for the task</li>
     *   <li>description - New description for the task</li>
     *   <li>completed - Update completion status</li>
     *   <li>completedAt - Update completion timestamp</li>
     *   <li>dueDate - Update due date</li>
     *   <li>contactId - New contact association</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the task ID
     * @return a Mono containing the updated task data
     */
    public Mono<Map<String, Object>> updateTask(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes a task by its ID.
     *
     * @param arguments map containing "id" - the task ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteTask(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists tasks with optional filtering and pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     *   <li>contactId - Filter by contact ID</li>
     *   <li>completed - Filter by completion status</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional filters and pagination
     * @return a Mono containing the list of tasks and pagination metadata
     */
    public Mono<Map<String, Object>> listTasks(Map<String, Object> arguments) {
        return list(arguments);
    }

    public Mono<Map<String, Object>> listTasksByContact(Map<String, Object> arguments) {
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

            String endpoint = "/contacts/" + contactId + "/tasks";
            Map<String, String> queryParams = Map.of(
                "page", String.valueOf(page),
                "limit", String.valueOf(limit)
            );

            return monicaClient.get(endpoint, queryParams)
                .map(this::formatListResponse)
                .doOnSuccess(result -> log.info("Tasks for contact {} listed successfully", contactId))
                .doOnError(error -> log.error("Failed to list tasks for contact {}: {}", contactId, error.getMessage()));
        } catch (IllegalArgumentException e) {
            return Mono.error(new IllegalArgumentException("Invalid arguments: " + e.getMessage()));
        }
    }
}
