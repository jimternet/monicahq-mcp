package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.UserFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing User entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link UserFieldMappingConfig} for User-specific field mappings and validation.
 * </p>
 * <p>
 * Note: The Users API may return 404 responses in some Monica configurations.
 * This may be an admin-only feature or not implemented in all Monica versions.
 * All operations include graceful error handling for API availability issues.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createUser - Create a new user</li>
 *   <li>getUser - Retrieve a user by ID</li>
 *   <li>updateUser - Update an existing user</li>
 *   <li>deleteUser - Delete a user by ID</li>
 *   <li>listUsers - List users with optional pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class UserService extends AbstractCrudService<Object> {

    private final UserFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a UserService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Users
     */
    public UserService(MonicaHqClient monicaClient,
                       ContentFormatter contentFormatter,
                       UserFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new user.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>firstName - The first name of the user (must be non-empty)</li>
     *   <li>email - The email address of the user (must be non-empty)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>lastName - The last name of the user</li>
     *   <li>isAdministrator - Whether the user is an administrator</li>
     *   <li>profilePictureUrl - URL to the user's profile picture</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created user data
     */
    public Mono<Map<String, Object>> createUser(Map<String, Object> arguments) {
        // Validate firstName and email are non-empty strings before delegating
        if (arguments != null && !arguments.isEmpty()) {
            validateRequiredString(arguments, "firstName");
            validateRequiredString(arguments, "email");
        }
        return create(arguments)
            .onErrorResume(error -> {
                log.warn("Users API may not be available: {}", error.getMessage());
                return Mono.error(new IllegalStateException(
                    "Users API is not available - this may be an admin-only feature or not implemented in this Monica version: "
                        + error.getMessage(), error));
            });
    }

    /**
     * Retrieves a user by its ID.
     *
     * @param arguments map containing "id" - the user ID to retrieve
     * @return a Mono containing the user data
     */
    public Mono<Map<String, Object>> getUser(Map<String, Object> arguments) {
        return get(arguments)
            .onErrorResume(error -> {
                log.warn("Users API may not be available: {}", error.getMessage());
                return Mono.error(new IllegalStateException(
                    "Users API is not available - this may be an admin-only feature: "
                        + error.getMessage(), error));
            });
    }

    /**
     * Updates an existing user.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the user to update</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>firstName - New first name for the user</li>
     *   <li>lastName - New last name for the user</li>
     *   <li>email - New email address for the user</li>
     *   <li>isAdministrator - Update administrator status</li>
     *   <li>profilePictureUrl - New profile picture URL</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the user ID
     * @return a Mono containing the updated user data
     */
    public Mono<Map<String, Object>> updateUser(Map<String, Object> arguments) {
        return update(arguments)
            .onErrorResume(error -> {
                log.warn("Users API may not be available: {}", error.getMessage());
                return Mono.error(new IllegalStateException(
                    "Users API is not available: " + error.getMessage(), error));
            });
    }

    /**
     * Deletes a user by its ID.
     *
     * @param arguments map containing "id" - the user ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteUser(Map<String, Object> arguments) {
        return delete(arguments)
            .onErrorResume(error -> {
                log.warn("Users API may not be available: {}", error.getMessage());
                return Mono.error(new IllegalStateException(
                    "Users API is not available: " + error.getMessage(), error));
            });
    }

    /**
     * Lists users with optional pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional pagination
     * @return a Mono containing the list of users and pagination metadata
     */
    public Mono<Map<String, Object>> listUsers(Map<String, Object> arguments) {
        return list(arguments)
            .onErrorResume(error -> {
                log.warn("Users API may not be available: {}", error.getMessage());
                return Mono.error(new IllegalStateException(
                    "Users API is not available - this may be an admin-only feature: "
                        + error.getMessage(), error));
            });
    }
}
