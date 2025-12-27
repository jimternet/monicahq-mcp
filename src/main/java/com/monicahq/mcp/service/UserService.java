package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.*;

/**
 * Service for User entity operations.
 * Note: Users API may return 404 responses in some Monica configurations.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class UserService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> createUser(Map<String, Object> arguments) {
        log.info("Creating user with arguments: {}", arguments);
        
        try {
            validateUserCreateArguments(arguments);
            Map<String, Object> apiRequest = mapToApiFormat(arguments);
            
            return monicaClient.post("/users", apiRequest)
                .map(this::formatUserResponse)
                .doOnSuccess(result -> log.info("User created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create user: {}", error.getMessage()))
                .onErrorResume(error -> {
                    // Graceful handling for 404 or unavailable Users API
                    log.warn("Users API may not be available: {}", error.getMessage());
                    return Mono.error(new IllegalStateException(
                        "Users API is not available - this may be an admin-only feature or not implemented in this Monica version: " 
                        + error.getMessage(), error));
                });
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for user creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getUser(Map<String, Object> arguments) {
        log.info("Getting user with arguments: {}", arguments);
        
        try {
            Long userId = extractUserId(arguments);
            
            return monicaClient.get("/users/" + userId, null)
                .map(this::formatUserResponse)
                .doOnSuccess(result -> log.info("User retrieved successfully: {}", userId))
                .doOnError(error -> log.error("Failed to get user {}: {}", userId, error.getMessage()))
                .onErrorResume(error -> {
                    log.warn("Users API may not be available: {}", error.getMessage());
                    return Mono.error(new IllegalStateException(
                        "Users API is not available - this may be an admin-only feature: " 
                        + error.getMessage(), error));
                });
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for user retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateUser(Map<String, Object> arguments) {
        log.info("Updating user with arguments: {}", arguments);
        
        try {
            Long userId = extractUserId(arguments);
            validateUserUpdateArguments(arguments);
            
            Map<String, Object> apiRequest = mapToApiFormat(arguments);
            apiRequest.remove("id"); // Remove ID from update payload
            
            return monicaClient.put("/users/" + userId, apiRequest)
                .map(this::formatUserResponse)
                .doOnSuccess(result -> log.info("User updated successfully: {}", userId))
                .doOnError(error -> log.error("Failed to update user {}: {}", userId, error.getMessage()))
                .onErrorResume(error -> {
                    log.warn("Users API may not be available: {}", error.getMessage());
                    return Mono.error(new IllegalStateException(
                        "Users API is not available: " + error.getMessage(), error));
                });
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for user update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteUser(Map<String, Object> arguments) {
        log.info("Deleting user with arguments: {}", arguments);
        
        try {
            Long userId = extractUserId(arguments);
            
            return monicaClient.delete("/users/" + userId)
                .map(response -> {
                    String formattedContent = contentFormatter.formatOperationResult(
                        "Delete", "User", userId, true, 
                        "User with ID " + userId + " has been deleted successfully"
                    );
                    
                    Map<String, Object> result = new HashMap<>();
                    List<Map<String, Object>> content = List.of(
                        Map.of(
                            "type", "text",
                            "text", formattedContent
                        )
                    );
                    result.put("content", content);
                    return result;
                })
                .doOnSuccess(result -> log.info("User deleted successfully: {}", userId))
                .doOnError(error -> log.error("Failed to delete user {}: {}", userId, error.getMessage()))
                .onErrorResume(error -> {
                    log.warn("Users API may not be available: {}", error.getMessage());
                    return Mono.error(new IllegalStateException(
                        "Users API is not available: " + error.getMessage(), error));
                });
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for user deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listUsers(Map<String, Object> arguments) {
        log.info("Listing users with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/users", queryParams)
                .map(this::formatUserListResponse)
                .doOnSuccess(result -> log.info("Users listed successfully"))
                .doOnError(error -> log.error("Failed to list users: {}", error.getMessage()))
                .onErrorResume(error -> {
                    log.warn("Users API may not be available: {}", error.getMessage());
                    return Mono.error(new IllegalStateException(
                        "Users API is not available - this may be an admin-only feature: " 
                        + error.getMessage(), error));
                });
                
        } catch (Exception e) {
            log.error("Error building query parameters: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateUserCreateArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("User creation arguments cannot be empty");
        }
        
        if (!arguments.containsKey("firstName") || 
            arguments.get("firstName") == null || 
            arguments.get("firstName").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("firstName is required - please provide a first name for the user");
        }
        
        if (!arguments.containsKey("email") || 
            arguments.get("email") == null || 
            arguments.get("email").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("email is required - please provide an email address for the user");
        }
    }
    
    private void validateUserUpdateArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("User update arguments cannot be empty");
        }
    }

    private Long extractUserId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("id")) {
            throw new IllegalArgumentException("User ID is required");
        }

        Object idValue = arguments.get("id");
        if (idValue == null) {
            throw new IllegalArgumentException("User ID is required");
        }

        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }

        try {
            return Long.parseLong(idValue.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid user ID format: " + idValue);
        }
    }

    private Map<String, Object> mapToApiFormat(Map<String, Object> arguments) {
        Map<String, Object> apiRequest = new HashMap<>();
        
        // Map camelCase to snake_case for MonicaHQ API
        arguments.forEach((key, value) -> {
            switch (key) {
                case "firstName" -> apiRequest.put("first_name", value);
                case "lastName" -> apiRequest.put("last_name", value);
                case "isAdministrator" -> apiRequest.put("is_administrator", value);
                case "profilePictureUrl" -> apiRequest.put("profile_picture_url", value);
                default -> apiRequest.put(key, value);
            }
        });
        
        return apiRequest;
    }

    private Map<String, String> buildListQueryParams(Map<String, Object> arguments) {
        Map<String, String> queryParams = new HashMap<>();
        
        // Handle pagination
        if (arguments.containsKey("page")) {
            queryParams.put("page", arguments.get("page").toString());
        } else {
            queryParams.put("page", "1");
        }
        
        if (arguments.containsKey("limit")) {
            int limit = Math.min(100, Math.max(1, Integer.parseInt(arguments.get("limit").toString())));
            queryParams.put("limit", String.valueOf(limit));
        } else {
            queryParams.put("limit", "10");
        }
        
        return queryParams;
    }

    private Map<String, Object> formatUserResponse(Map<String, Object> apiResponse) {
        Map<String, Object> userData;
        Map<String, Object> rawApiData;
        
        if (apiResponse.containsKey("data")) {
            // Single user response
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            rawApiData = rawData;
            userData = mapFromApiFormat(rawData);
        } else {
            rawApiData = apiResponse;
            userData = mapFromApiFormat(apiResponse);
        }
        
        // Use raw API data as escaped JSON for complete field coverage
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", userData);
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> formatUserListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> users = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedUsers = users.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedUsers);
        
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) apiResponse.get("meta");
        if (meta != null) {
            result.put("meta", meta);
        }
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> mapFromApiFormat(Map<String, Object> apiData) {
        Map<String, Object> result = new HashMap<>();
        
        // Map snake_case to camelCase
        apiData.forEach((key, value) -> {
            switch (key) {
                case "first_name" -> result.put("firstName", value);
                case "last_name" -> result.put("lastName", value);
                case "is_administrator" -> result.put("isAdministrator", value);
                case "profile_picture_url" -> result.put("profilePictureUrl", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}