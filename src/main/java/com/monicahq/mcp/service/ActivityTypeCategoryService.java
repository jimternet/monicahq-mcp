package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class ActivityTypeCategoryService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> createActivityTypeCategory(Map<String, Object> arguments) {
        log.info("Creating activity type category with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            validateActivityTypeCategoryCreateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.post("/activitytypecategories", apiRequest)
                .map(this::formatActivityTypeCategoryResponse)
                .doOnSuccess(result -> log.info("Activity type category created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create activity type category: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for activity type category creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getActivityTypeCategory(Map<String, Object> arguments) {
        log.info("Getting activity type category with arguments: {}", arguments);
        
        try {
            Long categoryId = extractActivityTypeCategoryId(arguments);
            
            return monicaClient.get("/activitytypecategories/" + categoryId, null)
                .map(this::formatActivityTypeCategoryResponse)
                .doOnSuccess(result -> log.info("Activity type category retrieved successfully: {}", categoryId))
                .doOnError(error -> log.error("Failed to get activity type category {}: {}", categoryId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for activity type category retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateActivityTypeCategory(Map<String, Object> arguments) {
        log.info("Updating activity type category with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            Long categoryId = extractActivityTypeCategoryId(mutableArguments);
            validateActivityTypeCategoryUpdateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.put("/activitytypecategories/" + categoryId, apiRequest)
                .map(this::formatActivityTypeCategoryResponse)
                .doOnSuccess(result -> log.info("Activity type category updated successfully: {}", categoryId))
                .doOnError(error -> log.error("Failed to update activity type category {}: {}", categoryId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for activity type category update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteActivityTypeCategory(Map<String, Object> arguments) {
        log.info("Deleting activity type category with arguments: {}", arguments);
        
        try {
            Long categoryId = extractActivityTypeCategoryId(arguments);
            
            return monicaClient.delete("/activitytypecategories/" + categoryId)
                .map(response -> {
                    String deletionMessage = String.format("Activity type category %d deleted successfully", categoryId);
                    return Map.of(
                        "content", List.of(Map.of(
                            "type", "text",
                            "text", deletionMessage
                        )),
                        "data", Map.of("deleted", true, "id", categoryId)
                    );
                })
                .doOnSuccess(result -> log.info("Activity type category deleted successfully: {}", categoryId))
                .doOnError(error -> log.error("Failed to delete activity type category {}: {}", categoryId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for activity type category deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listActivityTypeCategories(Map<String, Object> arguments) {
        log.info("Listing activity type categories with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/activitytypecategories", queryParams)
                .map(this::formatActivityTypeCategoriesListResponse)
                .doOnSuccess(result -> log.info("Activity type categories listed successfully"))
                .doOnError(error -> log.error("Failed to list activity type categories: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error listing activity type categories: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateActivityTypeCategoryCreateArguments(Map<String, Object> arguments) {
        if (!arguments.containsKey("name") || arguments.get("name") == null || 
            arguments.get("name").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("name is required");
        }
    }

    private void validateActivityTypeCategoryUpdateArguments(Map<String, Object> arguments) {
        validateActivityTypeCategoryCreateArguments(arguments);
    }

    private Long extractActivityTypeCategoryId(Map<String, Object> arguments) {
        Object idObj = arguments.get("id");
        if (idObj == null) {
            throw new IllegalArgumentException("id is required");
        }
        
        if (idObj instanceof Number) {
            return ((Number) idObj).longValue();
        }
        
        try {
            return Long.parseLong(idObj.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("id must be a valid number");
        }
    }

    private Map<String, Object> mapToApiFormat(Map<String, Object> arguments) {
        Map<String, Object> apiRequest = new HashMap<>();
        
        if (arguments.containsKey("name")) {
            apiRequest.put("name", arguments.get("name"));
        }
        if (arguments.containsKey("parentId")) {
            apiRequest.put("parent_id", arguments.get("parentId"));
        }
        if (arguments.containsKey("description")) {
            apiRequest.put("description", arguments.get("description"));
        }
        if (arguments.containsKey("sortOrder")) {
            apiRequest.put("sort_order", arguments.get("sortOrder"));
        }
        
        return apiRequest;
    }

    private Map<String, String> buildListQueryParams(Map<String, Object> arguments) {
        Map<String, String> queryParams = new HashMap<>();
        
        if (arguments.containsKey("limit")) {
            queryParams.put("limit", arguments.get("limit").toString());
        } else {
            queryParams.put("limit", "10");
        }
        
        if (arguments.containsKey("page")) {
            queryParams.put("page", arguments.get("page").toString());
        } else {
            queryParams.put("page", "1");
        }
        
        return queryParams;
    }

    private Map<String, Object> formatActivityTypeCategoryResponse(Map<String, Object> apiResponse) {
        Map<String, Object> rawApiData;
        Map<String, Object> categoryData;
        
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            rawApiData = rawData;
            categoryData = mapFromApiFormat(rawData);
        } else {
            rawApiData = apiResponse;
            categoryData = mapFromApiFormat(apiResponse);
        }
        
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", categoryData);
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> formatActivityTypeCategoriesListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> categories = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedCategories = categories.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedCategories);
        
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
        
        apiData.forEach((key, value) -> {
            switch (key) {
                case "parent_id" -> result.put("parentId", value);
                case "sort_order" -> result.put("sortOrder", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}