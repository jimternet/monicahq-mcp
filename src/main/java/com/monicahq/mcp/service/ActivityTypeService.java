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
public class ActivityTypeService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> createActivityType(Map<String, Object> arguments) {
        log.info("Creating activity type with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            validateActivityTypeCreateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.post("/activitytypes", apiRequest)
                .map(this::formatActivityTypeResponse)
                .doOnSuccess(result -> log.info("Activity type created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create activity type: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for activity type creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getActivityType(Map<String, Object> arguments) {
        log.info("Getting activity type with arguments: {}", arguments);
        
        try {
            Long activityTypeId = extractActivityTypeId(arguments);
            
            return monicaClient.get("/activitytypes/" + activityTypeId, null)
                .map(this::formatActivityTypeResponse)
                .doOnSuccess(result -> log.info("Activity type retrieved successfully: {}", activityTypeId))
                .doOnError(error -> log.error("Failed to get activity type {}: {}", activityTypeId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for activity type retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateActivityType(Map<String, Object> arguments) {
        log.info("Updating activity type with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            Long activityTypeId = extractActivityTypeId(mutableArguments);
            validateActivityTypeUpdateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.put("/activitytypes/" + activityTypeId, apiRequest)
                .map(this::formatActivityTypeResponse)
                .doOnSuccess(result -> log.info("Activity type updated successfully: {}", activityTypeId))
                .doOnError(error -> log.error("Failed to update activity type {}: {}", activityTypeId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for activity type update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteActivityType(Map<String, Object> arguments) {
        log.info("Deleting activity type with arguments: {}", arguments);
        
        try {
            Long activityTypeId = extractActivityTypeId(arguments);
            
            return monicaClient.delete("/activitytypes/" + activityTypeId)
                .map(response -> {
                    String deletionMessage = String.format("Activity type %d deleted successfully", activityTypeId);
                    return Map.of(
                        "content", List.of(Map.of(
                            "type", "text",
                            "text", deletionMessage
                        )),
                        "data", Map.of("deleted", true, "id", activityTypeId)
                    );
                })
                .doOnSuccess(result -> log.info("Activity type deleted successfully: {}", activityTypeId))
                .doOnError(error -> log.error("Failed to delete activity type {}: {}", activityTypeId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for activity type deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listActivityTypes(Map<String, Object> arguments) {
        log.info("Listing activity types with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/activitytypes", queryParams)
                .map(this::formatActivityTypesListResponse)
                .doOnSuccess(result -> log.info("Activity types listed successfully"))
                .doOnError(error -> log.error("Failed to list activity types: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error listing activity types: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateActivityTypeCreateArguments(Map<String, Object> arguments) {
        if (!arguments.containsKey("name") || arguments.get("name") == null || 
            arguments.get("name").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("name is required");
        }
    }

    private void validateActivityTypeUpdateArguments(Map<String, Object> arguments) {
        validateActivityTypeCreateArguments(arguments);
    }

    private Long extractActivityTypeId(Map<String, Object> arguments) {
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
        if (arguments.containsKey("categoryId")) {
            apiRequest.put("category_id", arguments.get("categoryId"));
        }
        if (arguments.containsKey("description")) {
            apiRequest.put("description", arguments.get("description"));
        }
        if (arguments.containsKey("icon")) {
            apiRequest.put("icon", arguments.get("icon"));
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

    private Map<String, Object> formatActivityTypeResponse(Map<String, Object> apiResponse) {
        Map<String, Object> rawApiData;
        Map<String, Object> activityTypeData;
        
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            rawApiData = rawData;
            activityTypeData = mapFromApiFormat(rawData);
        } else {
            rawApiData = apiResponse;
            activityTypeData = mapFromApiFormat(apiResponse);
        }
        
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", activityTypeData);
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> formatActivityTypesListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> activityTypes = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedActivityTypes = activityTypes.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedActivityTypes);
        
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
                case "category_id" -> result.put("categoryId", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}