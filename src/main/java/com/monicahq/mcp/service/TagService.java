package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class TagService {

    private final MonicaHqClient monicaClient;

    public Mono<Map<String, Object>> createTag(Map<String, Object> arguments) {
        log.info("Creating tag with arguments: {}", arguments);
        
        try {
            validateTagCreateArguments(arguments);
            Map<String, Object> apiRequest = mapToApiFormat(arguments);
            
            return monicaClient.post("/tags", apiRequest)
                .map(this::formatTagResponse)
                .doOnSuccess(result -> log.info("Tag created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create tag: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for tag creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getTag(Map<String, Object> arguments) {
        log.info("Getting tag with arguments: {}", arguments);
        
        try {
            Long tagId = extractTagId(arguments);
            
            return monicaClient.get("/tags/" + tagId, null)
                .map(this::formatTagResponse)
                .doOnSuccess(result -> log.info("Tag retrieved successfully: {}", tagId))
                .doOnError(error -> log.error("Failed to get tag {}: {}", tagId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for tag retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateTag(Map<String, Object> arguments) {
        log.info("Updating tag with arguments: {}", arguments);
        
        try {
            Long tagId = extractTagId(arguments);
            
            Map<String, Object> updateData = new HashMap<>(arguments);
            updateData.remove("id");
            
            Map<String, Object> apiRequest = mapToApiFormat(updateData);
            
            return monicaClient.put("/tags/" + tagId, apiRequest)
                .map(this::formatTagResponse)
                .doOnSuccess(result -> log.info("Tag updated successfully: {}", tagId))
                .doOnError(error -> log.error("Failed to update tag {}: {}", tagId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for tag update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteTag(Map<String, Object> arguments) {
        log.info("Deleting tag with arguments: {}", arguments);
        
        try {
            Long tagId = extractTagId(arguments);
            
            return monicaClient.delete("/tags/" + tagId)
                .map(response -> {
                    Map<String, Object> result = new HashMap<>();
                    List<Map<String, Object>> content = List.of(
                        Map.of(
                            "type", "text",
                            "text", "Tag with ID " + tagId + " has been deleted successfully"
                        )
                    );
                    result.put("content", content);
                    return result;
                })
                .doOnSuccess(result -> log.info("Tag deleted successfully: {}", tagId))
                .doOnError(error -> log.error("Failed to delete tag {}: {}", tagId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for tag deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listTags(Map<String, Object> arguments) {
        log.info("Listing tags with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/tags", queryParams)
                .map(this::formatTagListResponse)
                .doOnSuccess(result -> log.info("Tags listed successfully"))
                .doOnError(error -> log.error("Failed to list tags: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error building query parameters: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateTagCreateArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("Tag creation arguments cannot be empty");
        }
        
        if (!arguments.containsKey("name") || 
            arguments.get("name") == null || 
            arguments.get("name").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("name is required");
        }
    }

    private Long extractTagId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("id")) {
            throw new IllegalArgumentException("Tag ID is required");
        }
        
        Object idValue = arguments.get("id");
        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }
        
        try {
            return Long.parseLong(idValue.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid tag ID format: " + idValue);
        }
    }

    private Map<String, Object> mapToApiFormat(Map<String, Object> arguments) {
        Map<String, Object> apiRequest = new HashMap<>();
        
        arguments.forEach((key, value) -> {
            apiRequest.put(key, value);
        });
        
        return apiRequest;
    }

    private Map<String, String> buildListQueryParams(Map<String, Object> arguments) {
        Map<String, String> queryParams = new HashMap<>();
        
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
        
        if (arguments.containsKey("search") && arguments.get("search") != null) {
            queryParams.put("query", arguments.get("search").toString());
        }
        
        return queryParams;
    }

    private Map<String, Object> formatTagResponse(Map<String, Object> apiResponse) {
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> tagData = (Map<String, Object>) apiResponse.get("data");
            return Map.of(
                "data", mapFromApiFormat(tagData)
            );
        }
        
        return Map.of("data", mapFromApiFormat(apiResponse));
    }

    private Map<String, Object> formatTagListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> tags = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedTags = tags.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedTags);
        
        // Add meta fields directly to result for MCP protocol
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) apiResponse.get("meta");
        if (meta != null) {
            result.put("meta", meta);
        }
        
        return result;
    }

    private Map<String, Object> mapFromApiFormat(Map<String, Object> apiData) {
        Map<String, Object> result = new HashMap<>();
        
        apiData.forEach((key, value) -> {
            switch (key) {
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}