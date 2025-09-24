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
public class TagService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

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
                    String formattedContent = contentFormatter.formatOperationResult(
                        "Delete", "Tag", tagId, true, 
                        "Tag with ID " + tagId + " has been deleted successfully"
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
        
        // Map camelCase to snake_case for MonicaHQ API
        arguments.forEach((key, value) -> {
            switch (key) {
                case "nameSlug" -> apiRequest.put("name_slug", value);
                case "contactCount" -> apiRequest.put("contact_count", value);
                default -> apiRequest.put(key, value);
            }
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
        Map<String, Object> tagData;
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            tagData = mapFromApiFormat(rawData);
        } else {
            tagData = mapFromApiFormat(apiResponse);
        }
        
        // Use raw API data for complete field coverage as per Constitutional Principle VI
        @SuppressWarnings("unchecked")
        Map<String, Object> rawApiData = apiResponse.containsKey("data") ? 
            (Map<String, Object>) apiResponse.get("data") : apiResponse;
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        // Return both data and content fields for protocol compliance
        Map<String, Object> result = new HashMap<>();
        result.put("data", tagData);
        
        // Format content for Claude Desktop visibility
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> formatTagListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> tags = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedTags = tags.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        // Format content for Claude Desktop visibility using raw API response
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedTags);
        
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) apiResponse.get("meta");
        if (meta != null) {
            result.put("meta", meta);
        }
        
        // Add content field for Claude Desktop visibility
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
                case "name_slug" -> result.put("nameSlug", value);
                case "contact_count" -> result.put("contactCount", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }

    public Mono<Map<String, Object>> listContactsByTag(Map<String, Object> arguments) {
        log.info("Listing contacts by tag with arguments: {}", arguments);
        
        try {
            Long tagId = extractTagId(arguments);
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/tags/" + tagId + "/contacts", queryParams)
                .map(this::formatContactsByTagResponse)
                .doOnSuccess(result -> log.info("Contacts by tag retrieved successfully: {}", tagId))
                .doOnError(error -> log.error("Failed to get contacts by tag {}: {}", tagId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contacts by tag: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private Map<String, Object> formatContactsByTagResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> contacts = (List<Map<String, Object>>) apiResponse.get("data");
        
        // Format contact data
        List<Map<String, Object>> formattedContacts = contacts.stream()
            .map(this::mapContactFromApiFormat)
            .toList();
        
        // Format content as escaped JSON for Claude Desktop accessibility
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedContacts);
        
        // Extract and preserve meta from API response
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) apiResponse.get("meta");
        if (meta != null) {
            result.put("meta", meta);
        }
        
        // Add content field for Claude Desktop visibility
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> mapContactFromApiFormat(Map<String, Object> apiData) {
        Map<String, Object> result = new HashMap<>();
        
        // Map snake_case to camelCase for contact data
        apiData.forEach((key, value) -> {
            switch (key) {
                case "first_name" -> result.put("firstName", value);
                case "last_name" -> result.put("lastName", value);
                case "gender_id" -> result.put("genderId", value);
                case "is_birthdate_known" -> result.put("isBirthdateKnown", value);
                case "is_deceased" -> result.put("isDeceased", value);
                case "is_deceased_date_known" -> result.put("isDeceasedDateKnown", value);
                case "job_title" -> result.put("jobTitle", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}