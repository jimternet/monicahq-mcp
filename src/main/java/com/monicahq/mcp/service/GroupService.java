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
public class GroupService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> createGroup(Map<String, Object> arguments) {
        log.info("Creating group with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            validateGroupCreateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.post("/groups", apiRequest)
                .map(this::formatGroupResponse)
                .doOnSuccess(result -> log.info("Group created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create group: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for group creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getGroup(Map<String, Object> arguments) {
        log.info("Getting group with arguments: {}", arguments);
        
        try {
            Long groupId = extractGroupId(arguments);
            
            return monicaClient.get("/groups/" + groupId, null)
                .map(this::formatGroupResponse)
                .doOnSuccess(result -> log.info("Group retrieved successfully: {}", groupId))
                .doOnError(error -> log.error("Failed to get group {}: {}", groupId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for group retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateGroup(Map<String, Object> arguments) {
        log.info("Updating group with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            Long groupId = extractGroupId(mutableArguments);
            validateGroupUpdateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.put("/groups/" + groupId, apiRequest)
                .map(this::formatGroupResponse)
                .doOnSuccess(result -> log.info("Group updated successfully: {}", groupId))
                .doOnError(error -> log.error("Failed to update group {}: {}", groupId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for group update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteGroup(Map<String, Object> arguments) {
        log.info("Deleting group with arguments: {}", arguments);
        
        try {
            Long groupId = extractGroupId(arguments);
            
            return monicaClient.delete("/groups/" + groupId)
                .map(response -> {
                    String deletionMessage = String.format("Group %d deleted successfully", groupId);
                    return Map.of(
                        "content", List.of(Map.of(
                            "type", "text",
                            "text", deletionMessage
                        )),
                        "data", Map.of("deleted", true, "id", groupId)
                    );
                })
                .doOnSuccess(result -> log.info("Group deleted successfully: {}", groupId))
                .doOnError(error -> log.error("Failed to delete group {}: {}", groupId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for group deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listGroups(Map<String, Object> arguments) {
        log.info("Listing groups with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/groups", queryParams)
                .map(this::formatGroupsListResponse)
                .doOnSuccess(result -> log.info("Groups listed successfully"))
                .doOnError(error -> log.error("Failed to list groups: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error listing groups: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateGroupCreateArguments(Map<String, Object> arguments) {
        if (!arguments.containsKey("name") || arguments.get("name") == null || 
            arguments.get("name").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("name is required");
        }
    }

    private void validateGroupUpdateArguments(Map<String, Object> arguments) {
        validateGroupCreateArguments(arguments);
    }

    private Long extractGroupId(Map<String, Object> arguments) {
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
        if (arguments.containsKey("description")) {
            apiRequest.put("description", arguments.get("description"));
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

    private Map<String, Object> formatGroupResponse(Map<String, Object> apiResponse) {
        Map<String, Object> rawApiData;
        Map<String, Object> groupData;
        
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            rawApiData = rawData;
            groupData = mapFromApiFormat(rawData);
        } else {
            rawApiData = apiResponse;
            groupData = mapFromApiFormat(apiResponse);
        }
        
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", groupData);
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> formatGroupsListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> groups = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedGroups = groups.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedGroups);
        
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
                case "account_id" -> result.put("accountId", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}