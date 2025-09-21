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
public class RelationshipTypeGroupService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> getRelationshipTypeGroup(Map<String, Object> arguments) {
        log.info("Getting relationship type group with arguments: {}", arguments);
        
        try {
            Long relationshipTypeGroupId = extractRelationshipTypeGroupId(arguments);
            
            return monicaClient.get("/relationshiptypegroups/" + relationshipTypeGroupId, null)
                .map(this::formatRelationshipTypeGroupResponse)
                .doOnSuccess(result -> log.info("Relationship type group retrieved successfully: {}", relationshipTypeGroupId))
                .doOnError(error -> log.error("Failed to get relationship type group {}: {}", relationshipTypeGroupId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for relationship type group retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listRelationshipTypeGroups(Map<String, Object> arguments) {
        log.info("Listing relationship type groups with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/relationshiptypegroups", queryParams)
                .map(this::formatRelationshipTypeGroupsListResponse)
                .doOnSuccess(result -> log.info("Relationship type groups listed successfully"))
                .doOnError(error -> log.error("Failed to list relationship type groups: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error listing relationship type groups: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private Long extractRelationshipTypeGroupId(Map<String, Object> arguments) {
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

    private Map<String, Object> formatRelationshipTypeGroupResponse(Map<String, Object> apiResponse) {
        Map<String, Object> rawApiData;
        Map<String, Object> relationshipTypeGroupData;
        
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            rawApiData = rawData;
            relationshipTypeGroupData = mapFromApiFormat(rawData);
        } else {
            rawApiData = apiResponse;
            relationshipTypeGroupData = mapFromApiFormat(apiResponse);
        }
        
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", relationshipTypeGroupData);
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> formatRelationshipTypeGroupsListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> relationshipTypeGroups = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedRelationshipTypeGroups = relationshipTypeGroups.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedRelationshipTypeGroups);
        
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
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}