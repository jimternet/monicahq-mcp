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
public class RelationshipTypeService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> getRelationshipType(Map<String, Object> arguments) {
        log.info("Getting relationship type with arguments: {}", arguments);
        
        try {
            Long relationshipTypeId = extractRelationshipTypeId(arguments);
            
            return monicaClient.get("/relationshiptypes/" + relationshipTypeId, null)
                .map(this::formatRelationshipTypeResponse)
                .doOnSuccess(result -> log.info("Relationship type retrieved successfully: {}", relationshipTypeId))
                .doOnError(error -> log.error("Failed to get relationship type {}: {}", relationshipTypeId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for relationship type retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listRelationshipTypes(Map<String, Object> arguments) {
        log.info("Listing relationship types with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/relationshiptypes", queryParams)
                .map(this::formatRelationshipTypesListResponse)
                .doOnSuccess(result -> log.info("Relationship types listed successfully"))
                .doOnError(error -> log.error("Failed to list relationship types: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error listing relationship types: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private Long extractRelationshipTypeId(Map<String, Object> arguments) {
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

    private Map<String, Object> formatRelationshipTypeResponse(Map<String, Object> apiResponse) {
        Map<String, Object> rawApiData;
        Map<String, Object> relationshipTypeData;
        
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            rawApiData = rawData;
            relationshipTypeData = mapFromApiFormat(rawData);
        } else {
            rawApiData = apiResponse;
            relationshipTypeData = mapFromApiFormat(apiResponse);
        }
        
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", relationshipTypeData);
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> formatRelationshipTypesListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> relationshipTypes = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedRelationshipTypes = relationshipTypes.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedRelationshipTypes);
        
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
                case "name_reverse" -> result.put("nameReverse", value);
                case "relationship_type_group_id" -> result.put("relationshipTypeGroupId", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}