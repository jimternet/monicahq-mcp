package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.dto.Relationship;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class RelationshipService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> createRelationship(Map<String, Object> arguments) {
        log.info("Creating relationship with arguments: {}", arguments);
        
        try {
            // Create mutable copy for validation and defaults
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            
            // Validate required fields
            validateRelationshipCreateArguments(mutableArguments);
            
            // Map arguments to API format
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            // Call MonicaHQ API
            return monicaClient.post("/relationships", apiRequest)
                .map(this::formatRelationshipResponse)
                .doOnSuccess(result -> log.info("Relationship created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create relationship: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for relationship creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getRelationship(Map<String, Object> arguments) {
        log.info("Getting relationship with arguments: {}", arguments);
        
        try {
            Long relationshipId = extractRelationshipId(arguments);
            
            return monicaClient.get("/relationships/" + relationshipId, null)
                .map(this::formatRelationshipResponse)
                .doOnSuccess(result -> log.info("Relationship retrieved successfully: {}", relationshipId))
                .doOnError(error -> log.error("Failed to get relationship {}: {}", relationshipId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for relationship retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateRelationship(Map<String, Object> arguments) {
        log.info("Updating relationship with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            Long relationshipId = extractRelationshipId(mutableArguments);
            
            // Validate required fields for update
            validateRelationshipUpdateArguments(mutableArguments);
            
            // Map arguments to API format
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.put("/relationships/" + relationshipId, apiRequest)
                .map(this::formatRelationshipResponse)
                .doOnSuccess(result -> log.info("Relationship updated successfully: {}", relationshipId))
                .doOnError(error -> log.error("Failed to update relationship {}: {}", relationshipId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for relationship update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteRelationship(Map<String, Object> arguments) {
        log.info("Deleting relationship with arguments: {}", arguments);
        
        try {
            Long relationshipId = extractRelationshipId(arguments);
            
            return monicaClient.delete("/relationships/" + relationshipId)
                .map(response -> {
                    String deletionMessage = String.format("Relationship %d deleted successfully", relationshipId);
                    return Map.of(
                        "content", List.of(Map.of(
                            "type", "text",
                            "text", deletionMessage
                        )),
                        "data", Map.of("deleted", true, "id", relationshipId)
                    );
                })
                .doOnSuccess(result -> log.info("Relationship deleted successfully: {}", relationshipId))
                .doOnError(error -> log.error("Failed to delete relationship {}: {}", relationshipId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for relationship deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listRelationships(Map<String, Object> arguments) {
        log.info("Listing relationships with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/relationships", queryParams)
                .map(this::formatRelationshipsListResponse)
                .doOnSuccess(result -> log.info("Relationships listed successfully"))
                .doOnError(error -> log.error("Failed to list relationships: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error listing relationships: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateRelationshipCreateArguments(Map<String, Object> arguments) {
        if (!arguments.containsKey("contactIs") || arguments.get("contactIs") == null) {
            throw new IllegalArgumentException("contactIs is required");
        }
        if (!arguments.containsKey("ofContact") || arguments.get("ofContact") == null) {
            throw new IllegalArgumentException("ofContact is required");
        }
        if (!arguments.containsKey("relationshipTypeId") || arguments.get("relationshipTypeId") == null) {
            throw new IllegalArgumentException("relationshipTypeId is required");
        }
    }

    private void validateRelationshipUpdateArguments(Map<String, Object> arguments) {
        validateRelationshipCreateArguments(arguments);
    }

    private Long extractRelationshipId(Map<String, Object> arguments) {
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
        
        // Map contact_is
        if (arguments.containsKey("contactIs")) {
            apiRequest.put("contact_is", arguments.get("contactIs"));
        }
        
        // Map of_contact  
        if (arguments.containsKey("ofContact")) {
            apiRequest.put("of_contact", arguments.get("ofContact"));
        }
        
        // Map relationship_type_id
        if (arguments.containsKey("relationshipTypeId")) {
            apiRequest.put("relationship_type_id", arguments.get("relationshipTypeId"));
        }
        
        // Map optional fields
        if (arguments.containsKey("notes")) {
            apiRequest.put("notes", arguments.get("notes"));
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

    private Map<String, Object> formatRelationshipResponse(Map<String, Object> apiResponse) {
        // Extract raw API data for complete field coverage
        Map<String, Object> rawApiData;
        Map<String, Object> relationshipData;
        
        if (apiResponse.containsKey("data")) {
            // Single relationship response
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            rawApiData = rawData; // Preserve original API data
            relationshipData = mapFromApiFormat(rawData);
        } else {
            rawApiData = apiResponse; // Preserve original API data
            relationshipData = mapFromApiFormat(apiResponse);
        }
        
        // Use raw API data as escaped JSON for complete field coverage as per Constitutional Principle VI
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        // Return both data and content fields for protocol compliance
        Map<String, Object> result = new HashMap<>();
        result.put("data", relationshipData);
        
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

    private Map<String, Object> formatRelationshipsListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> relationships = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedRelationships = relationships.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        // Format content as escaped JSON for Claude Desktop accessibility as per Constitutional Principle VI
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedRelationships);
        
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

    private Map<String, Object> mapFromApiFormat(Map<String, Object> apiData) {
        Map<String, Object> result = new HashMap<>();
        
        // Map snake_case to camelCase
        apiData.forEach((key, value) -> {
            switch (key) {
                case "contact_is" -> result.put("contactIs", value);
                case "of_contact" -> result.put("ofContact", value);
                case "relationship_type_id" -> result.put("relationshipTypeId", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}