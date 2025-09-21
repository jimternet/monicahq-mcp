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
public class ContactTagService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> attachTag(Map<String, Object> arguments) {
        log.info("Attaching tag to contact with arguments: {}", arguments);
        
        try {
            validateContactTagCreateArguments(arguments);
            Long contactId = extractContactId(arguments);
            Map<String, Object> apiRequest = mapToApiFormat(arguments);
            
            return monicaClient.post("/contacts/" + contactId + "/setTags", apiRequest)
                .map(this::formatContactTagResponse)
                .doOnSuccess(result -> log.info("Tag attached to contact successfully: {}", result))
                .doOnError(error -> log.error("Failed to attach tag to contact: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact tag attachment: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getContactTags(Map<String, Object> arguments) {
        log.info("Getting contact tags with arguments: {}", arguments);
        
        try {
            Long contactId = extractContactId(arguments);
            
            return monicaClient.get("/contacts/" + contactId + "/tags", null)
                .map(this::formatContactTagListResponse)
                .doOnSuccess(result -> log.info("Contact tags retrieved successfully: {}", contactId))
                .doOnError(error -> log.error("Failed to get contact tags for {}: {}", contactId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact tag retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateContactTags(Map<String, Object> arguments) {
        log.info("Updating contact tags with arguments: {}", arguments);
        
        try {
            Long contactId = extractContactId(arguments);
            Map<String, Object> apiRequest = mapToApiFormat(arguments);
            
            return monicaClient.put("/contacts/" + contactId + "/setTags", apiRequest)
                .map(this::formatContactTagResponse)
                .doOnSuccess(result -> log.info("Contact tags updated successfully: {}", contactId))
                .doOnError(error -> log.error("Failed to update contact tags for {}: {}", contactId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact tag update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> detachTag(Map<String, Object> arguments) {
        log.info("Detaching tag from contact with arguments: {}", arguments);
        
        try {
            Long contactId = extractContactId(arguments);
            Long tagId = extractTagId(arguments);
            
            return monicaClient.delete("/contacts/" + contactId + "/unsetTag/" + tagId)
                .map(response -> {
                    String formattedContent = contentFormatter.formatOperationResult(
                        "Detach", "Contact Tag", tagId, true, 
                        "Tag has been successfully detached from contact " + contactId
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
                .doOnSuccess(result -> log.info("Tag detached from contact successfully: contact {} tag {}", contactId, tagId))
                .doOnError(error -> log.error("Failed to detach tag {} from contact {}: {}", tagId, contactId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact tag detachment: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listContactsByTag(Map<String, Object> arguments) {
        log.info("Listing contacts by tag with arguments: {}", arguments);
        
        try {
            Long tagId = extractTagId(arguments);
            Map<String, String> queryParams = buildListQueryParams(arguments);
            queryParams.put("tags", tagId.toString());
            
            return monicaClient.get("/contacts", queryParams)
                .map(this::formatContactTagListResponse)
                .doOnSuccess(result -> log.info("Contacts listed by tag successfully"))
                .doOnError(error -> log.error("Failed to list contacts by tag: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error building query parameters: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateContactTagCreateArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("Contact tag creation arguments cannot be empty");
        }
        
        if (!arguments.containsKey("contactId") || arguments.get("contactId") == null) {
            throw new IllegalArgumentException("contactId is required");
        }
        
        if (!arguments.containsKey("tagId") || arguments.get("tagId") == null) {
            throw new IllegalArgumentException("tagId is required");
        }
    }

    private Long extractContactId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("contactId")) {
            throw new IllegalArgumentException("Contact ID is required");
        }
        
        Object idValue = arguments.get("contactId");
        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }
        
        try {
            return Long.parseLong(idValue.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid contact ID format: " + idValue);
        }
    }

    private Long extractTagId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("tagId")) {
            throw new IllegalArgumentException("Tag ID is required");
        }
        
        Object idValue = arguments.get("tagId");
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
        
        // Convert tagId to tags array format for MonicaHQ API
        if (arguments.containsKey("tagId")) {
            apiRequest.put("tags", List.of(arguments.get("tagId")));
        }
        
        // Add other fields as needed
        arguments.forEach((key, value) -> {
            if (!"tagId".equals(key) && !"contactId".equals(key)) {
                apiRequest.put(key, value);
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
        
        return queryParams;
    }

    private Map<String, Object> formatContactTagResponse(Map<String, Object> apiResponse) {
        // Use raw API data for complete field coverage as per Constitutional Principle VI
        @SuppressWarnings("unchecked")
        Map<String, Object> rawApiData = apiResponse.containsKey("data") ? 
            (Map<String, Object>) apiResponse.get("data") : apiResponse;
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", apiResponse);
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        return result;
    }

    private Map<String, Object> formatContactTagListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) apiResponse.get("data");
        if (data == null) {
            data = List.of();
        }
        
        // Extract meta for result structure
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) apiResponse.get("meta");
        
        // Format content for Claude Desktop visibility
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        // Return both data and content fields for protocol compliance
        Map<String, Object> result = new HashMap<>();
        result.put("data", data);
        
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
    
}