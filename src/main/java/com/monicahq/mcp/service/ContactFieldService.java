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
public class ContactFieldService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> createContactField(Map<String, Object> arguments) {
        log.info("Creating contact field with arguments: {}", arguments);
        
        try {
            validateContactFieldCreateArguments(arguments);
            Long contactId = extractContactId(arguments);
            Map<String, Object> apiRequest = mapToApiFormat(arguments);
            
            return monicaClient.post("/contactfields", apiRequest)
                .map(this::formatContactFieldResponse)
                .doOnSuccess(result -> log.info("Contact field created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create contact field: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact field creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getContactField(Map<String, Object> arguments) {
        log.info("Getting contact field with arguments: {}", arguments);
        
        try {
            Long fieldId = extractFieldId(arguments);
            
            return monicaClient.get("/contactfields/" + fieldId, null)
                .map(this::formatContactFieldResponse)
                .doOnSuccess(result -> log.info("Contact field retrieved successfully: {}", fieldId))
                .doOnError(error -> log.error("Failed to get contact field {}: {}", fieldId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact field retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateContactField(Map<String, Object> arguments) {
        log.info("Updating contact field with arguments: {}", arguments);
        
        try {
            Long fieldId = extractFieldId(arguments);
            
            Map<String, Object> updateData = new HashMap<>(arguments);
            updateData.remove("id");
            // Keep contactId for API request
            
            Map<String, Object> apiRequest = mapToApiFormat(updateData);
            
            return monicaClient.put("/contactfields/" + fieldId, apiRequest)
                .map(this::formatContactFieldResponse)
                .doOnSuccess(result -> log.info("Contact field updated successfully: {}", fieldId))
                .doOnError(error -> log.error("Failed to update contact field {}: {}", fieldId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact field update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteContactField(Map<String, Object> arguments) {
        log.info("Deleting contact field with arguments: {}", arguments);
        
        try {
            Long fieldId = extractFieldId(arguments);
            
            return monicaClient.delete("/contactfields/" + fieldId)
                .map(response -> {
                    String formattedContent = contentFormatter.formatOperationResult(
                        "Delete", "Contact Field", fieldId, true, 
                        "Field has been permanently removed from contact"
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
                .doOnSuccess(result -> log.info("Contact field deleted successfully: {}", fieldId))
                .doOnError(error -> log.error("Failed to delete contact field {}: {}", fieldId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact field deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listContactFields(Map<String, Object> arguments) {
        log.info("Listing contact fields with arguments: {}", arguments);
        
        try {
            Long contactId = extractContactId(arguments);
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/contact/" + contactId + "/contactfields", queryParams)
                .map(this::formatContactFieldListResponse)
                .doOnSuccess(result -> log.info("Contact fields listed successfully"))
                .doOnError(error -> log.error("Failed to list contact fields: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error building query parameters: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateContactFieldCreateArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("Contact field creation arguments cannot be empty");
        }
        
        if (!arguments.containsKey("contactId") || arguments.get("contactId") == null) {
            throw new IllegalArgumentException("contactId is required");
        }
        
        if (!arguments.containsKey("contactFieldTypeId") || arguments.get("contactFieldTypeId") == null) {
            throw new IllegalArgumentException("contactFieldTypeId is required");
        }
        
        if (!arguments.containsKey("data") || 
            arguments.get("data") == null || 
            arguments.get("data").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("data is required");
        }
    }

    private Long extractContactId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("contactId")) {
            throw new IllegalArgumentException("contactId is required - please provide the ID of an existing contact to list its fields");
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

    private Long extractFieldId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("id")) {
            throw new IllegalArgumentException("Contact field ID is required");
        }
        
        Object idValue = arguments.get("id");
        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }
        
        try {
            return Long.parseLong(idValue.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid contact field ID format: " + idValue);
        }
    }

    private Map<String, Object> mapToApiFormat(Map<String, Object> arguments) {
        Map<String, Object> apiRequest = new HashMap<>();
        
        arguments.forEach((key, value) -> {
            switch (key) {
                case "contactFieldTypeId" -> apiRequest.put("contact_field_type_id", value);
                case "contactId" -> apiRequest.put("contact_id", value);
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
        
        return queryParams;
    }

    private Map<String, Object> formatContactFieldResponse(Map<String, Object> apiResponse) {
        Map<String, Object> fieldData;
        if (apiResponse.containsKey("data")) {
            // Single field response
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            fieldData = mapFromApiFormat(rawData);
        } else {
            fieldData = mapFromApiFormat(apiResponse);
        }
        
        // Use raw API data for complete field coverage as per Constitutional Principle VI
        @SuppressWarnings("unchecked")
        Map<String, Object> rawApiData = apiResponse.containsKey("data") ? 
            (Map<String, Object>) apiResponse.get("data") : apiResponse;
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        // Return both data and content fields for protocol compliance
        Map<String, Object> result = new HashMap<>();
        result.put("data", fieldData);
        
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

    private Map<String, Object> formatContactFieldListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> fields = (List<Map<String, Object>>) apiResponse.get("data");
        
        // Convert each field to consistent format
        List<Map<String, Object>> formattedFields = fields.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        // Format content for Claude Desktop visibility
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        // Extract meta for result structure
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) apiResponse.get("meta");
        
        // Return both data and content fields for protocol compliance
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedFields);
        
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
                case "contact_field_type_id" -> result.put("contactFieldTypeId", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
    
}