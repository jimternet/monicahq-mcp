package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.*;

/**
 * Service for Compliance entity operations.
 * Note: Compliance API endpoints may not be clearly defined in all Monica versions.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class ComplianceService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> createCompliance(Map<String, Object> arguments) {
        log.info("Creating compliance record with arguments: {}", arguments);
        
        try {
            validateComplianceCreateArguments(arguments);
            Map<String, Object> apiRequest = mapToApiFormat(arguments);
            
            return monicaClient.post("/compliance", apiRequest)
                .map(this::formatComplianceResponse)
                .doOnSuccess(result -> log.info("Compliance record created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create compliance record: {}", error.getMessage()))
                .onErrorResume(error -> {
                    // Graceful handling for unclear/unavailable Compliance API
                    log.warn("Compliance API may not be available: {}", error.getMessage());
                    return Mono.error(new IllegalStateException(
                        "Compliance API is not available - endpoints may not be implemented in this Monica version: " 
                        + error.getMessage(), error));
                });
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for compliance creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getCompliance(Map<String, Object> arguments) {
        log.info("Getting compliance record with arguments: {}", arguments);
        
        try {
            Long complianceId = extractComplianceId(arguments);
            
            return monicaClient.get("/compliance/" + complianceId, null)
                .map(this::formatComplianceResponse)
                .doOnSuccess(result -> log.info("Compliance record retrieved successfully: {}", complianceId))
                .doOnError(error -> log.error("Failed to get compliance record {}: {}", complianceId, error.getMessage()))
                .onErrorResume(error -> {
                    log.warn("Compliance API may not be available: {}", error.getMessage());
                    return Mono.error(new IllegalStateException(
                        "Compliance API is not available: " + error.getMessage(), error));
                });
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for compliance retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateCompliance(Map<String, Object> arguments) {
        log.info("Updating compliance record with arguments: {}", arguments);
        
        try {
            Long complianceId = extractComplianceId(arguments);
            validateComplianceUpdateArguments(arguments);
            
            Map<String, Object> apiRequest = mapToApiFormat(arguments);
            apiRequest.remove("id"); // Remove ID from update payload
            
            return monicaClient.put("/compliance/" + complianceId, apiRequest)
                .map(this::formatComplianceResponse)
                .doOnSuccess(result -> log.info("Compliance record updated successfully: {}", complianceId))
                .doOnError(error -> log.error("Failed to update compliance record {}: {}", complianceId, error.getMessage()))
                .onErrorResume(error -> {
                    log.warn("Compliance API may not be available: {}", error.getMessage());
                    return Mono.error(new IllegalStateException(
                        "Compliance API is not available: " + error.getMessage(), error));
                });
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for compliance update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteCompliance(Map<String, Object> arguments) {
        log.info("Deleting compliance record with arguments: {}", arguments);
        
        try {
            Long complianceId = extractComplianceId(arguments);
            
            return monicaClient.delete("/compliance/" + complianceId)
                .map(response -> {
                    String formattedContent = contentFormatter.formatOperationResult(
                        "Delete", "Compliance", complianceId, true, 
                        "Compliance record with ID " + complianceId + " has been deleted successfully"
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
                .doOnSuccess(result -> log.info("Compliance record deleted successfully: {}", complianceId))
                .doOnError(error -> log.error("Failed to delete compliance record {}: {}", complianceId, error.getMessage()))
                .onErrorResume(error -> {
                    log.warn("Compliance API may not be available: {}", error.getMessage());
                    return Mono.error(new IllegalStateException(
                        "Compliance API is not available: " + error.getMessage(), error));
                });
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for compliance deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listCompliance(Map<String, Object> arguments) {
        log.info("Listing compliance records with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/compliance", queryParams)
                .map(this::formatComplianceListResponse)
                .doOnSuccess(result -> log.info("Compliance records listed successfully"))
                .doOnError(error -> log.error("Failed to list compliance records: {}", error.getMessage()))
                .onErrorResume(error -> {
                    log.warn("Compliance API may not be available: {}", error.getMessage());
                    return Mono.error(new IllegalStateException(
                        "Compliance API is not available: " + error.getMessage(), error));
                });
                
        } catch (Exception e) {
            log.error("Error building query parameters: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateComplianceCreateArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("Compliance creation arguments cannot be empty");
        }
        
        if (!arguments.containsKey("type") || 
            arguments.get("type") == null || 
            arguments.get("type").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("type is required - please provide a compliance type");
        }
    }
    
    private void validateComplianceUpdateArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("Compliance update arguments cannot be empty");
        }
    }

    private Long extractComplianceId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("id")) {
            throw new IllegalArgumentException("Compliance ID is required");
        }
        
        Object idValue = arguments.get("id");
        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }
        
        try {
            return Long.parseLong(idValue.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid compliance ID format: " + idValue);
        }
    }

    private Map<String, Object> mapToApiFormat(Map<String, Object> arguments) {
        Map<String, Object> apiRequest = new HashMap<>();
        
        // Map camelCase to snake_case for MonicaHQ API
        arguments.forEach((key, value) -> {
            switch (key) {
                case "contactId" -> apiRequest.put("contact_id", value);
                case "isActive" -> apiRequest.put("is_active", value);
                case "dataRetentionDays" -> apiRequest.put("data_retention_days", value);
                case "privacyLevel" -> apiRequest.put("privacy_level", value);
                case "consentGiven" -> apiRequest.put("consent_given", value);
                case "consentDate" -> apiRequest.put("consent_date", value);
                case "auditRequired" -> apiRequest.put("audit_required", value);
                default -> apiRequest.put(key, value);
            }
        });
        
        return apiRequest;
    }

    private Map<String, String> buildListQueryParams(Map<String, Object> arguments) {
        Map<String, String> queryParams = new HashMap<>();
        
        // Handle pagination
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
        
        // Handle filtering by contact
        if (arguments.containsKey("contactId") && arguments.get("contactId") != null) {
            queryParams.put("contact_id", arguments.get("contactId").toString());
        }
        
        return queryParams;
    }

    private Map<String, Object> formatComplianceResponse(Map<String, Object> apiResponse) {
        Map<String, Object> complianceData;
        Map<String, Object> rawApiData;
        
        if (apiResponse.containsKey("data")) {
            // Single compliance response
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            rawApiData = rawData;
            complianceData = mapFromApiFormat(rawData);
        } else {
            rawApiData = apiResponse;
            complianceData = mapFromApiFormat(apiResponse);
        }
        
        // Use raw API data as escaped JSON for complete field coverage
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", complianceData);
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> formatComplianceListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> complianceRecords = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedRecords = complianceRecords.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedRecords);
        
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
        
        // Map snake_case to camelCase
        apiData.forEach((key, value) -> {
            switch (key) {
                case "contact_id" -> result.put("contactId", value);
                case "is_active" -> result.put("isActive", value);
                case "data_retention_days" -> result.put("dataRetentionDays", value);
                case "privacy_level" -> result.put("privacyLevel", value);
                case "consent_given" -> result.put("consentGiven", value);
                case "consent_date" -> result.put("consentDate", value);
                case "audit_required" -> result.put("auditRequired", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}