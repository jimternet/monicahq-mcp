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
public class CallService {

    private final MonicaHqClient monicaClient;

    public Mono<Map<String, Object>> createCall(Map<String, Object> arguments) {
        log.info("Creating call with arguments: {}", arguments);
        
        try {
            validateCallCreateArguments(arguments);
            Map<String, Object> apiRequest = mapToApiFormat(arguments);
            
            return monicaClient.post("/calls", apiRequest)
                .map(this::formatCallResponse)
                .doOnSuccess(result -> log.info("Call created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create call: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for call creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getCall(Map<String, Object> arguments) {
        log.info("Getting call with arguments: {}", arguments);
        
        try {
            Long callId = extractCallId(arguments);
            
            return monicaClient.get("/calls/" + callId, null)
                .map(this::formatCallResponse)
                .doOnSuccess(result -> log.info("Call retrieved successfully: {}", callId))
                .doOnError(error -> log.error("Failed to get call {}: {}", callId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for call retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateCall(Map<String, Object> arguments) {
        log.info("Updating call with arguments: {}", arguments);
        
        try {
            Long callId = extractCallId(arguments);
            
            Map<String, Object> updateData = new HashMap<>(arguments);
            updateData.remove("id");
            
            Map<String, Object> apiRequest = mapToApiFormat(updateData);
            
            return monicaClient.put("/calls/" + callId, apiRequest)
                .map(this::formatCallResponse)
                .doOnSuccess(result -> log.info("Call updated successfully: {}", callId))
                .doOnError(error -> log.error("Failed to update call {}: {}", callId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for call update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteCall(Map<String, Object> arguments) {
        log.info("Deleting call with arguments: {}", arguments);
        
        try {
            Long callId = extractCallId(arguments);
            
            return monicaClient.delete("/calls/" + callId)
                .map(response -> {
                    Map<String, Object> result = new HashMap<>();
                    List<Map<String, Object>> content = List.of(
                        Map.of(
                            "type", "text",
                            "text", "Call with ID " + callId + " has been deleted successfully"
                        )
                    );
                    result.put("content", content);
                    return result;
                })
                .doOnSuccess(result -> log.info("Call deleted successfully: {}", callId))
                .doOnError(error -> log.error("Failed to delete call {}: {}", callId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for call deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listCalls(Map<String, Object> arguments) {
        log.info("Listing calls with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/calls", queryParams)
                .map(this::formatCallListResponse)
                .doOnSuccess(result -> log.info("Calls listed successfully"))
                .doOnError(error -> log.error("Failed to list calls: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error building query parameters: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateCallCreateArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("Call creation arguments cannot be empty");
        }
        
        if (!arguments.containsKey("contactId") || arguments.get("contactId") == null) {
            throw new IllegalArgumentException("contactId is required");
        }
        
        if (!arguments.containsKey("calledAt") || 
            arguments.get("calledAt") == null || 
            arguments.get("calledAt").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("calledAt is required");
        }
        
        // Validate duration if provided
        if (arguments.containsKey("durationInMinutes")) {
            Object durationObj = arguments.get("durationInMinutes");
            if (durationObj instanceof Number duration) {
                int durationMinutes = duration.intValue();
                if (durationMinutes < 0) {
                    throw new IllegalArgumentException("Duration cannot be negative");
                }
                if (durationMinutes > 1440) { // 24 hours = 1440 minutes
                    throw new IllegalArgumentException("Duration cannot exceed 24 hours (1440 minutes)");
                }
            }
        }
    }

    private Long extractCallId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("id")) {
            throw new IllegalArgumentException("Call ID is required");
        }
        
        Object idValue = arguments.get("id");
        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }
        
        try {
            return Long.parseLong(idValue.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid call ID format: " + idValue);
        }
    }

    private Map<String, Object> mapToApiFormat(Map<String, Object> arguments) {
        Map<String, Object> apiRequest = new HashMap<>();
        
        arguments.forEach((key, value) -> {
            switch (key) {
                case "contactId" -> apiRequest.put("contact_id", value);
                case "calledAt" -> apiRequest.put("called_at", value);
                case "durationInMinutes" -> apiRequest.put("duration", value);
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
        
        if (arguments.containsKey("contactId") && arguments.get("contactId") != null) {
            queryParams.put("contact_id", arguments.get("contactId").toString());
        }
        
        return queryParams;
    }

    private Map<String, Object> formatCallResponse(Map<String, Object> apiResponse) {
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> callData = (Map<String, Object>) apiResponse.get("data");
            return Map.of(
                "data", mapFromApiFormat(callData)
            );
        }
        
        return Map.of("data", mapFromApiFormat(apiResponse));
    }

    private Map<String, Object> formatCallListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> calls = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedCalls = calls.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedCalls);
        
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
                case "contact_id" -> result.put("contactId", value);
                case "called_at" -> result.put("calledAt", value);
                case "duration" -> result.put("durationInMinutes", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}