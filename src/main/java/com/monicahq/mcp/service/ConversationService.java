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
public class ConversationService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> createConversation(Map<String, Object> arguments) {
        log.info("Creating conversation with arguments: {}", arguments);
        
        try {
            validateConversationCreateArguments(arguments);
            Map<String, Object> apiRequest = mapToApiFormat(arguments);
            
            return monicaClient.post("/conversations", apiRequest)
                .map(this::formatConversationResponse)
                .doOnSuccess(result -> log.info("Conversation created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create conversation: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for conversation creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getConversation(Map<String, Object> arguments) {
        log.info("Getting conversation with arguments: {}", arguments);
        
        try {
            Long conversationId = extractConversationId(arguments);
            
            return monicaClient.get("/conversations/" + conversationId, null)
                .map(this::formatConversationResponse)
                .doOnSuccess(result -> log.info("Conversation retrieved successfully: {}", conversationId))
                .doOnError(error -> log.error("Failed to get conversation {}: {}", conversationId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for conversation retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateConversation(Map<String, Object> arguments) {
        log.info("Updating conversation with arguments: {}", arguments);
        
        try {
            Long conversationId = extractConversationId(arguments);
            
            Map<String, Object> updateData = new HashMap<>(arguments);
            updateData.remove("id");
            
            Map<String, Object> apiRequest = mapToApiFormat(updateData);
            
            return monicaClient.put("/conversations/" + conversationId, apiRequest)
                .map(this::formatConversationResponse)
                .doOnSuccess(result -> log.info("Conversation updated successfully: {}", conversationId))
                .doOnError(error -> log.error("Failed to update conversation {}: {}", conversationId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for conversation update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteConversation(Map<String, Object> arguments) {
        log.info("Deleting conversation with arguments: {}", arguments);
        
        try {
            Long conversationId = extractConversationId(arguments);
            
            return monicaClient.delete("/conversations/" + conversationId)
                .map(response -> {
                    String formattedContent = contentFormatter.formatOperationResult(
                        "Delete", "Conversation", conversationId, true, 
                        "Conversation has been permanently removed"
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
                .doOnSuccess(result -> log.info("Conversation deleted successfully: {}", conversationId))
                .doOnError(error -> log.error("Failed to delete conversation {}: {}", conversationId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for conversation deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listConversations(Map<String, Object> arguments) {
        log.info("Listing conversations with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/conversations", queryParams)
                .map(this::formatConversationListResponse)
                .doOnSuccess(result -> log.info("Conversations listed successfully"))
                .doOnError(error -> log.error("Failed to list conversations: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error building query parameters: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateConversationCreateArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("Conversation creation arguments cannot be empty");
        }
        
        if (!arguments.containsKey("contactId") || arguments.get("contactId") == null) {
            throw new IllegalArgumentException("contactId is required");
        }
        
        if (!arguments.containsKey("happenedAt") || arguments.get("happenedAt") == null) {
            throw new IllegalArgumentException("happenedAt is required");
        }
    }

    private Long extractConversationId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("id")) {
            throw new IllegalArgumentException("Conversation ID is required");
        }
        
        Object idValue = arguments.get("id");
        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }
        
        try {
            return Long.parseLong(idValue.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid conversation ID format: " + idValue);
        }
    }

    private Map<String, Object> mapToApiFormat(Map<String, Object> arguments) {
        Map<String, Object> apiRequest = new HashMap<>();
        
        arguments.forEach((key, value) -> {
            switch (key) {
                case "contactId" -> apiRequest.put("contact_id", value);
                case "happenedAt" -> apiRequest.put("happened_at", value);
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

    private Map<String, Object> formatConversationResponse(Map<String, Object> apiResponse) {
        Map<String, Object> conversationData;
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            conversationData = mapFromApiFormat(rawData);
        } else {
            conversationData = mapFromApiFormat(apiResponse);
        }
        
        // Use raw API data for complete field coverage as per Constitutional Principle VI
        @SuppressWarnings("unchecked")
        Map<String, Object> rawApiData = apiResponse.containsKey("data") ? 
            (Map<String, Object>) apiResponse.get("data") : apiResponse;
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        // Return both data and content fields for protocol compliance
        Map<String, Object> result = new HashMap<>();
        result.put("data", conversationData);
        
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

    private Map<String, Object> formatConversationListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> conversations = (List<Map<String, Object>>) apiResponse.get("data");
        
        // Convert each conversation to consistent format
        List<Map<String, Object>> formattedConversations = conversations.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        // Format content for Claude Desktop visibility
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        // Extract meta for result structure
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) apiResponse.get("meta");
        
        // Return both data and content fields for protocol compliance
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedConversations);
        
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
                case "contact_id" -> result.put("contactId", value);
                case "happened_at" -> result.put("happenedAt", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
    
}