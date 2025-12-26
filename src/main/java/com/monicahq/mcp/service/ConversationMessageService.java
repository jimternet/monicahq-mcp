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
public class ConversationMessageService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> createConversationMessage(Map<String, Object> arguments) {
        log.info("Creating conversation message with arguments: {}", arguments);
        
        try {
            validateConversationMessageCreateArguments(arguments);
            Long conversationId = extractConversationId(arguments);
            Map<String, Object> apiRequest = mapToApiFormat(arguments);
            
            return monicaClient.post("/conversations/" + conversationId + "/messages", apiRequest)
                .map(this::formatConversationMessageResponse)
                .doOnSuccess(result -> log.info("Conversation message created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create conversation message: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for conversation message creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getConversationMessage(Map<String, Object> arguments) {
        log.info("Getting conversation message with arguments: {}", arguments);
        
        try {
            Long conversationId = extractConversationId(arguments);
            Long messageId = extractMessageId(arguments);
            
            return monicaClient.get("/conversations/" + conversationId + "/messages/" + messageId, null)
                .map(this::formatConversationMessageResponse)
                .doOnSuccess(result -> log.info("Conversation message retrieved successfully: {}", messageId))
                .doOnError(error -> log.error("Failed to get conversation message {}: {}", messageId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for conversation message retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateConversationMessage(Map<String, Object> arguments) {
        log.info("Updating conversation message with arguments: {}", arguments);
        
        try {
            Long conversationId = extractConversationId(arguments);
            Long messageId = extractMessageId(arguments);
            
            Map<String, Object> updateData = new HashMap<>(arguments);
            updateData.remove("id");
            updateData.remove("conversationId");
            
            Map<String, Object> apiRequest = mapToApiFormat(updateData);
            
            return monicaClient.put("/conversations/" + conversationId + "/messages/" + messageId, apiRequest)
                .map(this::formatConversationMessageResponse)
                .doOnSuccess(result -> log.info("Conversation message updated successfully: {}", messageId))
                .doOnError(error -> log.error("Failed to update conversation message {}: {}", messageId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for conversation message update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteConversationMessage(Map<String, Object> arguments) {
        log.info("Deleting conversation message with arguments: {}", arguments);
        
        try {
            Long conversationId = extractConversationId(arguments);
            Long messageId = extractMessageId(arguments);
            
            return monicaClient.delete("/conversations/" + conversationId + "/messages/" + messageId)
                .map(response -> {
                    String formattedContent = contentFormatter.formatOperationResult(
                        "Delete", "Conversation Message", messageId, true, 
                        "Message has been permanently removed from conversation"
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
                .doOnSuccess(result -> log.info("Conversation message deleted successfully: {}", messageId))
                .doOnError(error -> log.error("Failed to delete conversation message {}: {}", messageId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for conversation message deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Lists conversation messages by extracting from the conversation GET response.
     *
     * WORKAROUND: GET /conversations/{id}/messages returns HTTP 405 (Method Not Allowed),
     * so we fetch the full conversation via GET /conversations/{id} and extract the
     * messages from the "data.messages" field in the response.
     *
     * See docs/API-LIMITATIONS.md for details on MonicaHQ API limitations.
     */
    public Mono<Map<String, Object>> listConversationMessages(Map<String, Object> arguments) {
        log.info("Listing conversation messages with arguments: {}", arguments);

        try {
            Long conversationId = extractConversationId(arguments);

            // WORKAROUND: Use GET /conversations/{id} instead of GET /conversations/{id}/messages
            // The /messages endpoint returns HTTP 405, but messages are included in the conversation response
            return monicaClient.get("/conversations/" + conversationId, null)
                .map(this::extractMessagesFromConversation)
                .doOnSuccess(result -> log.info("Conversation messages listed successfully"))
                .doOnError(error -> log.error("Failed to list conversation messages: {}", error.getMessage()));

        } catch (Exception e) {
            log.error("Error extracting conversation messages: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Extracts messages from a conversation GET response.
     * The messages are nested under data.messages in the API response.
     */
    @SuppressWarnings("unchecked")
    private Map<String, Object> extractMessagesFromConversation(Map<String, Object> apiResponse) {
        // Extract the data object from the response
        Map<String, Object> data = (Map<String, Object>) apiResponse.get("data");

        // Extract messages array - handle null/missing messages gracefully
        List<Map<String, Object>> messages = Collections.emptyList();
        if (data != null && data.containsKey("messages")) {
            Object messagesObj = data.get("messages");
            if (messagesObj instanceof List) {
                messages = (List<Map<String, Object>>) messagesObj;
            }
        }

        // Convert each message to consistent format
        List<Map<String, Object>> formattedMessages = messages.stream()
            .map(this::mapFromApiFormat)
            .toList();

        // Build synthetic API response structure for formatting
        Map<String, Object> syntheticResponse = new HashMap<>();
        syntheticResponse.put("data", messages);
        syntheticResponse.put("meta", Map.of("total", messages.size()));

        // Format content for Claude Desktop visibility
        String formattedContent = contentFormatter.formatListAsEscapedJson(syntheticResponse);

        // Return both data and content fields for protocol compliance
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedMessages);
        result.put("meta", Map.of(
            "total", messages.size(),
            "source", "extracted_from_conversation"
        ));

        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);

        return result;
    }

    private void validateConversationMessageCreateArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("Conversation message creation arguments cannot be empty");
        }
        
        if (!arguments.containsKey("conversationId") || arguments.get("conversationId") == null) {
            throw new IllegalArgumentException("conversationId is required");
        }
        
        if (!arguments.containsKey("writtenAt") || arguments.get("writtenAt") == null) {
            throw new IllegalArgumentException("writtenAt is required");
        }
        
        if (!arguments.containsKey("writtenByMe") || arguments.get("writtenByMe") == null) {
            throw new IllegalArgumentException("writtenByMe is required");
        }
    }

    private Long extractConversationId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("conversationId")) {
            throw new IllegalArgumentException("Conversation ID is required");
        }
        
        Object idValue = arguments.get("conversationId");
        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }
        
        try {
            return Long.parseLong(idValue.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid conversation ID format: " + idValue);
        }
    }

    private Long extractMessageId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("id")) {
            throw new IllegalArgumentException("Message ID is required");
        }
        
        Object idValue = arguments.get("id");
        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }
        
        try {
            return Long.parseLong(idValue.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid message ID format: " + idValue);
        }
    }

    private Map<String, Object> mapToApiFormat(Map<String, Object> arguments) {
        Map<String, Object> apiRequest = new HashMap<>();
        
        arguments.forEach((key, value) -> {
            switch (key) {
                case "writtenAt" -> apiRequest.put("written_at", value);
                case "writtenByMe" -> apiRequest.put("written_by_me", value);
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

    private Map<String, Object> formatConversationMessageResponse(Map<String, Object> apiResponse) {
        Map<String, Object> messageData;
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            messageData = mapFromApiFormat(rawData);
        } else {
            messageData = mapFromApiFormat(apiResponse);
        }
        
        // Use raw API data for complete field coverage as per Constitutional Principle VI
        @SuppressWarnings("unchecked")
        Map<String, Object> rawApiData = apiResponse.containsKey("data") ? 
            (Map<String, Object>) apiResponse.get("data") : apiResponse;
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        // Return both data and content fields for protocol compliance
        Map<String, Object> result = new HashMap<>();
        result.put("data", messageData);
        
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

    private Map<String, Object> formatConversationMessageListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> messages = (List<Map<String, Object>>) apiResponse.get("data");
        
        // Convert each message to consistent format
        List<Map<String, Object>> formattedMessages = messages.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        // Format content for Claude Desktop visibility
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        // Extract meta for result structure
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) apiResponse.get("meta");
        
        // Return both data and content fields for protocol compliance
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedMessages);
        
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
                case "written_at" -> result.put("writtenAt", value);
                case "written_by_me" -> result.put("writtenByMe", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
    
}