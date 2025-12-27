package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.ConversationMessageFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.*;

/**
 * Service for managing Conversation Message entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit field mapping and response formatting utilities.
 * Uses {@link ConversationMessageFieldMappingConfig} for field mappings and validation rules.
 * </p>
 * <p>
 * This service handles nested resources under conversations with endpoints like:
 * <ul>
 *   <li>POST /conversations/{conversationId}/messages</li>
 *   <li>GET /conversations/{conversationId}/messages/{id}</li>
 *   <li>PUT /conversations/{conversationId}/messages/{id}</li>
 *   <li>DELETE /conversations/{conversationId}/messages/{id}</li>
 * </ul>
 * </p>
 * <p>
 * IMPORTANT: The listConversationMessages method uses a workaround because
 * GET /conversations/{id}/messages returns HTTP 405 (Method Not Allowed).
 * Instead, it fetches the conversation and extracts messages from the response.
 * </p>
 */
@Service
@Slf4j
public class ConversationMessageService extends AbstractCrudService<Object> {

    private final ConversationMessageFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a ConversationMessageService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Conversation Messages
     */
    public ConversationMessageService(MonicaHqClient monicaClient,
                                      ContentFormatter contentFormatter,
                                      ConversationMessageFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new conversation message.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>conversationId - The ID of the parent conversation</li>
     *   <li>writtenAt - The date/time when the message was written</li>
     *   <li>writtenByMe - Whether the message was written by the user</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>content - The message content</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created message data
     */
    public Mono<Map<String, Object>> createConversationMessage(Map<String, Object> arguments) {
        log.info("Creating conversation message with arguments: {}", arguments);

        try {
            validateRequiredFields(arguments, fieldMappingConfig.getRequiredCreateFields());
            Long conversationId = extractConversationId(arguments);
            Map<String, Object> apiRequest = mapToApiFormat(arguments);

            String endpoint = "/conversations/" + conversationId + "/messages";

            return monicaClient.post(endpoint, apiRequest)
                .map(this::formatSingleResponse)
                .doOnSuccess(result -> log.info("Conversation message created successfully"))
                .doOnError(error -> log.error("Failed to create conversation message: {}", error.getMessage()));

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for conversation message creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Retrieves a conversation message by its ID.
     *
     * @param arguments map containing "conversationId" and "id"
     * @return a Mono containing the message data
     */
    public Mono<Map<String, Object>> getConversationMessage(Map<String, Object> arguments) {
        log.info("Getting conversation message with arguments: {}", arguments);

        try {
            Long conversationId = extractConversationId(arguments);
            Long messageId = extractMessageId(arguments);

            String endpoint = "/conversations/" + conversationId + "/messages/" + messageId;

            return monicaClient.get(endpoint, null)
                .map(this::formatSingleResponse)
                .doOnSuccess(result -> log.info("Conversation message retrieved successfully: {}", messageId))
                .doOnError(error -> log.error("Failed to get conversation message {}: {}", messageId, error.getMessage()));

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for conversation message retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Updates an existing conversation message.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>conversationId - The ID of the parent conversation</li>
     *   <li>id - The ID of the message to update</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>content - New message content</li>
     *   <li>writtenAt - New written date/time</li>
     *   <li>writtenByMe - New written-by-me status</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments
     * @return a Mono containing the updated message data
     */
    public Mono<Map<String, Object>> updateConversationMessage(Map<String, Object> arguments) {
        log.info("Updating conversation message with arguments: {}", arguments);

        try {
            Long conversationId = extractConversationId(arguments);
            Long messageId = extractMessageId(arguments);

            Map<String, Object> updateData = new HashMap<>(arguments);
            updateData.remove("id");
            updateData.remove("conversationId");

            Map<String, Object> apiRequest = mapToApiFormat(updateData);

            String endpoint = "/conversations/" + conversationId + "/messages/" + messageId;

            return monicaClient.put(endpoint, apiRequest)
                .map(this::formatSingleResponse)
                .doOnSuccess(result -> log.info("Conversation message updated successfully: {}", messageId))
                .doOnError(error -> log.error("Failed to update conversation message {}: {}", messageId, error.getMessage()));

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for conversation message update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Deletes a conversation message by its ID.
     *
     * @param arguments map containing "conversationId" and "id"
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteConversationMessage(Map<String, Object> arguments) {
        log.info("Deleting conversation message with arguments: {}", arguments);

        try {
            Long conversationId = extractConversationId(arguments);
            Long messageId = extractMessageId(arguments);

            String endpoint = "/conversations/" + conversationId + "/messages/" + messageId;

            return monicaClient.delete(endpoint)
                .map(response -> formatNestedDeleteResponse(messageId))
                .doOnSuccess(result -> log.info("Conversation message deleted successfully: {}", messageId))
                .doOnError(error -> log.error("Failed to delete conversation message {}: {}", messageId, error.getMessage()));

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for conversation message deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Lists conversation messages by extracting from the conversation GET response.
     * <p>
     * WORKAROUND: GET /conversations/{id}/messages returns HTTP 405 (Method Not Allowed),
     * so we fetch the full conversation via GET /conversations/{id} and extract the
     * messages from the "data.messages" field in the response.
     * </p>
     * <p>
     * See docs/API-LIMITATIONS.md for details on MonicaHQ API limitations.
     * </p>
     *
     * @param arguments map containing "conversationId"
     * @return a Mono containing the list of messages with metadata
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
     * Extracts and formats the conversationId from arguments.
     *
     * @param arguments the arguments map containing conversationId
     * @return the conversation ID as a Long
     * @throws IllegalArgumentException if conversationId is missing or invalid
     */
    private Long extractConversationId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("conversationId")) {
            throw new IllegalArgumentException("Conversation ID is required");
        }

        Object idValue = arguments.get("conversationId");
        if (idValue == null) {
            throw new IllegalArgumentException("Conversation ID is required");
        }

        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }

        try {
            return Long.parseLong(idValue.toString().trim());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid conversation ID format: " + idValue);
        }
    }

    /**
     * Extracts and formats the message ID from arguments.
     *
     * @param arguments the arguments map containing id
     * @return the message ID as a Long
     * @throws IllegalArgumentException if id is missing or invalid
     */
    private Long extractMessageId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("id")) {
            throw new IllegalArgumentException("Message ID is required");
        }

        Object idValue = arguments.get("id");
        if (idValue == null) {
            throw new IllegalArgumentException("Message ID is required");
        }

        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }

        try {
            return Long.parseLong(idValue.toString().trim());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid message ID format: " + idValue);
        }
    }

    /**
     * Extracts messages from a conversation GET response.
     * The messages are nested under data.messages in the API response.
     *
     * @param apiResponse the API response from GET /conversations/{id}
     * @return formatted response containing extracted messages
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

        // Convert each message to consistent format using inherited mapFromApiFormat
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

    /**
     * Formats a delete response for nested resources.
     * Uses a custom message since the entity name includes the parent context.
     *
     * @param messageId the ID of the deleted message
     * @return formatted delete response
     */
    private Map<String, Object> formatNestedDeleteResponse(Long messageId) {
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
    }
}
