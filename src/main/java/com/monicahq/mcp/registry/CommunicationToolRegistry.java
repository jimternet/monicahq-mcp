package com.monicahq.mcp.registry;

import com.monicahq.mcp.service.CallService;
import com.monicahq.mcp.service.ConversationMessageService;
import com.monicahq.mcp.service.ConversationService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Domain-specific tool registry for Communication operations.
 *
 * This registry handles communication-related MCP tools:
 * - Call CRUD operations (create, get, update, delete, list)
 * - Conversation CRUD operations (create, get, update, delete, list)
 * - Conversation Message CRUD operations (create, get, update, delete, list)
 *
 * Calls represent phone call records with contacts.
 * Conversations track text-based communication threads.
 * Conversation Messages are individual messages within conversations.
 *
 * The registry delegates execution to:
 * - CallService for call operations
 * - ConversationService for conversation operations
 * - ConversationMessageService for conversation message operations
 */
@Component
@Slf4j
public class CommunicationToolRegistry extends AbstractDomainToolRegistry {

    private static final String DOMAIN = "Communication";
    private static final String CATEGORY = "Activity & Communication";

    private final CallService callService;
    private final ConversationService conversationService;
    private final ConversationMessageService conversationMessageService;

    public CommunicationToolRegistry(
            CallService callService,
            ConversationService conversationService,
            ConversationMessageService conversationMessageService) {
        this.callService = callService;
        this.conversationService = conversationService;
        this.conversationMessageService = conversationMessageService;
    }

    @Override
    public String getDomain() {
        return DOMAIN;
    }

    @Override
    protected void initializeTools() {
        // Call CRUD operations (5)
        registerTool(
            "call_create",
            "[Call] Create a new call record",
            createCallSchema(),
            CATEGORY
        );

        registerTool(
            "call_get",
            "[Call] Get a call by ID",
            createIdSchema("Call ID"),
            CATEGORY
        );

        registerTool(
            "call_update",
            "[Call] Update an existing call",
            createCallUpdateSchema(),
            CATEGORY
        );

        registerTool(
            "call_delete",
            "[Call] Delete a call",
            createIdSchema("Call ID"),
            CATEGORY
        );

        registerTool(
            "call_list",
            "[Call] List calls with pagination",
            createListSchema(),
            CATEGORY
        );

        // Conversation CRUD operations (5)
        registerTool(
            "conversation_create",
            "[Conversation] Create a new conversation",
            createConversationSchema(),
            CATEGORY
        );

        registerTool(
            "conversation_get",
            "[Conversation] Get a conversation by ID",
            createIdSchema("Conversation ID"),
            CATEGORY
        );

        registerTool(
            "conversation_update",
            "[Conversation] Update an existing conversation",
            createConversationUpdateSchema(),
            CATEGORY
        );

        registerTool(
            "conversation_delete",
            "[Conversation] Delete a conversation",
            createIdSchema("Conversation ID"),
            CATEGORY
        );

        registerTool(
            "conversation_list",
            "[Conversation] List conversations with pagination",
            createListSchema(),
            CATEGORY
        );

        // Conversation Message CRUD operations (5)
        registerTool(
            "conversation_message_create",
            "[Message] Create a new conversation message",
            createMessageSchema(),
            CATEGORY
        );

        registerTool(
            "conversation_message_get",
            "[Message] Get a conversation message by ID",
            createMessageGetSchema(),
            CATEGORY
        );

        registerTool(
            "conversation_message_update",
            "[Message] Update an existing conversation message",
            createMessageUpdateSchema(),
            CATEGORY
        );

        registerTool(
            "conversation_message_delete",
            "[Message] Delete a conversation message",
            createMessageGetSchema(),
            CATEGORY
        );

        registerTool(
            "conversation_message_list",
            "[Message] List conversation messages",
            createMessageListSchema(),
            CATEGORY
        );
    }

    @Override
    protected Mono<Map<String, Object>> executeToolInternal(String toolName, Map<String, Object> arguments) {
        return switch (toolName) {
            // Call operations
            case "call_create" -> callService.createCall(arguments);
            case "call_get" -> callService.getCall(arguments);
            case "call_update" -> callService.updateCall(arguments);
            case "call_delete" -> callService.deleteCall(arguments);
            case "call_list" -> callService.listCalls(arguments);

            // Conversation operations
            case "conversation_create" -> conversationService.createConversation(arguments);
            case "conversation_get" -> conversationService.getConversation(arguments);
            case "conversation_update" -> conversationService.updateConversation(arguments);
            case "conversation_delete" -> conversationService.deleteConversation(arguments);
            case "conversation_list" -> conversationService.listConversations(arguments);

            // Conversation message operations
            case "conversation_message_create" -> conversationMessageService.createConversationMessage(arguments);
            case "conversation_message_get" -> conversationMessageService.getConversationMessage(arguments);
            case "conversation_message_update" -> conversationMessageService.updateConversationMessage(arguments);
            case "conversation_message_delete" -> conversationMessageService.deleteConversationMessage(arguments);
            case "conversation_message_list" -> conversationMessageService.listConversationMessages(arguments);

            default -> Mono.error(new UnsupportedOperationException(
                "Tool '" + toolName + "' is not implemented in " + DOMAIN + " domain registry"));
        };
    }

    // ========== Call Schema Methods ==========

    /**
     * Creates the schema for call creation.
     * Calls track phone conversations with contacts.
     */
    private Map<String, Object> createCallSchema() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("contactId", Map.of(
            "type", "integer",
            "description", "ID of the contact you called or who called you (required)"
        ));
        properties.put("calledAt", Map.of(
            "type", "string",
            "format", "date-time",
            "description", "Date and time of the call in ISO 8601 format (required)"
        ));
        properties.put("content", Map.of(
            "type", "string",
            "description", "Notes about what was discussed during the call (optional)"
        ));
        properties.put("contactCalledYou", Map.of(
            "type", "boolean",
            "description", "Set to true if contact called you, false if you called them (optional, default: false)",
            "default", false
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("contactId", "calledAt"));

        return schema;
    }

    /**
     * Creates the schema for call updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createCallUpdateSchema() {
        return createUpdateSchema(createCallSchema());
    }

    // ========== Conversation Schema Methods ==========

    /**
     * Creates the schema for conversation creation.
     * Conversations are text-based communication threads with contacts.
     */
    private Map<String, Object> createConversationSchema() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("contactId", Map.of(
            "type", "integer",
            "description", "Associated contact ID"
        ));
        properties.put("happenedAt", Map.of(
            "type", "string",
            "format", "date-time",
            "description", "Conversation timestamp"
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("contactId", "happenedAt"));

        return schema;
    }

    /**
     * Creates the schema for conversation updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createConversationUpdateSchema() {
        return createUpdateSchema(createConversationSchema());
    }

    // ========== Conversation Message Schema Methods ==========

    /**
     * Creates the schema for conversation message creation.
     * Messages are individual entries within a conversation thread.
     */
    private Map<String, Object> createMessageSchema() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("conversationId", Map.of(
            "type", "integer",
            "description", "Associated conversation ID"
        ));
        properties.put("content", Map.of(
            "type", "string",
            "description", "Message content"
        ));
        properties.put("writtenAt", Map.of(
            "type", "string",
            "format", "date-time",
            "description", "Message timestamp"
        ));
        properties.put("writtenByMe", Map.of(
            "type", "boolean",
            "description", "Whether message was written by user"
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("conversationId", "content", "writtenAt", "writtenByMe"));

        return schema;
    }

    /**
     * Creates the schema for message list operations.
     * Requires conversation ID and supports pagination.
     */
    private Map<String, Object> createMessageListSchema() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("conversationId", Map.of(
            "type", "integer",
            "description", "Conversation ID"
        ));
        properties.put("page", Map.of(
            "type", "integer",
            "description", "Page number",
            "default", 1
        ));
        properties.put("limit", Map.of(
            "type", "integer",
            "description", "Items per page",
            "default", 10
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("conversationId"));

        return schema;
    }

    /**
     * Creates the schema for getting a specific message.
     * Requires both conversation ID and message ID.
     */
    private Map<String, Object> createMessageGetSchema() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("conversationId", Map.of(
            "type", "integer",
            "description", "Conversation ID"
        ));
        properties.put("id", Map.of(
            "type", "integer",
            "description", "Message ID"
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("conversationId", "id"));

        return schema;
    }

    /**
     * Creates the schema for message updates.
     * Requires conversation ID, message ID, and supports updating message fields.
     */
    private Map<String, Object> createMessageUpdateSchema() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("conversationId", Map.of(
            "type", "integer",
            "description", "Conversation ID"
        ));
        properties.put("id", Map.of(
            "type", "integer",
            "description", "Message ID"
        ));
        properties.put("content", Map.of(
            "type", "string",
            "description", "Message content"
        ));
        properties.put("writtenAt", Map.of(
            "type", "string",
            "format", "date-time",
            "description", "Message timestamp"
        ));
        properties.put("writtenByMe", Map.of(
            "type", "boolean",
            "description", "Whether message was written by user"
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("conversationId", "id"));

        return schema;
    }
}
