package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.ConversationFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Conversation entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link ConversationFieldMappingConfig} for Conversation-specific field mappings and validation.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createConversation - Create a new conversation</li>
 *   <li>getConversation - Retrieve a conversation by ID</li>
 *   <li>updateConversation - Update an existing conversation</li>
 *   <li>deleteConversation - Delete a conversation by ID</li>
 *   <li>listConversations - List conversations with optional filtering and pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class ConversationService extends AbstractCrudService<Object> {

    private final ConversationFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a ConversationService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Conversations
     */
    public ConversationService(MonicaHqClient monicaClient,
                               ContentFormatter contentFormatter,
                               ConversationFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new conversation.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The ID of the contact associated with the conversation</li>
     *   <li>happenedAt - The date when the conversation happened</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created conversation data
     */
    public Mono<Map<String, Object>> createConversation(Map<String, Object> arguments) {
        return create(arguments);
    }

    /**
     * Retrieves a conversation by its ID.
     *
     * @param arguments map containing "id" - the conversation ID to retrieve
     * @return a Mono containing the conversation data
     */
    public Mono<Map<String, Object>> getConversation(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing conversation.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the conversation to update</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>contactId - New contact ID</li>
     *   <li>happenedAt - New conversation date</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the conversation ID
     * @return a Mono containing the updated conversation data
     */
    public Mono<Map<String, Object>> updateConversation(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes a conversation by its ID.
     *
     * @param arguments map containing "id" - the conversation ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteConversation(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists conversations with optional filtering and pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     *   <li>contactId - Filter by contact ID</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional filters and pagination
     * @return a Mono containing the list of conversations and pagination metadata
     */
    public Mono<Map<String, Object>> listConversations(Map<String, Object> arguments) {
        return list(arguments);
    }
}
