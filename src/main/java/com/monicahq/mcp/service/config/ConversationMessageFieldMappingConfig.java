package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Conversation Message entities.
 * <p>
 * Defines the field mappings, validation rules for Conversation Message operations.
 * Conversation Messages are a nested resource under Conversations, with endpoints like:
 * <ul>
 *   <li>POST /conversations/{conversationId}/messages</li>
 *   <li>GET /conversations/{conversationId}/messages/{id}</li>
 *   <li>PUT /conversations/{conversationId}/messages/{id}</li>
 *   <li>DELETE /conversations/{conversationId}/messages/{id}</li>
 * </ul>
 * </p>
 * <p>
 * Note: The endpoint path is not used directly since ConversationMessageService overrides
 * all CRUD methods to handle the nested URL pattern. This config provides field mappings
 * and validation rules that the service uses.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>writtenAt (client) -> written_at (API)</li>
 *   <li>writtenByMe (client) -> written_by_me (API)</li>
 *   <li>conversationId (client) -> conversation_id (API)</li>
 * </ul>
 * </p>
 * <p>
 * Required fields for creation: conversationId, writtenAt, writtenByMe
 * </p>
 */
@Component
public class ConversationMessageFieldMappingConfig implements FieldMappingConfig {

    // This is a placeholder - the actual endpoint construction is done in the service
    // because messages use nested endpoints under conversations
    private static final String ENDPOINT_PATH = "/conversations";
    private static final String ENTITY_NAME = "Conversation Message";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "writtenAt", "written_at",
        "writtenByMe", "written_by_me",
        "conversationId", "conversation_id"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "written_at", "writtenAt",
        "written_by_me", "writtenByMe",
        "conversation_id", "conversationId",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Required fields for Conversation Message creation.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("conversationId", "writtenAt", "writtenByMe");

    @Override
    public String getEndpointPath() {
        return ENDPOINT_PATH;
    }

    @Override
    public String getEntityName() {
        return ENTITY_NAME;
    }

    @Override
    public Map<String, String> getToApiMappings() {
        return TO_API_MAPPINGS;
    }

    @Override
    public Map<String, String> getFromApiMappings() {
        return FROM_API_MAPPINGS;
    }

    @Override
    public Set<String> getRequiredCreateFields() {
        return REQUIRED_CREATE_FIELDS;
    }

    /**
     * Conversation Messages have no list-level filters as they are accessed through
     * the conversation workaround.
     */
    @Override
    public List<String> getListFilterFields() {
        return Collections.emptyList();
    }

    /**
     * Standard list operation returns HTTP 405 for conversation messages.
     * The listConversationMessages method uses a workaround instead.
     */
    @Override
    public boolean supportsList() {
        return false;
    }
}
