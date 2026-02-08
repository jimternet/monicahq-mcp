package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Conversation entities.
 * <p>
 * Defines the field mappings, validation rules, and API endpoint for Conversation CRUD operations.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>contactId (client) -> contact_id (API)</li>
 *   <li>happenedAt (client) -> happened_at (API)</li>
 * </ul>
 * </p>
 * <p>
 * Required fields for creation: contactId, happenedAt
 * </p>
 * <p>
 * Available list filters: contactId
 * </p>
 */
@Component
public class ConversationFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/conversations";
    private static final String ENTITY_NAME = "Conversation";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "contactId", "contact_id",
        "happenedAt", "happened_at",
        "contactFieldTypeId", "contact_field_type_id"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.ofEntries(
        Map.entry("contact_id", "contactId"),
        Map.entry("happened_at", "happenedAt"),
        Map.entry("contact_field_type_id", "contactFieldTypeId"),
        Map.entry("created_at", "createdAt"),
        Map.entry("updated_at", "updatedAt")
    );

    /**
     * Required fields for Conversation creation.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("contactId", "happenedAt", "contactFieldTypeId");

    /**
     * Fields that can be used as filters in list operations.
     */
    private static final List<String> LIST_FILTER_FIELDS = List.of("contactId");

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

    @Override
    public List<String> getListFilterFields() {
        return LIST_FILTER_FIELDS;
    }
}
