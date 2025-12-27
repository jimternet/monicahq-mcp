package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for ContactTag entities.
 * <p>
 * Defines the field mappings and entity metadata for ContactTag operations.
 * </p>
 * <p>
 * Note: ContactTag operations use nested resource endpoints and do not follow
 * standard CRUD patterns. The service overrides base class methods to handle:
 * <ul>
 *   <li>attachTag: POST /contacts/{contactId}/setTags</li>
 *   <li>getContactTags: GET /contacts/{contactId}/tags</li>
 *   <li>updateContactTags: PUT /contacts/{contactId}/setTags</li>
 *   <li>detachTag: DELETE /contacts/{contactId}/unsetTag/{tagId}</li>
 *   <li>listContactsByTag: GET /contacts with tags filter</li>
 * </ul>
 * </p>
 * <p>
 * Required fields for attach operation: contactId, tagId
 * </p>
 */
@Component
public class ContactTagFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/contacts";
    private static final String ENTITY_NAME = "Contact Tag";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     * ContactTag has minimal field mappings as it primarily uses IDs.
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "contactId", "contact_id",
        "tagId", "tag_id"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "contact_id", "contactId",
        "tag_id", "tagId",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Required fields for ContactTag attach operation.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("contactId", "tagId");

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
     * ContactTag does not support standard create operations.
     * Use attachTag method instead.
     */
    @Override
    public boolean supportsCreate() {
        return false;
    }

    /**
     * ContactTag does not support standard update operations.
     * Use updateContactTags method instead.
     */
    @Override
    public boolean supportsUpdate() {
        return false;
    }

    /**
     * ContactTag does not support standard delete operations.
     * Use detachTag method instead.
     */
    @Override
    public boolean supportsDelete() {
        return false;
    }

    /**
     * ContactTag does not support standard list operations.
     * Use getContactTags or listContactsByTag methods instead.
     */
    @Override
    public boolean supportsList() {
        return false;
    }
}
