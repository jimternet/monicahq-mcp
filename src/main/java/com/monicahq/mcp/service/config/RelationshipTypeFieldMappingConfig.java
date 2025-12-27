package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Relationship Type entities.
 * <p>
 * Defines the field mappings and API endpoint for Relationship Type operations.
 * Relationship Types are read-only lookup entities that define the types of relationships
 * between contacts (e.g., "Partner", "Friend", "Parent").
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>nameReverse (client) -> name_reverse (API)</li>
 *   <li>relationshipTypeGroupId (client) -> relationship_type_group_id (API)</li>
 * </ul>
 * </p>
 * <p>
 * This is a read-only entity - create, update, and delete operations are not supported.
 * </p>
 */
@Component
public class RelationshipTypeFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/relationshiptypes";
    private static final String ENTITY_NAME = "Relationship Type";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     * Note: This is a read-only entity, so these mappings are only used for reference.
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "nameReverse", "name_reverse",
        "relationshipTypeGroupId", "relationship_type_group_id"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "name_reverse", "nameReverse",
        "relationship_type_group_id", "relationshipTypeGroupId",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

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
        return Collections.emptySet();
    }

    @Override
    public List<String> getListFilterFields() {
        return Collections.emptyList();
    }

    /**
     * Relationship Types are read-only lookup entities.
     * @return false - create is not supported
     */
    @Override
    public boolean supportsCreate() {
        return false;
    }

    /**
     * Relationship Types are read-only lookup entities.
     * @return false - update is not supported
     */
    @Override
    public boolean supportsUpdate() {
        return false;
    }

    /**
     * Relationship Types are read-only lookup entities.
     * @return false - delete is not supported
     */
    @Override
    public boolean supportsDelete() {
        return false;
    }
}
