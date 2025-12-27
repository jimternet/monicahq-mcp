package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Relationship Type Group entities.
 * <p>
 * Defines the field mappings and API endpoint for Relationship Type Group operations.
 * Relationship Type Groups categorize relationship types into groups
 * (e.g., "Love", "Family", "Friends", "Work").
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>Standard timestamp fields (created_at, updated_at) are handled by the base class</li>
 * </ul>
 * </p>
 * <p>
 * This is a read-only entity - create, update, and delete operations are not supported.
 * </p>
 */
@Component
public class RelationshipTypeGroupFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/relationshiptypegroups";
    private static final String ENTITY_NAME = "Relationship Type Group";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     * Note: This is a read-only entity with only standard timestamp fields.
     */
    private static final Map<String, String> TO_API_MAPPINGS = Collections.emptyMap();

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     * Standard timestamp fields are handled by base class, but included here for completeness.
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
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
     * Relationship Type Groups are read-only lookup entities.
     * @return false - create is not supported
     */
    @Override
    public boolean supportsCreate() {
        return false;
    }

    /**
     * Relationship Type Groups are read-only lookup entities.
     * @return false - update is not supported
     */
    @Override
    public boolean supportsUpdate() {
        return false;
    }

    /**
     * Relationship Type Groups are read-only lookup entities.
     * @return false - delete is not supported
     */
    @Override
    public boolean supportsDelete() {
        return false;
    }
}
