package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Relationship entities.
 * <p>
 * Defines the field mappings, validation rules, and API endpoint for Relationship CRUD operations.
 * Relationships connect two contacts with a specific relationship type.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>contactIs (client) -> contact_is (API)</li>
 *   <li>ofContact (client) -> of_contact (API)</li>
 *   <li>relationshipTypeId (client) -> relationship_type_id (API)</li>
 * </ul>
 * </p>
 * <p>
 * Required fields for creation and update: contactIs, ofContact, relationshipTypeId
 * </p>
 */
@Component
public class RelationshipFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/relationships";
    private static final String ENTITY_NAME = "Relationship";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "contactIs", "contact_is",
        "ofContact", "of_contact",
        "relationshipTypeId", "relationship_type_id"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "contact_is", "contactIs",
        "of_contact", "ofContact",
        "relationship_type_id", "relationshipTypeId",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Required fields for Relationship creation.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of(
        "contactIs", "ofContact", "relationshipTypeId"
    );

    /**
     * Required fields for Relationship update.
     * Relationships require all fields to be specified on update.
     */
    private static final Set<String> REQUIRED_UPDATE_FIELDS = Set.of(
        "contactIs", "ofContact", "relationshipTypeId"
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
        return REQUIRED_CREATE_FIELDS;
    }

    @Override
    public Set<String> getRequiredUpdateFields() {
        return REQUIRED_UPDATE_FIELDS;
    }

    @Override
    public List<String> getListFilterFields() {
        return Collections.emptyList();
    }
}
