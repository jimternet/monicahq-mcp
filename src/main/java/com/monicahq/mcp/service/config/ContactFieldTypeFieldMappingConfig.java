package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Contact Field Type entities.
 * <p>
 * Defines the field mappings and API endpoint for Contact Field Type operations.
 * Contact Field Types define valid types for contact fields (e.g., email, phone, social media handles).
 * </p>
 * <p>
 * Implements Constitutional Principle VII: API Discovery and Completeness.
 * Enables users to create and manage custom field type definitions per account.
 * </p>
 * <p>
 * Gap Analysis Phase 2: Enabled CRUD operations (previously read-only).
 * </p>
 */
@Component
public class ContactFieldTypeFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/contactfieldtypes";
    private static final String ENTITY_NAME = "Contact Field Type";

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "created_at", "createdAt",
        "updated_at", "updatedAt",
        "fontawesome_icon", "fontawesomeIcon"
    );

    /**
     * Required fields for creating a new contact field type.
     * Both 'name' and 'type' are required for field type creation.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("name", "type");

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
        return Collections.emptyMap();
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
        return Collections.emptyList();
    }

    /**
     * Contact Field Type entities support create operations.
     * Allows creation of custom field type definitions per account.
     * @return true - create is supported
     */
    @Override
    public boolean supportsCreate() {
        return true;
    }

    /**
     * Contact Field Type entities support update operations.
     * Allows modification of field type definitions.
     * @return true - update is supported
     */
    @Override
    public boolean supportsUpdate() {
        return true;
    }

    /**
     * Contact Field Type entities support delete operations.
     * Allows removal of custom field type definitions.
     * @return true - delete is supported
     */
    @Override
    public boolean supportsDelete() {
        return true;
    }
}
