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
 * Contact Field Types are read-only lookup values that define valid types for
 * contact fields (e.g., email, phone, social media handles).
 * </p>
 * <p>
 * Implements Constitutional Principle VII: API Discovery and Completeness.
 * This discovery tool shows valid contactFieldTypeId values for contact field creation.
 * </p>
 * <p>
 * This is a read-only entity - only list operation is supported.
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
        return Collections.emptyMap();
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
     * Contact Field Types are read-only lookup values.
     * @return false - create is not supported
     */
    @Override
    public boolean supportsCreate() {
        return false;
    }

    /**
     * Contact Field Types are read-only lookup values.
     * @return false - update is not supported
     */
    @Override
    public boolean supportsUpdate() {
        return false;
    }

    /**
     * Contact Field Types are read-only lookup values.
     * @return false - delete is not supported
     */
    @Override
    public boolean supportsDelete() {
        return false;
    }
}
