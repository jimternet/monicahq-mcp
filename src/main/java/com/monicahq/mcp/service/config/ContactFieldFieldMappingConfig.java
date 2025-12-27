package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for ContactField entities.
 * <p>
 * Defines the field mappings, validation rules, and API endpoint for ContactField CRUD operations.
 * </p>
 * <p>
 * Note: ContactField has a special list endpoint (/contacts/{contactId}/contactfields) that
 * requires the contactId to be extracted separately. The service handles this special case.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>contactFieldTypeId (client) -> contact_field_type_id (API)</li>
 *   <li>contactId (client) -> contact_id (API)</li>
 * </ul>
 * </p>
 * <p>
 * Required fields for creation: contactId, contactFieldTypeId, data (must be non-empty string)
 * </p>
 */
@Component
public class ContactFieldFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/contactfields";
    private static final String ENTITY_NAME = "Contact Field";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "contactFieldTypeId", "contact_field_type_id",
        "contactId", "contact_id"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "contact_field_type_id", "contactFieldTypeId",
        "contact_id", "contactId",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Required fields for ContactField creation.
     * Note: data validation requires non-empty string check, done in service.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("contactId", "contactFieldTypeId", "data");

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
}
