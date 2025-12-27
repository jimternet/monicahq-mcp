package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Call entities.
 * <p>
 * Defines the field mappings, validation rules, and API endpoint for Call CRUD operations.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>contactId (client) -> contact_id (API)</li>
 *   <li>calledAt (client) -> called_at (API)</li>
 *   <li>durationInMinutes (client) -> duration (API)</li>
 * </ul>
 * </p>
 * <p>
 * Required fields for creation: contactId, calledAt (calledAt requires non-empty string validation)
 * </p>
 * <p>
 * Available list filters: contactId
 * </p>
 */
@Component
public class CallFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/calls";
    private static final String ENTITY_NAME = "Call";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "contactId", "contact_id",
        "calledAt", "called_at",
        "durationInMinutes", "duration"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "contact_id", "contactId",
        "called_at", "calledAt",
        "duration", "durationInMinutes",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Required fields for Call creation.
     * Note: calledAt validation requires non-empty string check, done in service.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("contactId", "calledAt");

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
