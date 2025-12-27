package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Document entities.
 * <p>
 * Defines the field mappings, validation rules, and API endpoint for Document CRUD operations.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>contactId (client) -> contact_id (API)</li>
 *   <li>originalFilename (client) -> original_filename (API)</li>
 *   <li>mimeType (client) -> mime_type (API)</li>
 *   <li>downloadUrl (client) <- download_url (API)</li>
 * </ul>
 * </p>
 * <p>
 * Required fields for creation: contactId, filename
 * Note: filename requires additional non-empty string validation in the service.
 * </p>
 */
@Component
public class DocumentFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/documents";
    private static final String ENTITY_NAME = "Document";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "contactId", "contact_id",
        "originalFilename", "original_filename",
        "mimeType", "mime_type"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.ofEntries(
        Map.entry("contact_id", "contactId"),
        Map.entry("original_filename", "originalFilename"),
        Map.entry("mime_type", "mimeType"),
        Map.entry("download_url", "downloadUrl"),
        Map.entry("created_at", "createdAt"),
        Map.entry("updated_at", "updatedAt")
    );

    /**
     * Required fields for Document creation.
     * Note: filename requires additional non-empty string validation in the service.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("contactId", "filename");

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
        return Collections.emptyList();
    }

    @Override
    public Map<String, String> getQueryParamMappings() {
        return Collections.emptyMap();
    }
}
