package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Journal Entry entities.
 * <p>
 * Defines the field mappings, validation rules, and API endpoint for Journal Entry CRUD operations.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>journalEntry (client) -> journal_entry (API)</li>
 * </ul>
 * </p>
 * <p>
 * Required fields for creation: title, date
 * </p>
 */
@Component
public class JournalEntryFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/entries";
    private static final String ENTITY_NAME = "Journal Entry";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "journalEntry", "journal_entry"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "journal_entry", "journalEntry",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Required fields for Journal Entry creation.
     * Note: title requires additional non-empty string validation in the service.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("title", "date");

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
}
