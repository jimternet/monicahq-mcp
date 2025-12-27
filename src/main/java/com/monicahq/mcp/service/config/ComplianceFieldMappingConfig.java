package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Compliance entities.
 * <p>
 * Defines the field mappings, validation rules, and API endpoint for Compliance CRUD operations.
 * </p>
 * <p>
 * Note: Compliance API endpoints may not be clearly defined in all Monica versions.
 * Operations include graceful error handling for API availability issues.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>contactId (client) -> contact_id (API)</li>
 *   <li>isActive (client) -> is_active (API)</li>
 *   <li>dataRetentionDays (client) -> data_retention_days (API)</li>
 *   <li>privacyLevel (client) -> privacy_level (API)</li>
 *   <li>consentGiven (client) -> consent_given (API)</li>
 *   <li>consentDate (client) -> consent_date (API)</li>
 *   <li>auditRequired (client) -> audit_required (API)</li>
 * </ul>
 * </p>
 * <p>
 * Required fields for creation: type (must be non-empty)
 * </p>
 * <p>
 * Available list filters: contactId
 * </p>
 */
@Component
public class ComplianceFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/compliance";
    private static final String ENTITY_NAME = "Compliance";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "contactId", "contact_id",
        "isActive", "is_active",
        "dataRetentionDays", "data_retention_days",
        "privacyLevel", "privacy_level",
        "consentGiven", "consent_given",
        "consentDate", "consent_date",
        "auditRequired", "audit_required"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.ofEntries(
        Map.entry("contact_id", "contactId"),
        Map.entry("is_active", "isActive"),
        Map.entry("data_retention_days", "dataRetentionDays"),
        Map.entry("privacy_level", "privacyLevel"),
        Map.entry("consent_given", "consentGiven"),
        Map.entry("consent_date", "consentDate"),
        Map.entry("audit_required", "auditRequired"),
        Map.entry("created_at", "createdAt"),
        Map.entry("updated_at", "updatedAt")
    );

    /**
     * Required fields for Compliance creation.
     * Note: type requires non-empty string validation.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("type");

    /**
     * Fields that can be used as filters in list operations.
     */
    private static final List<String> LIST_FILTER_FIELDS = List.of("contactId");

    /**
     * Query parameter mappings for list operations.
     */
    private static final Map<String, String> QUERY_PARAM_MAPPINGS = Map.of(
        "contactId", "contact_id"
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
    public List<String> getListFilterFields() {
        return LIST_FILTER_FIELDS;
    }

    @Override
    public Map<String, String> getQueryParamMappings() {
        return QUERY_PARAM_MAPPINGS;
    }
}
