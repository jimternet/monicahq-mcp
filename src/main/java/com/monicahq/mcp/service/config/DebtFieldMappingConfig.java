package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Debt entities.
 * <p>
 * Defines the field mappings, validation rules, and API endpoint for Debt CRUD operations.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>contactId (client) -> contact_id (API)</li>
 *   <li>inDebt (client) -> in_debt (API)</li>
 * </ul>
 * </p>
 * <p>
 * Required fields for creation: contactId, amount
 * </p>
 */
@Component
public class DebtFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/debts";
    private static final String ENTITY_NAME = "Debt";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "contactId", "contact_id",
        "inDebt", "in_debt"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.ofEntries(
        Map.entry("contact_id", "contactId"),
        Map.entry("in_debt", "inDebt"),
        Map.entry("created_at", "createdAt"),
        Map.entry("updated_at", "updatedAt")
    );

    /**
     * Required fields for Debt creation.
     * Monica API requires contactId and amount.
     * inDebt defaults to "owed" if not provided.
     * status is also required by the API.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("contactId", "amount", "status");

    /**
     * Fields that can be used as filters in list operations.
     */
    private static final List<String> LIST_FILTER_FIELDS = List.of("contactId", "status");

    /**
     * Query parameter mappings for list operations.
     * Maps client filter names to API query parameter names.
     */
    private static final Map<String, String> QUERY_PARAM_MAPPINGS = Map.of(
        "contactId", "contact_id",
        "status", "status"
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

    @Override
    public Map<String, Object> getCreateDefaults() {
        // Default in_debt to "yes" (user owes contact) if not provided
        // Valid values: "yes" (I owe them) or "no" (they owe me)
        return Map.of("inDebt", "yes");
    }
}
