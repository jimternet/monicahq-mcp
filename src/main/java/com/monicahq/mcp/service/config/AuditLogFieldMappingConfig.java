package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for AuditLog entities.
 * <p>
 * Defines the field mappings and API endpoint for AuditLog operations.
 * AuditLog entities are read-only records that track system changes.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>auditableType (client) <-> auditable_type (API)</li>
 *   <li>auditableId (client) <-> auditable_id (API)</li>
 *   <li>userId (client) <-> user_id (API)</li>
 *   <li>userName (client) <-> user_name (API)</li>
 *   <li>ipAddress (client) <-> ip_address (API)</li>
 *   <li>userAgent (client) <-> user_agent (API)</li>
 *   <li>oldValues (client) <-> old_values (API)</li>
 *   <li>newValues (client) <-> new_values (API)</li>
 * </ul>
 * </p>
 * <p>
 * This is a read-only entity - only get and list operations are supported.
 * Additionally, searchAuditLogs provides filtered listing by action, auditableType, and userId.
 * </p>
 */
@Component
public class AuditLogFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/auditlogs";
    private static final String ENTITY_NAME = "Audit Log";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     * Used for query parameter mapping in search operations.
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "auditableType", "auditable_type",
        "auditableId", "auditable_id",
        "userId", "user_id",
        "userName", "user_name",
        "ipAddress", "ip_address",
        "userAgent", "user_agent",
        "oldValues", "old_values",
        "newValues", "new_values"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "auditable_type", "auditableType",
        "auditable_id", "auditableId",
        "user_id", "userId",
        "user_name", "userName",
        "ip_address", "ipAddress",
        "user_agent", "userAgent",
        "old_values", "oldValues",
        "new_values", "newValues",
        "created_at", "createdAt"
    );

    /**
     * Search filter fields supported by the audit log API.
     * These can be used to filter results when listing/searching.
     */
    private static final List<String> LIST_FILTER_FIELDS = List.of("action", "auditableType", "userId");

    /**
     * Query parameter mappings for list/search operations.
     */
    private static final Map<String, String> QUERY_PARAM_MAPPINGS = Map.of(
        "auditableType", "auditable_type",
        "userId", "user_id"
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
        return LIST_FILTER_FIELDS;
    }

    @Override
    public Map<String, String> getQueryParamMappings() {
        return QUERY_PARAM_MAPPINGS;
    }

    /**
     * Audit logs are read-only records.
     * @return false - create is not supported
     */
    @Override
    public boolean supportsCreate() {
        return false;
    }

    /**
     * Audit logs are read-only records.
     * @return false - update is not supported
     */
    @Override
    public boolean supportsUpdate() {
        return false;
    }

    /**
     * Audit logs are read-only records.
     * @return false - delete is not supported
     */
    @Override
    public boolean supportsDelete() {
        return false;
    }
}
