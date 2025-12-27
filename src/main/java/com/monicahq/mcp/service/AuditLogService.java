package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.AuditLogFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing AuditLog entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard operation implementations.
 * Uses {@link AuditLogFieldMappingConfig} for AuditLog-specific field mappings.
 * </p>
 * <p>
 * AuditLog entities are read-only records that track system changes.
 * Only get, list, and search operations are supported.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>getAuditLog - Retrieve an audit log entry by ID</li>
 *   <li>listAuditLogs - List audit log entries with pagination</li>
 *   <li>searchAuditLogs - Search audit logs by action, auditableType, or userId</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class AuditLogService extends AbstractCrudService<Object> {

    private final AuditLogFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs an AuditLogService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for AuditLogs
     */
    public AuditLogService(MonicaHqClient monicaClient,
                           ContentFormatter contentFormatter,
                           AuditLogFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Retrieves an audit log entry by its ID.
     *
     * @param arguments map containing "id" - the audit log ID to retrieve
     * @return a Mono containing the audit log data
     */
    public Mono<Map<String, Object>> getAuditLog(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Lists audit log entries with optional pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional pagination
     * @return a Mono containing the list of audit logs and pagination metadata
     */
    public Mono<Map<String, Object>> listAuditLogs(Map<String, Object> arguments) {
        return list(arguments != null ? arguments : Map.of());
    }

    /**
     * Searches audit log entries with optional filters.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>action - Filter by action type</li>
     *   <li>auditableType - Filter by auditable entity type</li>
     *   <li>userId - Filter by user ID</li>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page (default: 10)</li>
     * </ul>
     * </p>
     * <p>
     * This method uses the same list endpoint but with search filters applied.
     * The filter fields are defined in {@link AuditLogFieldMappingConfig}.
     * </p>
     *
     * @param arguments the search arguments including optional filters and pagination
     * @return a Mono containing the filtered list of audit logs and pagination metadata
     */
    public Mono<Map<String, Object>> searchAuditLogs(Map<String, Object> arguments) {
        log.info("Searching audit logs with arguments: {}", arguments);

        try {
            // Use the inherited buildListQueryParams which handles the filter fields
            // defined in AuditLogFieldMappingConfig (action, auditableType, userId)
            Map<String, String> queryParams = buildListQueryParams(arguments != null ? arguments : Map.of());

            return monicaClient.get(getEndpointPath(), queryParams)
                .map(this::formatListResponse)
                .doOnSuccess(result -> log.info("Audit logs searched successfully"))
                .doOnError(error -> log.error("Failed to search audit logs: {}", error.getMessage()));

        } catch (Exception e) {
            log.error("Error searching audit logs: {}", e.getMessage());
            return Mono.error(e);
        }
    }
}
