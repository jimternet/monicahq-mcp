package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.RelationshipTypeGroupFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Relationship Type Group entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard operation implementations.
 * Uses {@link RelationshipTypeGroupFieldMappingConfig} for Relationship Type Group-specific field mappings.
 * </p>
 * <p>
 * Relationship Type Groups categorize relationship types into logical groups
 * (e.g., "Love", "Family", "Friends", "Work").
 * </p>
 * <p>
 * This is a read-only service - only get and list operations are supported.
 * Relationship Type Groups are system-defined and cannot be created, updated, or deleted.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>getRelationshipTypeGroup - Retrieve a relationship type group by ID</li>
 *   <li>listRelationshipTypeGroups - List relationship type groups with optional pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class RelationshipTypeGroupService extends AbstractCrudService<Object> {

    private final RelationshipTypeGroupFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a RelationshipTypeGroupService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Relationship Type Groups
     */
    public RelationshipTypeGroupService(MonicaHqClient monicaClient,
                                        ContentFormatter contentFormatter,
                                        RelationshipTypeGroupFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Retrieves a relationship type group by its ID.
     *
     * @param arguments map containing "id" - the relationship type group ID to retrieve
     * @return a Mono containing the relationship type group data
     */
    public Mono<Map<String, Object>> getRelationshipTypeGroup(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Lists relationship type groups with optional pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional pagination
     * @return a Mono containing the list of relationship type groups and pagination metadata
     */
    public Mono<Map<String, Object>> listRelationshipTypeGroups(Map<String, Object> arguments) {
        return list(arguments);
    }
}
