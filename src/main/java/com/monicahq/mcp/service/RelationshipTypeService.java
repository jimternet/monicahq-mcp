package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.RelationshipTypeFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Relationship Type entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard operation implementations.
 * Uses {@link RelationshipTypeFieldMappingConfig} for Relationship Type-specific field mappings.
 * </p>
 * <p>
 * Relationship Types define the types of relationships between contacts
 * (e.g., "Partner", "Friend", "Parent", "Child", "Sibling").
 * </p>
 * <p>
 * This is a read-only service - only get and list operations are supported.
 * Relationship Types are system-defined and cannot be created, updated, or deleted.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>getRelationshipType - Retrieve a relationship type by ID</li>
 *   <li>listRelationshipTypes - List relationship types with optional pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class RelationshipTypeService extends AbstractCrudService<Object> {

    private final RelationshipTypeFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a RelationshipTypeService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Relationship Types
     */
    public RelationshipTypeService(MonicaHqClient monicaClient,
                                   ContentFormatter contentFormatter,
                                   RelationshipTypeFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Retrieves a relationship type by its ID.
     *
     * @param arguments map containing "id" - the relationship type ID to retrieve
     * @return a Mono containing the relationship type data
     */
    public Mono<Map<String, Object>> getRelationshipType(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Lists relationship types with optional pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional pagination
     * @return a Mono containing the list of relationship types and pagination metadata
     */
    public Mono<Map<String, Object>> listRelationshipTypes(Map<String, Object> arguments) {
        return list(arguments);
    }
}
