package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.RelationshipFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Relationship entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link RelationshipFieldMappingConfig} for Relationship-specific field mappings and validation.
 * </p>
 * <p>
 * Relationships connect two contacts with a specific relationship type (e.g., partner, parent, friend).
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createRelationship - Create a new relationship between contacts</li>
 *   <li>getRelationship - Retrieve a relationship by ID</li>
 *   <li>updateRelationship - Update an existing relationship</li>
 *   <li>deleteRelationship - Delete a relationship by ID</li>
 *   <li>listRelationships - List relationships with optional pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class RelationshipService extends AbstractCrudService<Object> {

    private final RelationshipFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a RelationshipService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Relationships
     */
    public RelationshipService(MonicaHqClient monicaClient,
                               ContentFormatter contentFormatter,
                               RelationshipFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new relationship between two contacts.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactIs - The ID of the first contact in the relationship</li>
     *   <li>ofContact - The ID of the second contact in the relationship</li>
     *   <li>relationshipTypeId - The ID of the relationship type</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>notes - Additional notes about the relationship</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created relationship data
     */
    public Mono<Map<String, Object>> createRelationship(Map<String, Object> arguments) {
        return create(arguments);
    }

    /**
     * Retrieves a relationship by its ID.
     *
     * @param arguments map containing "id" - the relationship ID to retrieve
     * @return a Mono containing the relationship data
     */
    public Mono<Map<String, Object>> getRelationship(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing relationship.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the relationship to update</li>
     *   <li>contactIs - The ID of the first contact in the relationship</li>
     *   <li>ofContact - The ID of the second contact in the relationship</li>
     *   <li>relationshipTypeId - The ID of the relationship type</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>notes - Additional notes about the relationship</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the relationship ID
     * @return a Mono containing the updated relationship data
     */
    public Mono<Map<String, Object>> updateRelationship(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes a relationship by its ID.
     *
     * @param arguments map containing "id" - the relationship ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteRelationship(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists relationships with optional pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional pagination
     * @return a Mono containing the list of relationships and pagination metadata
     */
    public Mono<Map<String, Object>> listRelationships(Map<String, Object> arguments) {
        return list(arguments);
    }
}
