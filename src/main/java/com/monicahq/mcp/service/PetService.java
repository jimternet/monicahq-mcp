package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.PetFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing pets associated with contacts in MonicaHQ.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link PetFieldMappingConfig} for Pet-specific field mappings and validation.
 * </p>
 * <p>
 * This service uses root-level POST endpoint (/pets) instead of nested endpoint
 * (/contacts/{id}/pets) to avoid HTTP 405 errors. The contact_id is passed in the
 * request body as required by the MonicaHQ API.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createPet - Create a new pet for a contact</li>
 *   <li>getPet - Retrieve a pet by ID</li>
 *   <li>updatePet - Update an existing pet (petCategoryId required)</li>
 *   <li>deletePet - Delete a pet by ID</li>
 *   <li>listPets - List pets with optional pagination</li>
 * </ul>
 * </p>
 *
 * @see <a href="docs/API-LIMITATIONS.md">API Limitations documentation</a>
 */
@Service
@Slf4j
public class PetService extends AbstractCrudService<Object> {

    private final PetFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a PetService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Pets
     */
    public PetService(MonicaHqClient monicaClient,
                      ContentFormatter contentFormatter,
                      PetFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new pet for a contact.
     * <p>
     * Uses POST /pets with contact_id in the request body (not the nested
     * /contacts/{id}/pets endpoint which returns HTTP 405).
     * </p>
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The ID of the contact to associate the pet with</li>
     *   <li>petCategoryId - The ID of the pet category (e.g., cat, dog)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>name - The pet's name</li>
     * </ul>
     * </p>
     *
     * @param arguments Map containing contactId, petCategoryId, and optionally name
     * @return Mono containing the created pet data
     */
    public Mono<Map<String, Object>> createPet(Map<String, Object> arguments) {
        return create(arguments);
    }

    /**
     * Retrieves a pet by its ID.
     *
     * @param arguments map containing "id" - the pet ID to retrieve
     * @return a Mono containing the pet data
     */
    public Mono<Map<String, Object>> getPet(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing pet.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the pet to update</li>
     *   <li>petCategoryId - The pet category ID (required by API)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>name - New name for the pet</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the pet ID
     * @return a Mono containing the updated pet data
     */
    public Mono<Map<String, Object>> updatePet(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes a pet by its ID.
     *
     * @param arguments map containing "id" - the pet ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deletePet(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists pets with optional pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional pagination
     * @return a Mono containing the list of pets and pagination metadata
     */
    public Mono<Map<String, Object>> listPets(Map<String, Object> arguments) {
        return list(arguments);
    }
}
