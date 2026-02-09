package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.ContactFieldFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.*;

/**
 * Service for managing ContactField entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link ContactFieldFieldMappingConfig} for ContactField-specific field mappings and validation.
 * </p>
 * <p>
 * Note: The list operation uses a nested resource endpoint (/contacts/{contactId}/contactfields)
 * which requires the contactId. This is handled by overriding the listContactFields method.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createContactField - Create a new contact field</li>
 *   <li>getContactField - Retrieve a contact field by ID</li>
 *   <li>updateContactField - Update an existing contact field</li>
 *   <li>deleteContactField - Delete a contact field by ID</li>
 *   <li>listContactFields - List contact fields for a specific contact</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class ContactFieldService extends AbstractCrudService<Object> {

    private final ContactFieldFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a ContactFieldService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for ContactFields
     */
    public ContactFieldService(MonicaHqClient monicaClient,
                               ContentFormatter contentFormatter,
                               ContactFieldFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new contact field.
     * <p>
     * Note: Uses the global /contactfields endpoint with contact_id in the request body.
     * The nested endpoint /contacts/{id}/contactfields POST returns 405 Method Not Allowed.
     * </p>
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The ID of the contact this field belongs to</li>
     *   <li>contactFieldTypeId - The type of contact field</li>
     *   <li>data - The field value (must be non-empty)</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created contact field data
     */
    public Mono<Map<String, Object>> createContactField(Map<String, Object> arguments) {
        // Validate data is non-empty string before delegating to base class
        if (arguments != null && !arguments.isEmpty()) {
            validateRequiredString(arguments, "data");
        }
        // Use the base create() method which uses /contactfields from the config
        return create(arguments);
    }

    /**
     * Retrieves a contact field by its ID.
     *
     * @param arguments map containing "id" - the contact field ID to retrieve
     * @return a Mono containing the contact field data
     */
    public Mono<Map<String, Object>> getContactField(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing contact field.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the contact field to update</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>contactId - The ID of the contact</li>
     *   <li>contactFieldTypeId - The type of contact field</li>
     *   <li>data - The field value</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the contact field ID
     * @return a Mono containing the updated contact field data
     */
    public Mono<Map<String, Object>> updateContactField(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes a contact field by its ID.
     *
     * @param arguments map containing "id" - the contact field ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteContactField(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists contact fields for a specific contact.
     * <p>
     * This method uses a nested resource endpoint: /contacts/{contactId}/contactfields
     * </p>
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The ID of the contact to list fields for</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including required contactId and optional pagination
     * @return a Mono containing the list of contact fields and pagination metadata
     */
    public Mono<Map<String, Object>> listContactFields(Map<String, Object> arguments) {
        log.info("Listing contact fields with arguments: {}", arguments);

        try {
            Long contactId = extractContactId(arguments);
            Map<String, String> queryParams = buildListQueryParams(arguments);

            // Use nested resource endpoint for listing contact fields
            String endpoint = "/contacts/" + contactId + "/contactfields";

            return monicaClient.get(endpoint, queryParams)
                .map(this::formatListResponse)
                .doOnSuccess(result -> log.info("Contact fields listed successfully for contact: {}", contactId))
                .doOnError(error -> log.error("Failed to list contact fields: {}", error.getMessage()));

        } catch (Exception e) {
            log.error("Error listing contact fields: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Lists contact fields for a specific contact (alias for listContactFields with consistent naming).
     * <p>
     * This method provides a consistent naming convention with other contact-scoped list operations
     * (activity_list_by_contact, address_list_by_contact, etc.) while delegating to listContactFields.
     * </p>
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The ID of the contact to list fields for</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including required contactId and optional pagination
     * @return a Mono containing the list of contact fields and pagination metadata
     */
    public Mono<Map<String, Object>> listContactFieldsByContact(Map<String, Object> arguments) {
        return listContactFields(arguments);
    }

    /**
     * Extracts the contact ID from the arguments.
     *
     * @param arguments the arguments containing contactId
     * @return the extracted contact ID
     * @throws IllegalArgumentException if contactId is missing or invalid
     */
    private Long extractContactId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("contactId")) {
            throw new IllegalArgumentException("contactId is required - please provide the ID of an existing contact to list its fields");
        }

        Object idValue = arguments.get("contactId");
        if (idValue == null) {
            throw new IllegalArgumentException("contactId is required");
        }

        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }

        try {
            return Long.parseLong(idValue.toString().trim());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid contact ID format: " + idValue);
        }
    }
}
