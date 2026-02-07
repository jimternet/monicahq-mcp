package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.ContactFieldTypeFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing contact field types in MonicaHQ API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link ContactFieldTypeFieldMappingConfig} for Contact Field Type-specific field mappings.
 * </p>
 * <p>
 * Implements Constitutional Principle VII: API Discovery and Completeness.
 * Enables users to create and manage custom field type definitions per account.
 * </p>
 * <p>
 * Gap Analysis Phase 2: Extended from read-only to full CRUD support.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createContactFieldType - Create a new custom field type definition</li>
 *   <li>getContactFieldType - Retrieve a specific field type by ID</li>
 *   <li>updateContactFieldType - Modify an existing field type</li>
 *   <li>deleteContactFieldType - Remove a field type definition</li>
 *   <li>listContactFieldTypes - List all available contact field types</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class ContactFieldTypeService extends AbstractCrudService<Object> {

    private final ContactFieldTypeFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a ContactFieldTypeService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Contact Field Types
     */
    public ContactFieldTypeService(MonicaHqClient monicaClient,
                                   ContentFormatter contentFormatter,
                                   ContactFieldTypeFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new custom contact field type.
     * <p>
     * Enables creation of custom field type definitions for flexible contact field management.
     * </p>
     *
     * @param arguments must contain 'name' and 'type'; may contain 'protocol', 'fontawesome_icon', 'delible'
     * @return a Mono containing the created contact field type with full Monica API data
     */
    public Mono<Map<String, Object>> createContactFieldType(Map<String, Object> arguments) {
        return create(arguments);
    }

    /**
     * Retrieves a specific contact field type by ID.
     * <p>
     * Provides detailed information about a field type including metadata and configuration.
     * </p>
     *
     * @param arguments must contain 'id' (the contact field type ID)
     * @return a Mono containing the contact field type details with full Monica API data
     */
    public Mono<Map<String, Object>> getContactFieldType(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing contact field type.
     * <p>
     * Allows modification of field type definitions and properties.
     * </p>
     *
     * @param arguments must contain 'id' (the contact field type ID) and fields to update
     * @return a Mono containing the updated contact field type with full Monica API data
     */
    public Mono<Map<String, Object>> updateContactFieldType(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes a contact field type.
     * <p>
     * Removes a custom field type definition from the account.
     * Note: May fail if the field type is in use by existing contact fields.
     * </p>
     *
     * @param arguments must contain 'id' (the contact field type ID to delete)
     * @return a Mono containing the deletion confirmation
     */
    public Mono<Map<String, Object>> deleteContactFieldType(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists all available contact field types from MonicaHQ API.
     * <p>
     * Includes both system-defined and custom field types.
     * </p>
     *
     * @param arguments optional pagination arguments (page, limit)
     * @return a Mono containing the list of contact field types with full Monica API data
     */
    public Mono<Map<String, Object>> listContactFieldTypes(Map<String, Object> arguments) {
        return list(arguments);
    }
}
