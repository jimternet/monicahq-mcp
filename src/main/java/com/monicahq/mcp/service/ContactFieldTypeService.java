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
 * Service for discovering available contact field types from MonicaHQ API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard operation implementations.
 * Uses {@link ContactFieldTypeFieldMappingConfig} for Contact Field Type-specific field mappings.
 * </p>
 * <p>
 * Implements Constitutional Principle VII: API Discovery and Completeness.
 * This discovery tool shows valid contactFieldTypeId values for contact field creation.
 * </p>
 * <p>
 * This is a read-only service - only list operation is supported.
 * Contact Field Types are system-defined lookup values that define valid types
 * for contact fields (e.g., email, phone, social media handles).
 * </p>
 * <p>
 * Supported operations:
 * <ul>
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
     * Lists all available contact field types from MonicaHQ API.
     * This discovery tool shows valid contactFieldTypeId values for contact field creation.
     *
     * @param arguments optional pagination arguments (page, limit)
     * @return a Mono containing the list of contact field types
     */
    public Mono<Map<String, Object>> listContactFieldTypes(Map<String, Object> arguments) {
        return list(arguments);
    }
}
