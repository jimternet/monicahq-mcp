package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.GenderFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for discovering available gender options from MonicaHQ API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard operation implementations.
 * Uses {@link GenderFieldMappingConfig} for Gender-specific field mappings.
 * </p>
 * <p>
 * Implements Constitutional Principle VII: API Discovery and Completeness.
 * This discovery tool eliminates the need to hardcode gender values.
 * </p>
 * <p>
 * This is a read-only service - only list operation is supported.
 * Genders are system-defined lookup values.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>listGenders - List all available genders</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class GenderService extends AbstractCrudService<Object> {

    private final GenderFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a GenderService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Genders
     */
    public GenderService(MonicaHqClient monicaClient,
                         ContentFormatter contentFormatter,
                         GenderFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Lists all available genders from MonicaHQ API.
     * This discovery tool eliminates the need to hardcode gender values.
     *
     * @param arguments optional pagination arguments (page, limit)
     * @return a Mono containing the list of genders
     */
    public Mono<Map<String, Object>> listGenders(Map<String, Object> arguments) {
        return list(arguments);
    }
}
