package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.CompanyFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Company entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link CompanyFieldMappingConfig} for Company-specific field mappings and validation.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createCompany - Create a new company</li>
 *   <li>getCompany - Retrieve a company by ID</li>
 *   <li>updateCompany - Update an existing company</li>
 *   <li>deleteCompany - Delete a company by ID</li>
 *   <li>listCompanies - List companies with optional pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class CompanyService extends AbstractCrudService<Object> {

    private final CompanyFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a CompanyService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Companies
     */
    public CompanyService(MonicaHqClient monicaClient,
                          ContentFormatter contentFormatter,
                          CompanyFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new company.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>name - The name of the company (must be non-empty)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>website - Company website URL</li>
     *   <li>numberOfEmployees - Number of employees</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created company data
     */
    public Mono<Map<String, Object>> createCompany(Map<String, Object> arguments) {
        // Validate name is non-empty string before delegating to base class
        if (arguments != null && !arguments.isEmpty()) {
            validateRequiredString(arguments, "name");
        }
        return create(arguments);
    }

    /**
     * Retrieves a company by its ID.
     *
     * @param arguments map containing "id" - the company ID to retrieve
     * @return a Mono containing the company data
     */
    public Mono<Map<String, Object>> getCompany(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing company.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the company to update</li>
     *   <li>name - The name of the company (must be non-empty)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>website - Company website URL</li>
     *   <li>numberOfEmployees - Number of employees</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the company ID
     * @return a Mono containing the updated company data
     */
    public Mono<Map<String, Object>> updateCompany(Map<String, Object> arguments) {
        // Validate name is non-empty string before delegating to base class
        if (arguments != null && !arguments.isEmpty()) {
            validateRequiredString(arguments, "name");
        }
        return update(arguments);
    }

    /**
     * Deletes a company by its ID.
     *
     * @param arguments map containing "id" - the company ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteCompany(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists companies with optional pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional pagination
     * @return a Mono containing the list of companies and pagination metadata
     */
    public Mono<Map<String, Object>> listCompanies(Map<String, Object> arguments) {
        return list(arguments);
    }
}
