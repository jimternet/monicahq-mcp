package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.DebtFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Debt entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link DebtFieldMappingConfig} for Debt-specific field mappings and validation.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createDebt - Create a new debt record for a contact</li>
 *   <li>getDebt - Retrieve a debt by ID</li>
 *   <li>updateDebt - Update an existing debt</li>
 *   <li>deleteDebt - Delete a debt by ID</li>
 *   <li>listDebts - List debts with optional filtering and pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class DebtService extends AbstractCrudService<Object> {

    private final DebtFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a DebtService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Debts
     */
    public DebtService(MonicaHqClient monicaClient,
                       ContentFormatter contentFormatter,
                       DebtFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new debt record for a contact.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The ID of the contact to associate the debt with</li>
     *   <li>amount - The monetary amount of the debt</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>currency - Currency code (e.g., USD, EUR)</li>
     *   <li>inDebt - "yes" if you owe them, "no" if they owe you</li>
     *   <li>status - Debt status (inprogress, complete)</li>
     *   <li>reason - Description of what the debt is for</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created debt data
     */
    public Mono<Map<String, Object>> createDebt(Map<String, Object> arguments) {
        return create(arguments);
    }

    /**
     * Retrieves a debt by its ID.
     *
     * @param arguments map containing "id" - the debt ID to retrieve
     * @return a Mono containing the debt data
     */
    public Mono<Map<String, Object>> getDebt(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing debt.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the debt to update</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>contactId - New contact association</li>
     *   <li>amount - Updated monetary amount</li>
     *   <li>currency - Updated currency code</li>
     *   <li>inDebt - Updated debt direction</li>
     *   <li>status - Updated status</li>
     *   <li>reason - Updated description</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the debt ID
     * @return a Mono containing the updated debt data
     */
    public Mono<Map<String, Object>> updateDebt(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes a debt by its ID.
     *
     * @param arguments map containing "id" - the debt ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteDebt(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists debts with optional filtering and pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     *   <li>contactId - Filter by contact ID</li>
     *   <li>status - Filter by status</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional filters and pagination
     * @return a Mono containing the list of debts and pagination metadata
     */
    public Mono<Map<String, Object>> listDebts(Map<String, Object> arguments) {
        return list(arguments);
    }
}
