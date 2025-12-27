package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.JournalEntryFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Journal Entry entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link JournalEntryFieldMappingConfig} for Journal Entry-specific field mappings and validation.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createJournalEntry - Create a new journal entry</li>
 *   <li>getJournalEntry - Retrieve a journal entry by ID</li>
 *   <li>updateJournalEntry - Update an existing journal entry</li>
 *   <li>deleteJournalEntry - Delete a journal entry by ID</li>
 *   <li>listJournalEntries - List journal entries with optional pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class JournalEntryService extends AbstractCrudService<Object> {

    private final JournalEntryFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a JournalEntryService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Journal Entries
     */
    public JournalEntryService(MonicaHqClient monicaClient,
                               ContentFormatter contentFormatter,
                               JournalEntryFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new journal entry.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>title - The title of the journal entry (cannot be empty)</li>
     *   <li>date - The date for the journal entry in YYYY-MM-DD format</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>post - The main content of the journal entry</li>
     *   <li>journalEntry - Additional notes or reflections</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created journal entry data
     */
    public Mono<Map<String, Object>> createJournalEntry(Map<String, Object> arguments) {
        // Additional validation: title must be a non-empty string
        if (arguments != null && !arguments.isEmpty()) {
            validateRequiredString(arguments, "title");
        }
        return create(arguments);
    }

    /**
     * Retrieves a journal entry by its ID.
     *
     * @param arguments map containing "id" - the journal entry ID to retrieve
     * @return a Mono containing the journal entry data
     */
    public Mono<Map<String, Object>> getJournalEntry(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing journal entry.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the journal entry to update</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>title - New title for the journal entry</li>
     *   <li>date - New date for the journal entry</li>
     *   <li>post - New content for the journal entry</li>
     *   <li>journalEntry - New notes or reflections</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the journal entry ID
     * @return a Mono containing the updated journal entry data
     */
    public Mono<Map<String, Object>> updateJournalEntry(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes a journal entry by its ID.
     *
     * @param arguments map containing "id" - the journal entry ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteJournalEntry(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists journal entries with optional pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional pagination
     * @return a Mono containing the list of journal entries and pagination metadata
     */
    public Mono<Map<String, Object>> listJournalEntries(Map<String, Object> arguments) {
        return list(arguments);
    }
}
