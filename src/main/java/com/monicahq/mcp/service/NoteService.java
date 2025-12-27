package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.NoteFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Note entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link NoteFieldMappingConfig} for Note-specific field mappings and validation.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createNote - Create a new note for a contact</li>
 *   <li>getNote - Retrieve a note by ID</li>
 *   <li>updateNote - Update an existing note</li>
 *   <li>deleteNote - Delete a note by ID</li>
 *   <li>listNotes - List notes with optional filtering and pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class NoteService extends AbstractCrudService<Object> {

    private final NoteFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a NoteService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Notes
     */
    public NoteService(MonicaHqClient monicaClient,
                       ContentFormatter contentFormatter,
                       NoteFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new note for a contact.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The ID of the contact to associate the note with</li>
     *   <li>body - The content of the note</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>isFavorited - Whether to mark the note as a favorite (default: false)</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created note data
     */
    public Mono<Map<String, Object>> createNote(Map<String, Object> arguments) {
        return create(arguments);
    }

    /**
     * Retrieves a note by its ID.
     *
     * @param arguments map containing "id" - the note ID to retrieve
     * @return a Mono containing the note data
     */
    public Mono<Map<String, Object>> getNote(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing note.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the note to update</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>body - New content for the note</li>
     *   <li>contactId - New contact association</li>
     *   <li>isFavorited - Update favorite status</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the note ID
     * @return a Mono containing the updated note data
     */
    public Mono<Map<String, Object>> updateNote(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes a note by its ID.
     *
     * @param arguments map containing "id" - the note ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteNote(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists notes with optional filtering and pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     *   <li>contactId - Filter by contact ID</li>
     *   <li>favorited - Filter by favorite status</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional filters and pagination
     * @return a Mono containing the list of notes and pagination metadata
     */
    public Mono<Map<String, Object>> listNotes(Map<String, Object> arguments) {
        return list(arguments);
    }
}
