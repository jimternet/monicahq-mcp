package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.DocumentFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Document entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link DocumentFieldMappingConfig} for Document-specific field mappings and validation.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createDocument - Create a new document for a contact</li>
 *   <li>getDocument - Retrieve a document by ID</li>
 *   <li>updateDocument - Update an existing document</li>
 *   <li>deleteDocument - Delete a document by ID</li>
 *   <li>listDocuments - List documents with optional pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class DocumentService extends AbstractCrudService<Object> {

    private final DocumentFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a DocumentService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Documents
     */
    public DocumentService(MonicaHqClient monicaClient,
                           ContentFormatter contentFormatter,
                           DocumentFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new document for a contact.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The ID of the contact to associate the document with</li>
     *   <li>filename - The filename of the document (cannot be empty)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>originalFilename - The original filename before upload</li>
     *   <li>mimeType - The MIME type of the document (e.g., application/pdf)</li>
     *   <li>size - The size of the file in bytes</li>
     *   <li>description - A description of the document</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created document data
     */
    public Mono<Map<String, Object>> createDocument(Map<String, Object> arguments) {
        // Additional validation: filename must be a non-empty string
        if (arguments != null && !arguments.isEmpty()) {
            validateRequiredString(arguments, "filename");
        }
        return create(arguments);
    }

    /**
     * Retrieves a document by its ID.
     *
     * @param arguments map containing "id" - the document ID to retrieve
     * @return a Mono containing the document data
     */
    public Mono<Map<String, Object>> getDocument(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing document.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the document to update</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>contactId - New contact association</li>
     *   <li>filename - New filename</li>
     *   <li>originalFilename - New original filename</li>
     *   <li>mimeType - New MIME type</li>
     *   <li>size - New file size</li>
     *   <li>description - New description</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the document ID
     * @return a Mono containing the updated document data
     */
    public Mono<Map<String, Object>> updateDocument(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes a document by its ID.
     *
     * @param arguments map containing "id" - the document ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteDocument(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists documents with optional pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional pagination
     * @return a Mono containing the list of documents and pagination metadata
     */
    public Mono<Map<String, Object>> listDocuments(Map<String, Object> arguments) {
        return list(arguments);
    }

    public Mono<Map<String, Object>> listDocumentsByContact(Map<String, Object> arguments) {
        try {
            if (arguments == null || !arguments.containsKey("contactId")) {
                throw new IllegalArgumentException("contactId is required");
            }
            Object contactIdValue = arguments.get("contactId");
            if (contactIdValue == null) {
                throw new IllegalArgumentException("contactId is required");
            }
            Long contactId = contactIdValue instanceof Number
                ? ((Number) contactIdValue).longValue()
                : Long.parseLong(contactIdValue.toString().trim());

            int page = 1;
            int limit = 10;
            if (arguments.containsKey("page")) {
                page = Integer.parseInt(arguments.get("page").toString());
            }
            if (arguments.containsKey("limit")) {
                limit = parseLimit(arguments.get("limit"));
            }

            String endpoint = "/contacts/" + contactId + "/documents";
            Map<String, String> queryParams = Map.of(
                "page", String.valueOf(page),
                "limit", String.valueOf(limit)
            );

            return monicaClient.get(endpoint, queryParams)
                .map(this::formatListResponse)
                .doOnSuccess(result -> log.info("Documents for contact {} listed successfully", contactId))
                .doOnError(error -> log.error("Failed to list documents for contact {}: {}", contactId, error.getMessage()));
        } catch (IllegalArgumentException e) {
            return Mono.error(new IllegalArgumentException("Invalid arguments: " + e.getMessage()));
        }
    }
}
