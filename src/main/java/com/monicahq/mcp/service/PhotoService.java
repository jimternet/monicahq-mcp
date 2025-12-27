package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.PhotoFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Photo entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link PhotoFieldMappingConfig} for Photo-specific field mappings and validation.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createPhoto - Create a new photo for a contact</li>
 *   <li>getPhoto - Retrieve a photo by ID</li>
 *   <li>updatePhoto - Update an existing photo</li>
 *   <li>deletePhoto - Delete a photo by ID</li>
 *   <li>listPhotos - List photos with optional pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class PhotoService extends AbstractCrudService<Object> {

    private final PhotoFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a PhotoService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Photos
     */
    public PhotoService(MonicaHqClient monicaClient,
                        ContentFormatter contentFormatter,
                        PhotoFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new photo for a contact.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The ID of the contact to associate the photo with</li>
     *   <li>filename - The filename of the photo (cannot be empty)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>originalFilename - The original filename before upload</li>
     *   <li>mimeType - The MIME type of the photo (e.g., image/jpeg)</li>
     *   <li>filesize - The size of the file in bytes</li>
     *   <li>width - The width of the image in pixels</li>
     *   <li>height - The height of the image in pixels</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created photo data
     */
    public Mono<Map<String, Object>> createPhoto(Map<String, Object> arguments) {
        // Additional validation: filename must be a non-empty string
        if (arguments != null && !arguments.isEmpty()) {
            validateRequiredString(arguments, "filename");
        }
        return create(arguments);
    }

    /**
     * Retrieves a photo by its ID.
     *
     * @param arguments map containing "id" - the photo ID to retrieve
     * @return a Mono containing the photo data
     */
    public Mono<Map<String, Object>> getPhoto(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing photo.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the photo to update</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>contactId - New contact association</li>
     *   <li>filename - New filename</li>
     *   <li>originalFilename - New original filename</li>
     *   <li>mimeType - New MIME type</li>
     *   <li>filesize - New file size</li>
     *   <li>width - New width</li>
     *   <li>height - New height</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the photo ID
     * @return a Mono containing the updated photo data
     */
    public Mono<Map<String, Object>> updatePhoto(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes a photo by its ID.
     *
     * @param arguments map containing "id" - the photo ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deletePhoto(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists photos with optional pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional pagination
     * @return a Mono containing the list of photos and pagination metadata
     */
    public Mono<Map<String, Object>> listPhotos(Map<String, Object> arguments) {
        return list(arguments);
    }
}
