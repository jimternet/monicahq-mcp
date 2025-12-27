package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.TagFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.*;

/**
 * Service for managing Tag entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link TagFieldMappingConfig} for Tag-specific field mappings and validation.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createTag - Create a new tag</li>
 *   <li>getTag - Retrieve a tag by ID</li>
 *   <li>updateTag - Update an existing tag</li>
 *   <li>deleteTag - Delete a tag by ID</li>
 *   <li>listTags - List tags with optional filtering and pagination</li>
 *   <li>listContactsByTag - List contacts associated with a specific tag</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class TagService extends AbstractCrudService<Object> {

    private final TagFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a TagService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Tags
     */
    public TagService(MonicaHqClient monicaClient,
                      ContentFormatter contentFormatter,
                      TagFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new tag.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>name - The name of the tag (must be non-empty)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>nameSlug - URL-friendly version of the tag name</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created tag data
     */
    public Mono<Map<String, Object>> createTag(Map<String, Object> arguments) {
        // Validate name is non-empty string before delegating to base class
        if (arguments != null && !arguments.isEmpty()) {
            validateRequiredString(arguments, "name");
        }
        return create(arguments);
    }

    /**
     * Retrieves a tag by its ID.
     *
     * @param arguments map containing "id" - the tag ID to retrieve
     * @return a Mono containing the tag data
     */
    public Mono<Map<String, Object>> getTag(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing tag.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the tag to update</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>name - New name for the tag</li>
     *   <li>nameSlug - New URL-friendly name slug</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the tag ID
     * @return a Mono containing the updated tag data
     */
    public Mono<Map<String, Object>> updateTag(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes a tag by its ID.
     *
     * @param arguments map containing "id" - the tag ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteTag(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists tags with optional filtering and pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     *   <li>search - Search query to filter tags</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional filters and pagination
     * @return a Mono containing the list of tags and pagination metadata
     */
    public Mono<Map<String, Object>> listTags(Map<String, Object> arguments) {
        return list(arguments);
    }

    /**
     * Lists contacts associated with a specific tag.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The tag ID to get contacts for</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the arguments including tag ID and optional pagination
     * @return a Mono containing the list of contacts and pagination metadata
     */
    public Mono<Map<String, Object>> listContactsByTag(Map<String, Object> arguments) {
        log.info("Listing contacts by tag with arguments: {}", arguments);

        try {
            Long tagId = extractId(arguments);
            Map<String, String> queryParams = buildListQueryParams(arguments);

            return monicaClient.get("/tags/" + tagId + "/contacts", queryParams)
                .map(this::formatContactsByTagResponse)
                .doOnSuccess(result -> log.info("Contacts by tag retrieved successfully: {}", tagId))
                .doOnError(error -> log.error("Failed to get contacts by tag {}: {}", tagId, error.getMessage()));

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contacts by tag: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Formats the contacts by tag response for Claude Desktop visibility.
     *
     * @param apiResponse the raw API response
     * @return the formatted response with data, meta, and content fields
     */
    @SuppressWarnings("unchecked")
    private Map<String, Object> formatContactsByTagResponse(Map<String, Object> apiResponse) {
        List<Map<String, Object>> contacts = (List<Map<String, Object>>) apiResponse.get("data");

        // Format contact data
        List<Map<String, Object>> formattedContacts = contacts.stream()
            .map(this::mapContactFromApiFormat)
            .toList();

        // Format content as escaped JSON for Claude Desktop accessibility
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);

        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedContacts);

        // Extract and preserve meta from API response
        Map<String, Object> meta = (Map<String, Object>) apiResponse.get("meta");
        if (meta != null) {
            result.put("meta", meta);
        }

        // Add content field for Claude Desktop visibility
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);

        return result;
    }

    /**
     * Maps contact data from snake_case (API) to camelCase (client) format.
     *
     * @param apiData the API response contact data
     * @return the contact data with camelCase field names
     */
    private Map<String, Object> mapContactFromApiFormat(Map<String, Object> apiData) {
        Map<String, Object> result = new HashMap<>();

        // Map snake_case to camelCase for contact data
        apiData.forEach((key, value) -> {
            switch (key) {
                case "first_name" -> result.put("firstName", value);
                case "last_name" -> result.put("lastName", value);
                case "gender_id" -> result.put("genderId", value);
                case "is_birthdate_known" -> result.put("isBirthdateKnown", value);
                case "is_deceased" -> result.put("isDeceased", value);
                case "is_deceased_date_known" -> result.put("isDeceasedDateKnown", value);
                case "job_title" -> result.put("jobTitle", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });

        return result;
    }
}
