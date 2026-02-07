package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.GiftFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Gift entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link GiftFieldMappingConfig} for Gift-specific field mappings and validation.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createGift - Create a new gift for a contact</li>
 *   <li>getGift - Retrieve a gift by ID</li>
 *   <li>updateGift - Update an existing gift</li>
 *   <li>deleteGift - Delete a gift by ID</li>
 *   <li>listGifts - List gifts with optional filtering and pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class GiftService extends AbstractCrudService<Object> {

    private final GiftFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a GiftService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Gifts
     */
    public GiftService(MonicaHqClient monicaClient,
                       ContentFormatter contentFormatter,
                       GiftFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new gift for a contact.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The ID of the contact to associate the gift with</li>
     *   <li>name - The name/title of the gift (cannot be empty)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>comment - Additional notes about the gift</li>
     *   <li>url - Link to the gift (e.g., product page)</li>
     *   <li>value - Monetary value of the gift</li>
     *   <li>status - Gift status (idea, offered, received)</li>
     *   <li>date - Date associated with the gift</li>
     *   <li>isFor - Who the gift is for</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created gift data
     */
    public Mono<Map<String, Object>> createGift(Map<String, Object> arguments) {
        // Additional validation: name must be a non-empty string
        if (arguments != null && !arguments.isEmpty()) {
            validateRequiredString(arguments, "name");
        }
        return create(arguments);
    }

    /**
     * Retrieves a gift by its ID.
     *
     * @param arguments map containing "id" - the gift ID to retrieve
     * @return a Mono containing the gift data
     */
    public Mono<Map<String, Object>> getGift(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing gift.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the gift to update</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>contactId - New contact association</li>
     *   <li>name - Updated name for the gift</li>
     *   <li>comment - Updated notes about the gift</li>
     *   <li>url - Updated link to the gift</li>
     *   <li>value - Updated monetary value</li>
     *   <li>status - Updated status (idea, offered, received)</li>
     *   <li>date - Updated date</li>
     *   <li>isFor - Updated recipient</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the gift ID
     * @return a Mono containing the updated gift data
     */
    public Mono<Map<String, Object>> updateGift(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes a gift by its ID.
     *
     * @param arguments map containing "id" - the gift ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteGift(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists gifts with optional filtering and pagination.
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
     * @return a Mono containing the list of gifts and pagination metadata
     */
    public Mono<Map<String, Object>> listGifts(Map<String, Object> arguments) {
        return list(arguments);
    }

    public Mono<Map<String, Object>> listGiftsByContact(Map<String, Object> arguments) {
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

            String endpoint = "/contacts/" + contactId + "/gifts";
            Map<String, String> queryParams = Map.of(
                "page", String.valueOf(page),
                "limit", String.valueOf(limit)
            );

            return monicaClient.get(endpoint, queryParams)
                .map(this::formatListResponse)
                .doOnSuccess(result -> log.info("Gifts for contact {} listed successfully", contactId))
                .doOnError(error -> log.error("Failed to list gifts for contact {}: {}", contactId, error.getMessage()));
        } catch (IllegalArgumentException e) {
            return Mono.error(new IllegalArgumentException("Invalid arguments: " + e.getMessage()));
        }
    }
}
