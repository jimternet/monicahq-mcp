package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.ContactTagFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.*;

/**
 * Service for managing ContactTag entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} for utility methods but overrides all operations
 * to handle nested resource endpoints. ContactTag operations don't follow standard CRUD patterns.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>attachTag - POST /contacts/{contactId}/setTags - Attach a tag to a contact</li>
 *   <li>getContactTags - GET /contacts/{contactId}/tags - Get all tags for a contact</li>
 *   <li>updateContactTags - PUT /contacts/{contactId}/setTags - Update tags for a contact</li>
 *   <li>detachTag - DELETE /contacts/{contactId}/unsetTag/{tagId} - Detach a tag from a contact</li>
 *   <li>listContactsByTag - GET /contacts?tags={tagId} - List contacts with a specific tag</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class ContactTagService extends AbstractCrudService<Object> {

    private final ContactTagFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a ContactTagService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for ContactTags
     */
    public ContactTagService(MonicaHqClient monicaClient,
                             ContentFormatter contentFormatter,
                             ContactTagFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Attaches a tag to a contact.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The ID of the contact</li>
     *   <li>tagId - The ID of the tag to attach</li>
     * </ul>
     * </p>
     *
     * @param arguments the arguments containing contactId and tagId
     * @return a Mono containing the result of the attach operation
     */
    public Mono<Map<String, Object>> attachTag(Map<String, Object> arguments) {
        log.info("Attaching tag to contact with arguments: {}", arguments);

        try {
            validateAttachArguments(arguments);
            Long contactId = extractContactId(arguments);
            Long tagId = extractTagId(arguments);

            // Fetch tag details to get the tag name (MonicaHQ API expects tag names, not IDs)
            return monicaClient.get("/tags/" + tagId, null)
                .flatMap(tagResponse -> {
                    @SuppressWarnings("unchecked")
                    Map<String, Object> tagData = (Map<String, Object>) tagResponse.get("data");
                    String tagName = (String) tagData.get("name");

                    log.info("Resolved tag ID {} to name: {}", tagId, tagName);

                    // Build request with tag name instead of ID
                    Map<String, Object> apiRequest = new HashMap<>();
                    apiRequest.put("tags", List.of(tagName));

                    return monicaClient.post("/contacts/" + contactId + "/setTags", apiRequest);
                })
                .map(this::formatSingleResponse)
                .doOnSuccess(result -> log.info("Tag attached to contact successfully"))
                .doOnError(error -> log.error("Failed to attach tag to contact: {}", error.getMessage()));

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact tag attachment: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Gets all tags associated with a contact.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The ID of the contact</li>
     * </ul>
     * </p>
     *
     * @param arguments the arguments containing contactId
     * @return a Mono containing the list of tags for the contact
     */
    public Mono<Map<String, Object>> getContactTags(Map<String, Object> arguments) {
        log.info("Getting contact tags with arguments: {}", arguments);

        try {
            Long contactId = extractContactId(arguments);

            return monicaClient.get("/contacts/" + contactId + "/tags", null)
                .map(this::formatContactTagListResponse)
                .doOnSuccess(result -> log.info("Contact tags retrieved successfully: {}", contactId))
                .doOnError(error -> log.error("Failed to get contact tags for {}: {}", contactId, error.getMessage()));

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact tag retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Updates tags for a contact (replaces all existing tags).
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The ID of the contact</li>
     *   <li>tagId - The ID of the tag to set</li>
     * </ul>
     * </p>
     *
     * @param arguments the arguments containing contactId and tagId
     * @return a Mono containing the result of the update operation
     */
    public Mono<Map<String, Object>> updateContactTags(Map<String, Object> arguments) {
        log.info("Updating contact tags with arguments: {}", arguments);

        try {
            Long contactId = extractContactId(arguments);
            Long tagId = extractTagId(arguments);

            // Fetch tag details to get the tag name (MonicaHQ API expects tag names, not IDs)
            return monicaClient.get("/tags/" + tagId, null)
                .flatMap(tagResponse -> {
                    @SuppressWarnings("unchecked")
                    Map<String, Object> tagData = (Map<String, Object>) tagResponse.get("data");
                    String tagName = (String) tagData.get("name");

                    log.info("Resolved tag ID {} to name: {}", tagId, tagName);

                    // Build request with tag name instead of ID
                    Map<String, Object> apiRequest = new HashMap<>();
                    apiRequest.put("tags", List.of(tagName));

                    return monicaClient.put("/contacts/" + contactId + "/setTags", apiRequest);
                })
                .map(this::formatSingleResponse)
                .doOnSuccess(result -> log.info("Contact tags updated successfully: {}", contactId))
                .doOnError(error -> log.error("Failed to update contact tags for {}: {}", contactId, error.getMessage()));

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact tag update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Detaches a tag from a contact.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The ID of the contact</li>
     *   <li>tagId - The ID of the tag to detach</li>
     * </ul>
     * </p>
     *
     * @param arguments the arguments containing contactId and tagId
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> detachTag(Map<String, Object> arguments) {
        log.info("Detaching tag from contact with arguments: {}", arguments);

        try {
            Long contactId = extractContactId(arguments);
            Long tagId = extractTagId(arguments);

            // Monica API has no individual tag removal endpoint (DELETE /contacts/{id}/unsetTag/{tagId} returns 404).
            // Strategy: fetch current tags, filter out the target tag, then POST remaining names to setTags.
            return monicaClient.get("/contacts/" + contactId + "/tags", null)
                .flatMap(tagsResponse -> {
                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> currentTags = (List<Map<String, Object>>) tagsResponse.get("data");
                    if (currentTags == null) {
                        currentTags = List.of();
                    }

                    // Filter out the tag to remove, collect remaining names
                    final Long tagIdFinal = tagId;
                    List<String> remainingTagNames = currentTags.stream()
                        .filter(tag -> {
                            Object id = tag.get("id");
                            if (id instanceof Number) {
                                return !tagIdFinal.equals(((Number) id).longValue());
                            }
                            return !tagIdFinal.toString().equals(String.valueOf(id));
                        })
                        .map(tag -> (String) tag.get("name"))
                        .filter(Objects::nonNull)
                        .toList();

                    log.info("Removing tag {} from contact {}. Remaining tags: {}", tagId, contactId, remainingTagNames);

                    Map<String, Object> apiRequest = new HashMap<>();
                    apiRequest.put("tags", remainingTagNames);
                    return monicaClient.post("/contacts/" + contactId + "/setTags", apiRequest);
                })
                .map(response -> {
                    String formattedContent = contentFormatter.formatOperationResult(
                        "Detach", "Contact Tag", tagId, true,
                        "Tag has been successfully detached from contact " + contactId
                    );

                    Map<String, Object> result = new HashMap<>();
                    List<Map<String, Object>> content = List.of(
                        Map.of(
                            "type", "text",
                            "text", formattedContent
                        )
                    );
                    result.put("content", content);
                    return result;
                })
                .doOnSuccess(result -> log.info("Tag detached from contact successfully: contact {} tag {}", contactId, tagId))
                .doOnError(error -> log.error("Failed to detach tag {} from contact {}: {}", tagId, contactId, error.getMessage()));

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact tag detachment: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Lists contacts that have a specific tag.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>tagId - The ID of the tag to filter by</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the arguments containing tagId and optional pagination
     * @return a Mono containing the list of contacts with the specified tag
     */
    public Mono<Map<String, Object>> listContactsByTag(Map<String, Object> arguments) {
        log.info("Listing contacts by tag with arguments: {}", arguments);

        try {
            Long tagId = extractTagId(arguments);
            Map<String, String> queryParams = buildListQueryParams(arguments);
            queryParams.put("tags", tagId.toString());

            return monicaClient.get("/contacts", queryParams)
                .map(this::formatContactTagListResponse)
                .doOnSuccess(result -> log.info("Contacts listed by tag successfully"))
                .doOnError(error -> log.error("Failed to list contacts by tag: {}", error.getMessage()));

        } catch (Exception e) {
            log.error("Error listing contacts by tag: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Validates arguments for attach operation.
     */
    private void validateAttachArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("Contact Tag arguments cannot be empty");
        }

        if (!arguments.containsKey("contactId") || arguments.get("contactId") == null) {
            throw new IllegalArgumentException("contactId is required");
        }

        if (!arguments.containsKey("tagId") || arguments.get("tagId") == null) {
            throw new IllegalArgumentException("tagId is required");
        }
    }

    /**
     * Extracts the contact ID from the arguments.
     */
    private Long extractContactId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("contactId")) {
            throw new IllegalArgumentException("Contact ID is required");
        }

        Object idValue = arguments.get("contactId");
        if (idValue == null) {
            throw new IllegalArgumentException("Contact ID is required");
        }

        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }

        try {
            return Long.parseLong(idValue.toString().trim());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid contact ID format: " + idValue);
        }
    }

    /**
     * Extracts the tag ID from the arguments.
     */
    private Long extractTagId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("tagId")) {
            throw new IllegalArgumentException("Tag ID is required");
        }

        Object idValue = arguments.get("tagId");
        if (idValue == null) {
            throw new IllegalArgumentException("Tag ID is required");
        }

        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }

        try {
            return Long.parseLong(idValue.toString().trim());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid tag ID format: " + idValue);
        }
    }

    /**
     * Builds the API request for setting tags.
     * The Monica API expects tags as an array.
     */
    private Map<String, Object> buildTagsRequest(Map<String, Object> arguments) {
        Map<String, Object> apiRequest = new HashMap<>();

        // Convert tagId to tags array format for MonicaHQ API
        if (arguments.containsKey("tagId")) {
            apiRequest.put("tags", List.of(arguments.get("tagId")));
        }

        // Add other fields as needed (excluding contactId and tagId)
        arguments.forEach((key, value) -> {
            if (!"tagId".equals(key) && !"contactId".equals(key)) {
                apiRequest.put(key, value);
            }
        });

        return apiRequest;
    }

    /**
     * Formats a list response for contact tags or contacts by tag.
     */
    @SuppressWarnings("unchecked")
    private Map<String, Object> formatContactTagListResponse(Map<String, Object> apiResponse) {
        List<Map<String, Object>> data = (List<Map<String, Object>>) apiResponse.get("data");
        if (data == null) {
            data = List.of();
        }

        // Extract meta for result structure
        Map<String, Object> meta = (Map<String, Object>) apiResponse.get("meta");

        // Format content for Claude Desktop visibility
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);

        // Return both data and content fields for protocol compliance
        Map<String, Object> result = new HashMap<>();
        result.put("data", data);

        if (meta != null) {
            result.put("meta", meta);
        }

        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);

        return result;
    }
}
