package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.ActivityFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.*;

/**
 * Service for managing Activity entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Overrides field mapping methods to handle complex attendees array transformation.
 * </p>
 * <p>
 * Attendees can be specified in two formats:
 * <ul>
 *   <li>Object format: {"contactId": 123} - references an existing contact</li>
 *   <li>String format: "John Doe" - creates an ad-hoc attendee by name</li>
 * </ul>
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createActivity - Create a new activity with attendees</li>
 *   <li>getActivity - Retrieve an activity by ID</li>
 *   <li>updateActivity - Update an existing activity</li>
 *   <li>deleteActivity - Delete an activity by ID</li>
 *   <li>listActivities - List activities with optional filtering and pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class ActivityService extends AbstractCrudService<Object> {

    private final ActivityFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs an ActivityService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Activities
     */
    public ActivityService(MonicaHqClient monicaClient,
                           ContentFormatter contentFormatter,
                           ActivityFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new activity with attendees.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>summary - The summary/title of the activity (non-empty string)</li>
     *   <li>attendees - Array of attendees (object with contactId or string name)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>description - Detailed description of the activity</li>
     *   <li>happenedAt - Date when the activity occurred</li>
     *   <li>activityTypeId - ID of the activity type</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created activity data
     */
    public Mono<Map<String, Object>> createActivity(Map<String, Object> arguments) {
        // Validate arguments before delegating to base class
        // This handles the complex attendees validation that base class doesn't support
        if (arguments != null && !arguments.isEmpty()) {
            validateActivityCreateArguments(arguments);
        }
        return create(arguments);
    }

    /**
     * Retrieves an activity by its ID.
     *
     * @param arguments map containing "id" - the activity ID to retrieve
     * @return a Mono containing the activity data
     */
    public Mono<Map<String, Object>> getActivity(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing activity.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the activity to update</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>summary - New summary for the activity</li>
     *   <li>description - New description</li>
     *   <li>happenedAt - New date</li>
     *   <li>attendees - Updated list of attendees</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the activity ID
     * @return a Mono containing the updated activity data
     */
    public Mono<Map<String, Object>> updateActivity(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes an activity by its ID.
     *
     * @param arguments map containing "id" - the activity ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteActivity(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists activities with optional filtering and pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     *   <li>contactId - Filter by contact ID (activities involving this contact)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional filters and pagination
     * @return a Mono containing the list of activities and pagination metadata
     */
    public Mono<Map<String, Object>> listActivities(Map<String, Object> arguments) {
        return list(arguments);
    }

    /**
     * Lists all activities for a specific contact.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The contact ID to retrieve activities for</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including contactId and optional pagination
     * @return a Mono containing the list of activities for the contact and pagination metadata
     */
    public Mono<Map<String, Object>> listActivitiesByContact(Map<String, Object> arguments) {
        try {
            // Extract contactId parameter
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

            // Extract pagination parameters
            int page = 1;
            int limit = 10;
            if (arguments.containsKey("page")) {
                page = Integer.parseInt(arguments.get("page").toString());
            }
            if (arguments.containsKey("limit")) {
                limit = parseLimit(arguments.get("limit"));
            }

            String endpoint = "/contacts/" + contactId + "/activities";
            Map<String, String> queryParams = Map.of(
                "page", String.valueOf(page),
                "limit", String.valueOf(limit)
            );

            return monicaClient.get(endpoint, queryParams)
                .map(this::formatListResponse)
                .doOnSuccess(result -> log.info("Activities for contact {} listed successfully", contactId))
                .doOnError(error -> log.error("Failed to list activities for contact {}: {}", contactId, error.getMessage()));
        } catch (IllegalArgumentException e) {
            return Mono.error(new IllegalArgumentException("Invalid arguments: " + e.getMessage()));
        }
    }

    // ========================================================================================
    // CUSTOM VALIDATION
    // ========================================================================================

    /**
     * Validates activity creation arguments with special attendees/contactIds handling.
     * <p>
     * The base class validates required fields presence, but activities need
     * additional validation:
     * <ul>
     *   <li>summary must be a non-empty string</li>
     *   <li>attendees OR contactIds must be provided (at least one)</li>
     *   <li>the contact list array cannot be empty</li>
     *   <li>each attendee must be a string, number, or object with contactId</li>
     * </ul>
     * </p>
     *
     * @param arguments the arguments to validate
     * @throws IllegalArgumentException if validation fails
     */
    private void validateActivityCreateArguments(Map<String, Object> arguments) {
        // Validate summary is a non-empty string
        validateRequiredString(arguments, "summary");

        // Accept either 'attendees' or 'contactIds' field name
        Object contacts = arguments.get("attendees");
        String fieldName = "attendees";

        if (contacts == null) {
            contacts = arguments.get("contactIds");
            fieldName = "contactIds";
        }

        if (contacts == null) {
            throw new IllegalArgumentException("Either 'attendees' or 'contactIds' is required");
        }

        if (contacts instanceof List) {
            @SuppressWarnings("unchecked")
            List<?> contactList = (List<?>) contacts;
            if (contactList.isEmpty()) {
                throw new IllegalArgumentException(fieldName + " cannot be empty");
            }

            // Validate each contact format
            for (Object contact : contactList) {
                if (contact instanceof Map) {
                    // Object format validation - should have contactId
                    @SuppressWarnings("unchecked")
                    Map<String, Object> contactMap = (Map<String, Object>) contact;
                    if (!contactMap.containsKey("contactId")) {
                        throw new IllegalArgumentException("Invalid " + fieldName + " format: object must contain 'contactId' field");
                    }
                } else if (contact instanceof String) {
                    // String format validation - should not be empty
                    if (((String) contact).trim().isEmpty()) {
                        throw new IllegalArgumentException("Invalid " + fieldName + " format: contact name cannot be empty");
                    }
                } else if (contact instanceof Number || contact instanceof Boolean) {
                    // Numbers and booleans are allowed - will be converted to contact IDs
                    continue;
                } else {
                    // Other types are not allowed
                    throw new IllegalArgumentException("Invalid " + fieldName + " format: contact must be a string, number, or object with contactId, got: " + contact.getClass().getSimpleName());
                }
            }
        } else {
            throw new IllegalArgumentException(fieldName + " must be an array");
        }
    }

    // ========================================================================================
    // CUSTOM FIELD MAPPING (for attendees array)
    // ========================================================================================

    /**
     * Maps field names from camelCase to snake_case with special attendees handling.
     * <p>
     * Overrides the base implementation to handle attendees array transformation:
     * each attendee object's contactId field is mapped to contact_id.
     * </p>
     *
     * @param arguments the input data with camelCase field names
     * @return a new map with snake_case field names for API consumption
     */
    @Override
    protected Map<String, Object> mapToApiFormat(Map<String, Object> arguments) {
        Map<String, Object> apiRequest = new HashMap<>();

        arguments.forEach((key, value) -> {
            // Handle both 'attendees' and 'contactIds' field names (map both to 'contacts')
            if ("attendees".equals(key) || "contactIds".equals(key)) {
                apiRequest.put("contacts", transformAttendeesToApi(value));
            } else if ("happenedAt".equals(key)) {
                // Monica API expects date-only format (Y-m-d) for happened_at, not datetime
                apiRequest.put("happened_at", convertToDateOnly(value));
            } else {
                // Use parent's mapping for other fields
                String apiKey = getFieldMappingConfig().getToApiMappings().getOrDefault(key, key);
                apiRequest.put(apiKey, value);
            }
        });

        return apiRequest;
    }

    /**
     * Converts a datetime value to date-only format (YYYY-MM-DD).
     * Monica API expects happened_at in Y-m-d format, not ISO datetime.
     *
     * @param value the date/datetime value (String or LocalDateTime)
     * @return date-only string in YYYY-MM-DD format
     */
    private String convertToDateOnly(Object value) {
        if (value == null) {
            return null;
        }

        String strValue = value.toString();
        // If it's an ISO datetime like "2026-02-08T10:00:00Z", extract just the date part
        if (strValue.contains("T")) {
            return strValue.substring(0, strValue.indexOf("T"));
        }
        // If it's already date-only, return as-is
        return strValue;
    }

    /**
     * Maps field names from snake_case to camelCase with special attendees handling.
     * <p>
     * Overrides the base implementation to handle attendees array transformation:
     * each attendee object's contact_id field is mapped to contactId.
     * </p>
     *
     * @param apiData the API response data with snake_case field names
     * @return a new map with camelCase field names for client consumption
     */
    @Override
    protected Map<String, Object> mapFromApiFormat(Map<String, Object> apiData) {
        Map<String, Object> result = new HashMap<>();

        apiData.forEach((key, value) -> {
            if ("attendees".equals(key)) {
                result.put("attendees", transformAttendeesFromApi(value));
            } else {
                // Use parent's mapping for other fields
                String clientKey = getFieldMappingConfig().getFromApiMappings().getOrDefault(key, key);

                // Always map common timestamp fields even if not explicitly configured
                if ("created_at".equals(key) && !getFieldMappingConfig().getFromApiMappings().containsKey(key)) {
                    clientKey = "createdAt";
                } else if ("updated_at".equals(key) && !getFieldMappingConfig().getFromApiMappings().containsKey(key)) {
                    clientKey = "updatedAt";
                }

                result.put(clientKey, value);
            }
        });

        return result;
    }

    /**
     * Transforms attendees array for API format.
     * <p>
     * The Monica API expects a simple array of contact IDs (integers).
     * Handles two attendee formats:
     * <ul>
     *   <li>Object with contactId: extracts just the contact ID as an integer</li>
     *   <li>Number: uses the number directly as the contact ID</li>
     * </ul>
     * </p>
     *
     * @param value the attendees value (expected to be a List)
     * @return array of contact IDs for API (e.g., [91, 73])
     */
    private Object transformAttendeesToApi(Object value) {
        if (value instanceof List) {
            @SuppressWarnings("unchecked")
            List<?> attendeeList = (List<?>) value;
            return attendeeList.stream()
                .map(attendee -> {
                    // Handle object format: {"contactId": 123} -> extract just the ID
                    if (attendee instanceof Map) {
                        @SuppressWarnings("unchecked")
                        Map<String, Object> attendeeMap = (Map<String, Object>) attendee;
                        if (attendeeMap.containsKey("contactId")) {
                            return attendeeMap.get("contactId");
                        }
                    }
                    // Handle number format: 123 -> use directly
                    else if (attendee instanceof Number) {
                        return attendee;
                    }

                    // If we can't extract a contact ID, return null (will be filtered out)
                    return null;
                })
                .filter(id -> id != null)
                .toList();
        }
        return value;
    }

    /**
     * Transforms attendees array from API format.
     * <p>
     * Maps contact_id field to contactId for each attendee object.
     * </p>
     *
     * @param value the attendees value from API (expected to be a List)
     * @return transformed attendees list for client
     */
    private Object transformAttendeesFromApi(Object value) {
        if (value instanceof List) {
            @SuppressWarnings("unchecked")
            List<Map<String, Object>> attendeeList = (List<Map<String, Object>>) value;
            return attendeeList.stream()
                .map(attendee -> {
                    Map<String, Object> formatted = new HashMap<>();
                    attendee.forEach((k, v) -> {
                        if ("contact_id".equals(k)) {
                            formatted.put("contactId", v);
                        } else {
                            formatted.put(k, v);
                        }
                    });
                    return formatted;
                })
                .toList();
        }
        return value;
    }
}
