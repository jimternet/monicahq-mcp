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

    // ========================================================================================
    // CUSTOM VALIDATION
    // ========================================================================================

    /**
     * Validates activity creation arguments with special attendees handling.
     * <p>
     * The base class validates required fields presence, but activities need
     * additional validation:
     * <ul>
     *   <li>summary must be a non-empty string</li>
     *   <li>attendees must be an array</li>
     *   <li>attendees array cannot be empty</li>
     *   <li>each attendee must be a string or object with contactId</li>
     * </ul>
     * </p>
     *
     * @param arguments the arguments to validate
     * @throws IllegalArgumentException if validation fails
     */
    private void validateActivityCreateArguments(Map<String, Object> arguments) {
        // Validate summary is a non-empty string
        validateRequiredString(arguments, "summary");

        // Validate attendees format (beyond just presence check)
        Object attendees = arguments.get("attendees");
        if (attendees == null) {
            throw new IllegalArgumentException("attendees is required");
        }

        if (attendees instanceof List) {
            @SuppressWarnings("unchecked")
            List<?> attendeeList = (List<?>) attendees;
            if (attendeeList.isEmpty()) {
                throw new IllegalArgumentException("attendees cannot be empty");
            }

            // Validate each attendee format
            for (Object attendee : attendeeList) {
                if (attendee instanceof Map) {
                    // Object format validation - should have contactId
                    @SuppressWarnings("unchecked")
                    Map<String, Object> attendeeMap = (Map<String, Object>) attendee;
                    if (!attendeeMap.containsKey("contactId")) {
                        throw new IllegalArgumentException("Invalid attendees format: object must contain 'contactId' field");
                    }
                } else if (attendee instanceof String) {
                    // String format validation - should not be empty
                    if (((String) attendee).trim().isEmpty()) {
                        throw new IllegalArgumentException("Invalid attendees format: attendee name cannot be empty");
                    }
                } else if (attendee instanceof Number || attendee instanceof Boolean) {
                    // Numbers and booleans are allowed - will be converted to string
                    continue;
                } else {
                    // Other types are not allowed
                    throw new IllegalArgumentException("Invalid attendees format: attendee must be a string or object with contactId, got: " + attendee.getClass().getSimpleName());
                }
            }
        } else {
            throw new IllegalArgumentException("attendees must be an array");
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
            if ("attendees".equals(key)) {
                apiRequest.put("attendees", transformAttendeesToApi(value));
            } else {
                // Use parent's mapping for other fields
                String apiKey = getFieldMappingConfig().getToApiMappings().getOrDefault(key, key);
                apiRequest.put(apiKey, value);
            }
        });

        return apiRequest;
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
     * Handles three attendee formats:
     * <ul>
     *   <li>Object with contactId: maps contactId to contact_id</li>
     *   <li>String: wraps in object with name field</li>
     *   <li>Number/Boolean: converts to string and wraps with name field</li>
     * </ul>
     * </p>
     *
     * @param value the attendees value (expected to be a List)
     * @return transformed attendees list for API
     */
    private Object transformAttendeesToApi(Object value) {
        if (value instanceof List) {
            @SuppressWarnings("unchecked")
            List<?> attendeeList = (List<?>) value;
            return attendeeList.stream()
                .map(attendee -> {
                    Map<String, Object> formatted = new HashMap<>();

                    // Handle object format: {"contactId": 123}
                    if (attendee instanceof Map) {
                        @SuppressWarnings("unchecked")
                        Map<String, Object> attendeeMap = (Map<String, Object>) attendee;
                        if (attendeeMap.containsKey("contactId")) {
                            formatted.put("contact_id", attendeeMap.get("contactId"));
                        }
                        // Copy other properties as-is
                        attendeeMap.forEach((k, v) -> {
                            if (!"contactId".equals(k)) {
                                formatted.put(k, v);
                            }
                        });
                    }
                    // Handle string format: "John Doe"
                    else if (attendee instanceof String) {
                        formatted.put("name", attendee);
                    }
                    // Handle other types by converting to string
                    else {
                        formatted.put("name", attendee.toString());
                    }

                    return formatted;
                })
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
