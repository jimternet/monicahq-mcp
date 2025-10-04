package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.dto.Activity;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class ActivityService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> createActivity(Map<String, Object> arguments) {
        log.info("Creating activity with arguments: {}", arguments);
        
        try {
            validateActivityCreateArguments(arguments);
            Map<String, Object> apiRequest = mapToApiFormat(arguments);
            
            return monicaClient.post("/activities", apiRequest)
                .map(this::formatActivityResponse)
                .doOnSuccess(result -> log.info("Activity created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create activity: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for activity creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getActivity(Map<String, Object> arguments) {
        log.info("Getting activity with arguments: {}", arguments);
        
        try {
            Long activityId = extractActivityId(arguments);
            
            return monicaClient.get("/activities/" + activityId, null)
                .map(this::formatActivityResponse)
                .doOnSuccess(result -> log.info("Activity retrieved successfully: {}", activityId))
                .doOnError(error -> log.error("Failed to get activity {}: {}", activityId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for activity retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateActivity(Map<String, Object> arguments) {
        log.info("Updating activity with arguments: {}", arguments);
        
        try {
            Long activityId = extractActivityId(arguments);
            
            Map<String, Object> updateData = new HashMap<>(arguments);
            updateData.remove("id");
            
            Map<String, Object> apiRequest = mapToApiFormat(updateData);
            
            return monicaClient.put("/activities/" + activityId, apiRequest)
                .map(this::formatActivityResponse)
                .doOnSuccess(result -> log.info("Activity updated successfully: {}", activityId))
                .doOnError(error -> log.error("Failed to update activity {}: {}", activityId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for activity update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteActivity(Map<String, Object> arguments) {
        log.info("Deleting activity with arguments: {}", arguments);
        
        try {
            Long activityId = extractActivityId(arguments);
            
            return monicaClient.delete("/activities/" + activityId)
                .map(response -> {
                    String formattedContent = contentFormatter.formatOperationResult(
                        "Delete", "Activity", activityId, true, 
                        "Activity with ID " + activityId + " has been deleted successfully"
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
                .doOnSuccess(result -> log.info("Activity deleted successfully: {}", activityId))
                .doOnError(error -> log.error("Failed to delete activity {}: {}", activityId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for activity deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listActivities(Map<String, Object> arguments) {
        log.info("Listing activities with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/activities", queryParams)
                .map(this::formatActivityListResponse)
                .doOnSuccess(result -> log.info("Activities listed successfully"))
                .doOnError(error -> log.error("Failed to list activities: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error building query parameters: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateActivityCreateArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("Activity creation arguments cannot be empty");
        }
        
        if (!arguments.containsKey("summary") || 
            arguments.get("summary") == null || 
            arguments.get("summary").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("summary is required");
        }
        
        if (!arguments.containsKey("attendees") || arguments.get("attendees") == null) {
            throw new IllegalArgumentException("attendees is required");
        }
        
        // Validate attendees format
        Object attendees = arguments.get("attendees");
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

    private Long extractActivityId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("id")) {
            throw new IllegalArgumentException("Activity ID is required");
        }
        
        Object idValue = arguments.get("id");
        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }
        
        try {
            return Long.parseLong(idValue.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid activity ID format: " + idValue);
        }
    }

    private Map<String, Object> mapToApiFormat(Map<String, Object> arguments) {
        Map<String, Object> apiRequest = new HashMap<>();
        
        arguments.forEach((key, value) -> {
            switch (key) {
                case "happenedAt" -> apiRequest.put("happened_at", value);
                case "attendees" -> {
                    if (value instanceof List) {
                        @SuppressWarnings("unchecked")
                        List<?> attendeeList = (List<?>) value;
                        List<Map<String, Object>> formattedAttendees = attendeeList.stream()
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
                        apiRequest.put("attendees", formattedAttendees);
                    } else {
                        apiRequest.put("attendees", value);
                    }
                }
                default -> apiRequest.put(key, value);
            }
        });
        
        return apiRequest;
    }

    private Map<String, String> buildListQueryParams(Map<String, Object> arguments) {
        Map<String, String> queryParams = new HashMap<>();
        
        if (arguments.containsKey("page")) {
            queryParams.put("page", arguments.get("page").toString());
        } else {
            queryParams.put("page", "1");
        }
        
        if (arguments.containsKey("limit")) {
            int limit = Math.min(100, Math.max(1, Integer.parseInt(arguments.get("limit").toString())));
            queryParams.put("limit", String.valueOf(limit));
        } else {
            queryParams.put("limit", "10");
        }
        
        if (arguments.containsKey("contactId") && arguments.get("contactId") != null) {
            queryParams.put("contacts", arguments.get("contactId").toString());
        }
        
        return queryParams;
    }

    private Map<String, Object> formatActivityResponse(Map<String, Object> apiResponse) {
        Map<String, Object> activityData;
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            activityData = mapFromApiFormat(rawData);
        } else {
            activityData = mapFromApiFormat(apiResponse);
        }
        
        // Use raw API data for complete field coverage as per Constitutional Principle VI
        Map<String, Object> rawApiData = apiResponse.containsKey("data") ? 
            (Map<String, Object>) apiResponse.get("data") : apiResponse;
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        // Return both data and content fields for protocol compliance
        Map<String, Object> result = new HashMap<>();
        result.put("data", activityData);
        
        // Format content for Claude Desktop visibility
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> formatActivityListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> activities = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedActivities = activities.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        // Format content as escaped JSON for Claude Desktop accessibility as per Constitutional Principle VI
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedActivities);
        
        // Extract and preserve meta from API response
        @SuppressWarnings("unchecked")
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

    private Map<String, Object> mapFromApiFormat(Map<String, Object> apiData) {
        Map<String, Object> result = new HashMap<>();
        
        apiData.forEach((key, value) -> {
            switch (key) {
                case "contact_id" -> result.put("contactId", value);
                case "activity_type_id" -> result.put("activityTypeId", value);
                case "happened_at" -> result.put("happenedAt", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                case "attendees" -> {
                    if (value instanceof List) {
                        @SuppressWarnings("unchecked")
                        List<Map<String, Object>> attendeeList = (List<Map<String, Object>>) value;
                        List<Map<String, Object>> formattedAttendees = attendeeList.stream()
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
                        result.put("attendees", formattedAttendees);
                    } else {
                        result.put("attendees", value);
                    }
                }
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
    
    private Activity convertToActivityDto(Map<String, Object> activityData) {
        return Activity.builder()
            .id(getLongValue(activityData, "id"))
            .contactId(getLongValue(activityData, "contactId"))
            .type((String) activityData.get("type"))
            .summary((String) activityData.get("summary"))
            .description((String) activityData.get("description"))
            .date(getLocalDateTimeValue(activityData, "date"))
            .duration(getIntegerValue(activityData, "duration"))
            .createdAt(getLocalDateTimeValue(activityData, "createdAt"))
            .updatedAt(getLocalDateTimeValue(activityData, "updatedAt"))
            .build();
    }
    
    private Long getLongValue(Map<String, Object> data, String key) {
        Object value = data.get(key);
        if (value instanceof Number) {
            return ((Number) value).longValue();
        }
        return null;
    }
    
    private Integer getIntegerValue(Map<String, Object> data, String key) {
        Object value = data.get(key);
        if (value instanceof Number) {
            return ((Number) value).intValue();
        }
        return null;
    }
    
    private java.time.LocalDateTime getLocalDateTimeValue(Map<String, Object> data, String key) {
        Object value = data.get(key);
        if (value instanceof String) {
            try {
                String dateTimeStr = (String) value;
                // Handle ISO format with Z
                if (dateTimeStr.endsWith("Z")) {
                    return java.time.LocalDateTime.parse(dateTimeStr.substring(0, dateTimeStr.length() - 1));
                }
                return java.time.LocalDateTime.parse(dateTimeStr);
            } catch (Exception e) {
                return null;
            }
        }
        return null;
    }
}