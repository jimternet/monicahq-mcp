package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
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
                    Map<String, Object> result = new HashMap<>();
                    List<Map<String, Object>> content = List.of(
                        Map.of(
                            "type", "text",
                            "text", "Activity with ID " + activityId + " has been deleted successfully"
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
                        List<Map<String, Object>> attendeeList = (List<Map<String, Object>>) value;
                        List<Map<String, Object>> formattedAttendees = attendeeList.stream()
                            .map(attendee -> {
                                Map<String, Object> formatted = new HashMap<>();
                                if (attendee.containsKey("contactId")) {
                                    formatted.put("contact_id", attendee.get("contactId"));
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
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> activityData = (Map<String, Object>) apiResponse.get("data");
            return Map.of(
                "data", mapFromApiFormat(activityData)
            );
        }
        
        return Map.of("data", mapFromApiFormat(apiResponse));
    }

    private Map<String, Object> formatActivityListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> activities = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedActivities = activities.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedActivities);
        
        // Add meta fields directly to result for MCP protocol
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) apiResponse.get("meta");
        if (meta != null) {
            result.put("meta", meta);
        }
        
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
}