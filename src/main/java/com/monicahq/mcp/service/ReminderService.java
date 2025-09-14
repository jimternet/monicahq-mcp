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
public class ReminderService {

    private final MonicaHqClient monicaClient;

    public Mono<Map<String, Object>> createReminder(Map<String, Object> arguments) {
        log.info("Creating reminder with arguments: {}", arguments);
        
        try {
            validateReminderCreateArguments(arguments);
            Map<String, Object> apiRequest = mapToApiFormat(arguments);
            
            return monicaClient.post("/reminders", apiRequest)
                .map(this::formatReminderResponse)
                .doOnSuccess(result -> log.info("Reminder created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create reminder: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for reminder creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getReminder(Map<String, Object> arguments) {
        log.info("Getting reminder with arguments: {}", arguments);
        
        try {
            Long reminderId = extractReminderId(arguments);
            
            return monicaClient.get("/reminders/" + reminderId, null)
                .map(this::formatReminderResponse)
                .doOnSuccess(result -> log.info("Reminder retrieved successfully: {}", reminderId))
                .doOnError(error -> log.error("Failed to get reminder {}: {}", reminderId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for reminder retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateReminder(Map<String, Object> arguments) {
        log.info("Updating reminder with arguments: {}", arguments);
        
        try {
            Long reminderId = extractReminderId(arguments);
            
            Map<String, Object> updateData = new HashMap<>(arguments);
            updateData.remove("id");
            
            Map<String, Object> apiRequest = mapToApiFormat(updateData);
            
            return monicaClient.put("/reminders/" + reminderId, apiRequest)
                .map(this::formatReminderResponse)
                .doOnSuccess(result -> log.info("Reminder updated successfully: {}", reminderId))
                .doOnError(error -> log.error("Failed to update reminder {}: {}", reminderId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for reminder update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteReminder(Map<String, Object> arguments) {
        log.info("Deleting reminder with arguments: {}", arguments);
        
        try {
            Long reminderId = extractReminderId(arguments);
            
            return monicaClient.delete("/reminders/" + reminderId)
                .map(response -> {
                    Map<String, Object> result = new HashMap<>();
                    List<Map<String, Object>> content = List.of(
                        Map.of(
                            "type", "text",
                            "text", "Reminder with ID " + reminderId + " has been deleted successfully"
                        )
                    );
                    result.put("content", content);
                    return result;
                })
                .doOnSuccess(result -> log.info("Reminder deleted successfully: {}", reminderId))
                .doOnError(error -> log.error("Failed to delete reminder {}: {}", reminderId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for reminder deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listReminders(Map<String, Object> arguments) {
        log.info("Listing reminders with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/reminders", queryParams)
                .map(this::formatReminderListResponse)
                .doOnSuccess(result -> log.info("Reminders listed successfully"))
                .doOnError(error -> log.error("Failed to list reminders: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error building query parameters: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateReminderCreateArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("Reminder creation arguments cannot be empty");
        }
        
        if (!arguments.containsKey("contactId") || arguments.get("contactId") == null) {
            throw new IllegalArgumentException("contactId is required");
        }
        
        if (!arguments.containsKey("title") || 
            arguments.get("title") == null || 
            arguments.get("title").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("title is required");
        }
        
        if (!arguments.containsKey("nextExpectedDate") || arguments.get("nextExpectedDate") == null) {
            throw new IllegalArgumentException("nextExpectedDate is required");
        }
    }

    private Long extractReminderId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("id")) {
            throw new IllegalArgumentException("Reminder ID is required");
        }
        
        Object idValue = arguments.get("id");
        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }
        
        try {
            return Long.parseLong(idValue.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid reminder ID format: " + idValue);
        }
    }

    private Map<String, Object> mapToApiFormat(Map<String, Object> arguments) {
        Map<String, Object> apiRequest = new HashMap<>();
        
        arguments.forEach((key, value) -> {
            switch (key) {
                case "contactId" -> apiRequest.put("contact_id", value);
                case "nextExpectedDate" -> apiRequest.put("next_expected_date", value);
                case "lastTriggered" -> apiRequest.put("last_triggered", value);
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
            queryParams.put("contact_id", arguments.get("contactId").toString());
        }
        
        return queryParams;
    }

    private Map<String, Object> formatReminderResponse(Map<String, Object> apiResponse) {
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> reminderData = (Map<String, Object>) apiResponse.get("data");
            return Map.of(
                "data", mapFromApiFormat(reminderData)
            );
        }
        
        return Map.of("data", mapFromApiFormat(apiResponse));
    }

    private Map<String, Object> formatReminderListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> reminders = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedReminders = reminders.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedReminders);
        
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
                case "next_expected_date" -> result.put("nextExpectedDate", value);
                case "last_triggered" -> result.put("lastTriggered", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}