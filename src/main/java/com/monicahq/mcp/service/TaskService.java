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
public class TaskService {

    private final MonicaHqClient monicaClient;

    public Mono<Map<String, Object>> createTask(Map<String, Object> arguments) {
        log.info("Creating task with arguments: {}", arguments);
        
        try {
            validateTaskCreateArguments(arguments);
            Map<String, Object> apiRequest = mapToApiFormat(arguments);
            
            return monicaClient.post("/tasks", apiRequest)
                .map(this::formatTaskResponse)
                .doOnSuccess(result -> log.info("Task created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create task: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for task creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getTask(Map<String, Object> arguments) {
        log.info("Getting task with arguments: {}", arguments);
        
        try {
            Long taskId = extractTaskId(arguments);
            
            return monicaClient.get("/tasks/" + taskId, null)
                .map(this::formatTaskResponse)
                .doOnSuccess(result -> log.info("Task retrieved successfully: {}", taskId))
                .doOnError(error -> log.error("Failed to get task {}: {}", taskId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for task retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateTask(Map<String, Object> arguments) {
        log.info("Updating task with arguments: {}", arguments);
        
        try {
            Long taskId = extractTaskId(arguments);
            
            Map<String, Object> updateData = new HashMap<>(arguments);
            updateData.remove("id");
            
            Map<String, Object> apiRequest = mapToApiFormat(updateData);
            
            return monicaClient.put("/tasks/" + taskId, apiRequest)
                .map(this::formatTaskResponse)
                .doOnSuccess(result -> log.info("Task updated successfully: {}", taskId))
                .doOnError(error -> log.error("Failed to update task {}: {}", taskId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for task update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteTask(Map<String, Object> arguments) {
        log.info("Deleting task with arguments: {}", arguments);
        
        try {
            Long taskId = extractTaskId(arguments);
            
            return monicaClient.delete("/tasks/" + taskId)
                .map(response -> {
                    Map<String, Object> result = new HashMap<>();
                    List<Map<String, Object>> content = List.of(
                        Map.of(
                            "type", "text",
                            "text", "Task with ID " + taskId + " has been deleted successfully"
                        )
                    );
                    result.put("content", content);
                    return result;
                })
                .doOnSuccess(result -> log.info("Task deleted successfully: {}", taskId))
                .doOnError(error -> log.error("Failed to delete task {}: {}", taskId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for task deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listTasks(Map<String, Object> arguments) {
        log.info("Listing tasks with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/tasks", queryParams)
                .map(this::formatTaskListResponse)
                .doOnSuccess(result -> log.info("Tasks listed successfully"))
                .doOnError(error -> log.error("Failed to list tasks: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error building query parameters: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateTaskCreateArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("Task creation arguments cannot be empty");
        }
        
        if (!arguments.containsKey("contactId") || arguments.get("contactId") == null) {
            throw new IllegalArgumentException("contactId is required");
        }
        
        if (!arguments.containsKey("title") || 
            arguments.get("title") == null || 
            arguments.get("title").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("title is required");
        }
        
        arguments.putIfAbsent("completed", false);
    }

    private Long extractTaskId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("id")) {
            throw new IllegalArgumentException("Task ID is required");
        }
        
        Object idValue = arguments.get("id");
        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }
        
        try {
            return Long.parseLong(idValue.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid task ID format: " + idValue);
        }
    }

    private Map<String, Object> mapToApiFormat(Map<String, Object> arguments) {
        Map<String, Object> apiRequest = new HashMap<>();
        
        arguments.forEach((key, value) -> {
            switch (key) {
                case "contactId" -> apiRequest.put("contact_id", value);
                case "completedAt" -> apiRequest.put("completed_at", value);
                case "dueDate" -> apiRequest.put("due_date", value);
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
        
        if (arguments.containsKey("completed") && arguments.get("completed") != null) {
            queryParams.put("completed", arguments.get("completed").toString());
        }
        
        return queryParams;
    }

    private Map<String, Object> formatTaskResponse(Map<String, Object> apiResponse) {
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> taskData = (Map<String, Object>) apiResponse.get("data");
            return Map.of(
                "data", mapFromApiFormat(taskData)
            );
        }
        
        return Map.of("data", mapFromApiFormat(apiResponse));
    }

    private Map<String, Object> formatTaskListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> tasks = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedTasks = tasks.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedTasks);
        
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
                case "completed_at" -> result.put("completedAt", value);
                case "due_date" -> result.put("dueDate", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}