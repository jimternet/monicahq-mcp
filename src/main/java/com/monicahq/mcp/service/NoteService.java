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
public class NoteService {

    private final MonicaHqClient monicaClient;

    public Mono<Map<String, Object>> createNote(Map<String, Object> arguments) {
        log.info("Creating note with arguments: {}", arguments);
        
        try {
            validateNoteCreateArguments(arguments);
            Map<String, Object> apiRequest = mapToApiFormat(arguments);
            
            return monicaClient.post("/notes", apiRequest)
                .map(this::formatNoteResponse)
                .doOnSuccess(result -> log.info("Note created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create note: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for note creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getNote(Map<String, Object> arguments) {
        log.info("Getting note with arguments: {}", arguments);
        
        try {
            Long noteId = extractNoteId(arguments);
            
            return monicaClient.get("/notes/" + noteId, null)
                .map(this::formatNoteResponse)
                .doOnSuccess(result -> log.info("Note retrieved successfully: {}", noteId))
                .doOnError(error -> log.error("Failed to get note {}: {}", noteId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for note retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateNote(Map<String, Object> arguments) {
        log.info("Updating note with arguments: {}", arguments);
        
        try {
            Long noteId = extractNoteId(arguments);
            
            Map<String, Object> updateData = new HashMap<>(arguments);
            updateData.remove("id");
            
            Map<String, Object> apiRequest = mapToApiFormat(updateData);
            
            return monicaClient.put("/notes/" + noteId, apiRequest)
                .map(this::formatNoteResponse)
                .doOnSuccess(result -> log.info("Note updated successfully: {}", noteId))
                .doOnError(error -> log.error("Failed to update note {}: {}", noteId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for note update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteNote(Map<String, Object> arguments) {
        log.info("Deleting note with arguments: {}", arguments);
        
        try {
            Long noteId = extractNoteId(arguments);
            
            return monicaClient.delete("/notes/" + noteId)
                .map(response -> {
                    Map<String, Object> result = new HashMap<>();
                    List<Map<String, Object>> content = List.of(
                        Map.of(
                            "type", "text",
                            "text", "Note with ID " + noteId + " has been deleted successfully"
                        )
                    );
                    result.put("content", content);
                    return result;
                })
                .doOnSuccess(result -> log.info("Note deleted successfully: {}", noteId))
                .doOnError(error -> log.error("Failed to delete note {}: {}", noteId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for note deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listNotes(Map<String, Object> arguments) {
        log.info("Listing notes with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/notes", queryParams)
                .map(this::formatNoteListResponse)
                .doOnSuccess(result -> log.info("Notes listed successfully"))
                .doOnError(error -> log.error("Failed to list notes: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error building query parameters: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateNoteCreateArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("Note creation arguments cannot be empty");
        }
        
        if (!arguments.containsKey("contactId") || arguments.get("contactId") == null) {
            throw new IllegalArgumentException("contactId is required");
        }
        
        if (!arguments.containsKey("body") || arguments.get("body") == null) {
            throw new IllegalArgumentException("body is required");
        }
        
        // Allow empty body string - some notes might be empty
    }

    private Long extractNoteId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("id")) {
            throw new IllegalArgumentException("Note ID is required");
        }
        
        Object idValue = arguments.get("id");
        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }
        
        try {
            return Long.parseLong(idValue.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid note ID format: " + idValue);
        }
    }

    private Map<String, Object> mapToApiFormat(Map<String, Object> arguments) {
        Map<String, Object> apiRequest = new HashMap<>();
        
        arguments.forEach((key, value) -> {
            switch (key) {
                case "contactId" -> apiRequest.put("contact_id", value);
                case "isFavorited" -> apiRequest.put("is_favorited", value);
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
        
        if (arguments.containsKey("favorited") && arguments.get("favorited") != null) {
            queryParams.put("is_favorited", arguments.get("favorited").toString());
        }
        
        return queryParams;
    }

    private Map<String, Object> formatNoteResponse(Map<String, Object> apiResponse) {
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> noteData = (Map<String, Object>) apiResponse.get("data");
            return Map.of(
                "data", mapFromApiFormat(noteData)
            );
        }
        
        return Map.of("data", mapFromApiFormat(apiResponse));
    }

    private Map<String, Object> formatNoteListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> notes = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedNotes = notes.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedNotes);
        
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
                case "is_favorited" -> result.put("isFavorited", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}