package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class JournalEntryService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> createJournalEntry(Map<String, Object> arguments) {
        log.info("Creating journal entry with arguments: {}", arguments);
        
        try {
            validateJournalEntryCreateArguments(arguments);
            Map<String, Object> apiRequest = mapToApiFormat(arguments);
            
            return monicaClient.post("/entries", apiRequest)
                .map(this::formatJournalEntryResponse)
                .doOnSuccess(result -> log.info("Journal entry created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create journal entry: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for journal entry creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getJournalEntry(Map<String, Object> arguments) {
        log.info("Getting journal entry with arguments: {}", arguments);
        
        try {
            Long entryId = extractJournalEntryId(arguments);
            
            return monicaClient.get("/entries/" + entryId, null)
                .map(this::formatJournalEntryResponse)
                .doOnSuccess(result -> log.info("Journal entry retrieved successfully: {}", entryId))
                .doOnError(error -> log.error("Failed to get journal entry {}: {}", entryId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for journal entry retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateJournalEntry(Map<String, Object> arguments) {
        log.info("Updating journal entry with arguments: {}", arguments);
        
        try {
            Long entryId = extractJournalEntryId(arguments);
            
            Map<String, Object> updateData = new HashMap<>(arguments);
            updateData.remove("id");
            
            Map<String, Object> apiRequest = mapToApiFormat(updateData);
            
            return monicaClient.put("/entries/" + entryId, apiRequest)
                .map(this::formatJournalEntryResponse)
                .doOnSuccess(result -> log.info("Journal entry updated successfully: {}", entryId))
                .doOnError(error -> log.error("Failed to update journal entry {}: {}", entryId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for journal entry update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteJournalEntry(Map<String, Object> arguments) {
        log.info("Deleting journal entry with arguments: {}", arguments);
        
        try {
            Long entryId = extractJournalEntryId(arguments);
            
            return monicaClient.delete("/entries/" + entryId)
                .map(response -> {
                    String formattedContent = contentFormatter.formatOperationResult(
                        "Delete", "Journal Entry", entryId, true, 
                        "Journal entry with ID " + entryId + " has been deleted successfully"
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
                .doOnSuccess(result -> log.info("Journal entry deleted successfully: {}", entryId))
                .doOnError(error -> log.error("Failed to delete journal entry {}: {}", entryId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for journal entry deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listJournalEntries(Map<String, Object> arguments) {
        log.info("Listing journal entries with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/entries", queryParams)
                .map(this::formatJournalEntryListResponse)
                .doOnSuccess(result -> log.info("Journal entries listed successfully"))
                .doOnError(error -> log.error("Failed to list journal entries: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error building query parameters: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateJournalEntryCreateArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("Journal entry creation arguments cannot be empty");
        }
        
        if (!arguments.containsKey("title") || 
            arguments.get("title") == null || 
            arguments.get("title").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("title is required");
        }
        
        if (!arguments.containsKey("date") || arguments.get("date") == null) {
            throw new IllegalArgumentException("date is required");
        }
    }

    private Long extractJournalEntryId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("id")) {
            throw new IllegalArgumentException("Journal entry ID is required");
        }
        
        Object idValue = arguments.get("id");
        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }
        
        try {
            return Long.parseLong(idValue.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid journal entry ID format: " + idValue);
        }
    }

    private Map<String, Object> mapToApiFormat(Map<String, Object> arguments) {
        Map<String, Object> apiRequest = new HashMap<>();
        
        arguments.forEach((key, value) -> {
            switch (key) {
                case "journalEntry" -> apiRequest.put("journal_entry", value);
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
        
        return queryParams;
    }

    private Map<String, Object> formatJournalEntryResponse(Map<String, Object> apiResponse) {
        Map<String, Object> entryData;
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            entryData = mapFromApiFormat(rawData);
        } else {
            entryData = mapFromApiFormat(apiResponse);
        }
        
        // Use raw API data for complete field coverage as per Constitutional Principle VI
        @SuppressWarnings("unchecked")
        Map<String, Object> rawApiData = apiResponse.containsKey("data") ? 
            (Map<String, Object>) apiResponse.get("data") : apiResponse;
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        // Return both data and content fields for protocol compliance
        Map<String, Object> result = new HashMap<>();
        result.put("data", entryData);
        
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

    private Map<String, Object> formatJournalEntryListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> entries = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedEntries = entries.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        // Format content for Claude Desktop visibility
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        // Extract meta for result structure
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) apiResponse.get("meta");
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedEntries);
        
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
                case "journal_entry" -> result.put("journalEntry", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
    
}