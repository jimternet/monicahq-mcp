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
public class ContactService {

    private final MonicaHqClient monicaClient;

    public Mono<Map<String, Object>> createContact(Map<String, Object> arguments) {
        log.info("Creating contact with arguments: {}", arguments);
        
        try {
            // Create mutable copy for validation and defaults
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            
            // Validate required fields and set defaults
            validateContactCreateArguments(mutableArguments);
            
            // Map arguments to API format
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            // Call MonicaHQ API
            return monicaClient.post("/contacts", apiRequest)
                .map(this::formatContactResponse)
                .doOnSuccess(result -> log.info("Contact created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create contact: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getContact(Map<String, Object> arguments) {
        log.info("Getting contact with arguments: {}", arguments);
        
        try {
            Long contactId = extractContactId(arguments);
            
            return monicaClient.get("/contacts/" + contactId, null)
                .map(this::formatContactResponse)
                .doOnSuccess(result -> log.info("Contact retrieved successfully: {}", contactId))
                .doOnError(error -> log.error("Failed to get contact {}: {}", contactId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateContact(Map<String, Object> arguments) {
        log.info("Updating contact with arguments: {}", arguments);
        
        try {
            Long contactId = extractContactId(arguments);
            
            // Remove ID from arguments for API call
            Map<String, Object> updateData = new HashMap<>(arguments);
            updateData.remove("id");
            
            Map<String, Object> apiRequest = mapToApiFormat(updateData);
            
            return monicaClient.put("/contacts/" + contactId, apiRequest)
                .map(this::formatContactResponse)
                .doOnSuccess(result -> log.info("Contact updated successfully: {}", contactId))
                .doOnError(error -> log.error("Failed to update contact {}: {}", contactId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteContact(Map<String, Object> arguments) {
        log.info("Deleting contact with arguments: {}", arguments);
        
        try {
            Long contactId = extractContactId(arguments);
            
            return monicaClient.delete("/contacts/" + contactId)
                .map(response -> {
                    Map<String, Object> result = new HashMap<>();
                    List<Map<String, Object>> content = List.of(
                        Map.of(
                            "type", "text",
                            "text", "Contact with ID " + contactId + " has been deleted successfully"
                        )
                    );
                    result.put("content", content);
                    return result;
                })
                .doOnSuccess(result -> log.info("Contact deleted successfully: {}", contactId))
                .doOnError(error -> log.error("Failed to delete contact {}: {}", contactId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listContacts(Map<String, Object> arguments) {
        log.info("Listing contacts with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/contacts", queryParams)
                .map(this::formatContactListResponse)
                .doOnSuccess(result -> log.info("Contacts listed successfully"))
                .doOnError(error -> log.error("Failed to list contacts: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error building query parameters: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateContactCreateArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("Contact creation arguments cannot be empty");
        }
        
        if (!arguments.containsKey("firstName") || 
            arguments.get("firstName") == null || 
            arguments.get("firstName").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("firstName is required - please provide a first name for the contact");
        }
        
        if (!arguments.containsKey("genderId") || arguments.get("genderId") == null) {
            throw new IllegalArgumentException("genderId is required - please provide a gender ID (1=Man, 2=Woman, 3=Other)");
        }
        
        // Set defaults for required boolean fields if not provided
        arguments.putIfAbsent("isBirthdateKnown", false);
        arguments.putIfAbsent("isDeceased", false);
        arguments.putIfAbsent("isDeceasedDateKnown", false);
    }

    private Long extractContactId(Map<String, Object> arguments) {
        if (arguments == null || !arguments.containsKey("id")) {
            throw new IllegalArgumentException("Contact ID is required");
        }
        
        Object idValue = arguments.get("id");
        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }
        
        try {
            return Long.parseLong(idValue.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid contact ID format: " + idValue);
        }
    }

    private Map<String, Object> mapToApiFormat(Map<String, Object> arguments) {
        Map<String, Object> apiRequest = new HashMap<>();
        
        // Map camelCase to snake_case for MonicaHQ API
        arguments.forEach((key, value) -> {
            switch (key) {
                case "firstName" -> apiRequest.put("first_name", value);
                case "lastName" -> apiRequest.put("last_name", value);
                case "genderId" -> apiRequest.put("gender_id", value);
                case "isBirthdateKnown" -> apiRequest.put("is_birthdate_known", value);
                case "isDeceased" -> apiRequest.put("is_deceased", value);
                case "isDeceasedDateKnown" -> apiRequest.put("is_deceased_date_known", value);
                case "jobTitle" -> apiRequest.put("job_title", value);
                default -> apiRequest.put(key, value);
            }
        });
        
        return apiRequest;
    }

    private Map<String, String> buildListQueryParams(Map<String, Object> arguments) {
        Map<String, String> queryParams = new HashMap<>();
        
        // Handle pagination
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
        
        // Handle search
        if (arguments.containsKey("search") && arguments.get("search") != null) {
            queryParams.put("query", arguments.get("search").toString());
        }
        
        // Handle tag filter
        if (arguments.containsKey("tagId") && arguments.get("tagId") != null) {
            queryParams.put("tags", arguments.get("tagId").toString());
        }
        
        return queryParams;
    }

    private Map<String, Object> formatContactResponse(Map<String, Object> apiResponse) {
        if (apiResponse.containsKey("data")) {
            // Single contact response
            @SuppressWarnings("unchecked")
            Map<String, Object> contactData = (Map<String, Object>) apiResponse.get("data");
            return Map.of(
                "data", mapFromApiFormat(contactData)
            );
        }
        
        return Map.of("data", mapFromApiFormat(apiResponse));
    }

    private Map<String, Object> formatContactListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> contacts = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedContacts = contacts.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedContacts);
        
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
        
        // Map snake_case to camelCase
        apiData.forEach((key, value) -> {
            switch (key) {
                case "first_name" -> result.put("firstName", value);
                case "last_name" -> result.put("lastName", value);
                case "gender_id" -> result.put("genderId", value);
                case "is_birthdate_known" -> result.put("isBirthdateKnown", value);
                case "is_deceased" -> result.put("isDeceased", value);
                case "is_deceased_date_known" -> result.put("isDeceasedDateKnown", value);
                case "job_title" -> result.put("jobTitle", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}
