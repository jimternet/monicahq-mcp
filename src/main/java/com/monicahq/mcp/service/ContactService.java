package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.dto.Contact;
import com.monicahq.mcp.util.ContentFormatter;
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
    private final ContentFormatter contentFormatter;

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
            
            // Fetch existing contact to ensure all required fields are present
            return monicaClient.get("/contacts/" + contactId, null)
                .flatMap(existingContact -> {
                    // Extract the data portion of the response
                    @SuppressWarnings("unchecked")
                    Map<String, Object> existingData = existingContact.containsKey("data") ? 
                        (Map<String, Object>) existingContact.get("data") : existingContact;
                    
                    // Create update data by merging with existing data
                    Map<String, Object> updateData = new HashMap<>(arguments);
                    updateData.remove("id");
                    
                    // Ensure required fields from existing contact are preserved
                    // MonicaHQ requires these fields even when not changing them
                    if (!updateData.containsKey("firstName") && existingData.containsKey("first_name")) {
                        updateData.put("firstName", existingData.get("first_name"));
                    }
                    if (!updateData.containsKey("lastName") && existingData.containsKey("last_name")) {
                        updateData.put("lastName", existingData.get("last_name"));
                    }
                    if (!updateData.containsKey("genderId") && existingData.containsKey("gender_id")) {
                        updateData.put("genderId", existingData.get("gender_id"));
                    }
                    
                    // Handle birthdate logic - if birthdate is provided, automatically set isBirthdateKnown to true
                    if (updateData.containsKey("birthdate") && updateData.get("birthdate") != null 
                        && !updateData.get("birthdate").toString().trim().isEmpty()) {
                        // If birthdate is provided, ensure isBirthdateKnown is true
                        updateData.put("isBirthdateKnown", true);
                        log.info("Setting isBirthdateKnown=true because birthdate was provided");
                    }
                    
                    // Always include required boolean fields
                    if (!updateData.containsKey("isBirthdateKnown")) {
                        Object value = existingData.get("is_birthdate_known");
                        updateData.put("isBirthdateKnown", value != null ? value : false);
                    }
                    if (!updateData.containsKey("isDeceased")) {
                        Object value = existingData.get("is_deceased");
                        updateData.put("isDeceased", value != null ? value : false);
                    }
                    if (!updateData.containsKey("isDeceasedDateKnown")) {
                        Object value = existingData.get("is_deceased_date_known");
                        updateData.put("isDeceasedDateKnown", value != null ? value : false);
                    }
                    
                    // Validate that we have required fields
                    validateContactUpdateArguments(updateData);
                    
                    Map<String, Object> apiRequest = mapToApiFormat(updateData);
                    
                    log.info("Sending update request for contact {} with data: {}", contactId, apiRequest);
                    
                    return monicaClient.put("/contacts/" + contactId, apiRequest)
                        .map(this::formatContactResponse)
                        .doOnSuccess(result -> log.info("Contact updated successfully: {}", contactId))
                        .doOnError(error -> log.error("Failed to update contact {} with request {}: {}", 
                            contactId, apiRequest, error.getMessage()));
                })
                .onErrorResume(error -> {
                    log.error("Failed to fetch existing contact {} for update: {}", contactId, error.getMessage());
                    return Mono.error(new IllegalStateException(
                        "Unable to update contact - failed to fetch existing data: " + error.getMessage(), error));
                });
                
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
                    String formattedContent = contentFormatter.formatOperationResult(
                        "Delete", "Contact", contactId, true, 
                        "Contact with ID " + contactId + " has been deleted successfully"
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

    public Mono<Map<String, Object>> searchContacts(Map<String, Object> arguments) {
        log.info("Searching contacts with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildSearchQueryParams(arguments);
            
            return monicaClient.get("/contacts", queryParams)
                .map(this::formatContactListResponse)
                .doOnSuccess(result -> log.info("Contact search completed successfully"))
                .doOnError(error -> log.error("Failed to search contacts: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error building search parameters: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateContactCareer(Map<String, Object> arguments) {
        log.info("Updating contact career with arguments: {}", arguments);
        
        try {
            Long contactId = extractContactId(arguments);
            
            // Create career update data
            Map<String, Object> careerData = new HashMap<>();
            if (arguments.containsKey("jobTitle")) {
                careerData.put("job_title", arguments.get("jobTitle"));
            }
            if (arguments.containsKey("company")) {
                careerData.put("company", arguments.get("company"));
            }
            if (arguments.containsKey("startDate")) {
                careerData.put("start_date", arguments.get("startDate"));
            }
            if (arguments.containsKey("endDate")) {
                careerData.put("end_date", arguments.get("endDate"));
            }
            if (arguments.containsKey("salary")) {
                careerData.put("salary", arguments.get("salary"));
            }
            
            return monicaClient.put("/contacts/" + contactId + "/work", careerData)
                .map(this::formatContactResponse)
                .doOnSuccess(result -> log.info("Contact career updated successfully: {}", contactId))
                .doOnError(error -> log.error("Failed to update contact career {}: {}", contactId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact career update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getContactAuditLogs(Map<String, Object> arguments) {
        log.info("Getting contact audit logs with arguments: {}", arguments);
        
        try {
            Long contactId = extractContactId(arguments);
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/contacts/" + contactId + "/logs", queryParams)
                .map(this::formatAuditLogListResponse)
                .doOnSuccess(result -> log.info("Contact audit logs retrieved successfully: {}", contactId))
                .doOnError(error -> log.error("Failed to get contact audit logs {}: {}", contactId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact audit logs: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private Map<String, String> buildSearchQueryParams(Map<String, Object> arguments) {
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
        
        // Handle search query (required for search operation)
        if (arguments.containsKey("query") && arguments.get("query") != null) {
            queryParams.put("query", arguments.get("query").toString());
        } else {
            // Default to empty query for search (list all if no query)
            queryParams.put("query", "");
        }
        
        return queryParams;
    }

    private Map<String, Object> formatAuditLogListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> auditLogs = (List<Map<String, Object>>) apiResponse.get("data");
        
        // Format content as escaped JSON for Claude Desktop accessibility
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", auditLogs);
        
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
    
    private void validateContactUpdateArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("Contact update arguments cannot be empty");
        }
        
        // MonicaHQ API requires these fields for ALL updates
        if (!arguments.containsKey("firstName") || 
            arguments.get("firstName") == null || 
            arguments.get("firstName").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("firstName is required for updates - MonicaHQ requires this field even when not changing it");
        }
        
        // Ensure required boolean fields are present
        if (!arguments.containsKey("isBirthdateKnown")) {
            throw new IllegalArgumentException("isBirthdateKnown is required for updates - MonicaHQ requires this field");
        }
        
        if (!arguments.containsKey("isDeceased")) {
            throw new IllegalArgumentException("isDeceased is required for updates - MonicaHQ requires this field");
        }
        
        if (!arguments.containsKey("isDeceasedDateKnown")) {
            throw new IllegalArgumentException("isDeceasedDateKnown is required for updates - MonicaHQ requires this field");
        }
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
                case "birthdate" -> {
                    // MonicaHQ API expects day, month, year instead of YYYY-MM-DD
                    if (value != null && !value.toString().trim().isEmpty()) {
                        try {
                            String birthdateStr = value.toString();
                            String[] parts = birthdateStr.split("-");
                            if (parts.length == 3) {
                                apiRequest.put("year", parts[0]);
                                apiRequest.put("month", parts[1]);
                                apiRequest.put("day", parts[2]);
                                log.info("Converted birthdate {} to year={}, month={}, day={}", 
                                    birthdateStr, parts[0], parts[1], parts[2]);
                            } else {
                                log.warn("Invalid birthdate format: {}, expected YYYY-MM-DD", birthdateStr);
                            }
                        } catch (Exception e) {
                            log.error("Error parsing birthdate {}: {}", value, e.getMessage());
                        }
                    }
                }
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
        Map<String, Object> contactData;
        Map<String, Object> rawApiData;
        
        if (apiResponse.containsKey("data")) {
            // Single contact response
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            rawApiData = rawData; // Preserve original API data
            contactData = mapFromApiFormat(rawData);
        } else {
            rawApiData = apiResponse; // Preserve original API data
            contactData = mapFromApiFormat(apiResponse);
        }
        
        // Use raw API data as escaped JSON for complete field coverage as per Constitutional Principle VI
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        // Return both data and content fields for protocol compliance
        Map<String, Object> result = new HashMap<>();
        result.put("data", contactData);
        
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

    private Map<String, Object> formatContactListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> contacts = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedContacts = contacts.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        // Format content as escaped JSON for Claude Desktop accessibility as per Constitutional Principle VI
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedContacts);
        
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
    
    private Contact convertToContactDto(Map<String, Object> contactData) {
        return Contact.builder()
            .id(getLongValue(contactData, "id"))
            .firstName((String) contactData.get("firstName"))
            .lastName((String) contactData.get("lastName"))
            .nickname((String) contactData.get("nickname"))
            .genderId(getLongValue(contactData, "genderId"))
            .isBirthdateKnown(getBooleanValue(contactData, "isBirthdateKnown"))
            .isDeceased(getBooleanValue(contactData, "isDeceased"))
            .isDeceasedDateKnown(getBooleanValue(contactData, "isDeceasedDateKnown"))
            .email((String) contactData.get("email"))
            .phone((String) contactData.get("phone"))
            .birthdate(getLocalDateValue(contactData, "birthdate"))
            .company((String) contactData.get("company"))
            .jobTitle((String) contactData.get("jobTitle"))
            .createdAt(getLocalDateTimeValue(contactData, "createdAt"))
            .updatedAt(getLocalDateTimeValue(contactData, "updatedAt"))
            .build();
    }
    
    private Long getLongValue(Map<String, Object> data, String key) {
        Object value = data.get(key);
        if (value instanceof Number) {
            return ((Number) value).longValue();
        }
        return null;
    }
    
    private Boolean getBooleanValue(Map<String, Object> data, String key) {
        Object value = data.get(key);
        if (value instanceof Boolean) {
            return (Boolean) value;
        }
        return false;
    }
    
    private java.time.LocalDate getLocalDateValue(Map<String, Object> data, String key) {
        Object value = data.get(key);
        if (value instanceof String) {
            try {
                return java.time.LocalDate.parse((String) value);
            } catch (Exception e) {
                return null;
            }
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
