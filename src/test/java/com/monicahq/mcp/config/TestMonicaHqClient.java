package com.monicahq.mcp.config;

import com.monicahq.mcp.client.MonicaHqClient;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Stubbed MonicaHQ client for testing that returns predictable responses
 * without requiring MockWebServer or actual network calls.
 */
public class TestMonicaHqClient extends MonicaHqClient {

    private final Set<String> tagAssignments = new HashSet<>();
    private final Set<Integer> validContactIds = Set.of(123, 124, 12345); // Valid contact IDs for testing
    private static volatile boolean simulateApiError = false;
    private static volatile boolean simulateMonicaAuthFailure = false;

    public TestMonicaHqClient(org.springframework.web.reactive.function.client.WebClient webClient) {
        super(webClient);
        // Pre-populate with test data for duplicate detection tests
        tagAssignments.add("12345:99999"); // contactId:tagId used in duplicate test
    }
    
    public static void setSimulateApiError(boolean simulate) {
        simulateApiError = simulate;
    }
    
    public static void setSimulateMonicaAuthFailure(boolean simulate) {
        simulateMonicaAuthFailure = simulate;
    }
    

    @Override
    public Mono<Map<String, Object>> get(String endpoint, Map<String, String> queryParams) {
        try {
            return Mono.just(createStubResponse(endpoint, "GET", null, queryParams));
        } catch (RuntimeException e) {
            return Mono.error(e);
        }
    }

    @Override
    public Mono<Map<String, Object>> post(String endpoint, Map<String, Object> requestBody) {
        return Mono.just(createStubResponse(endpoint, "POST", requestBody, null));
    }

    @Override
    public Mono<Map<String, Object>> put(String endpoint, Map<String, Object> requestBody) {
        try {
            return Mono.just(createStubResponse(endpoint, "PUT", requestBody, null));
        } catch (RuntimeException e) {
            return Mono.error(e);
        }
    }

    @Override
    public Mono<Map<String, Object>> delete(String endpoint) {
        try {
            return Mono.just(createStubResponse(endpoint, "DELETE", null, null));
        } catch (RuntimeException e) {
            return Mono.error(e);
        }
    }

    private Map<String, Object> createStubResponse(String endpoint, String method, 
                                                   Map<String, Object> requestBody, Map<String, String> queryParams) {
        // Simulate API error if requested
        if (simulateApiError) {
            throw new RuntimeException("Internal server error");
        }
        
        // Check for Monica authentication failure simulation
        if (simulateMonicaAuthFailure) {
            throw new RuntimeException("Authentication failed - invalid or expired token");
        }
        
        Map<String, Object> data = new HashMap<>();
        
        // Extract ID from endpoint for GET requests (e.g., "/contacts/123")
        Long idFromEndpoint = extractIdFromEndpoint(endpoint);
        
        // Handle "not found" scenarios for specific test IDs
        if (idFromEndpoint != null && idFromEndpoint == 99999) {
            throw new RuntimeException("Resource not found");
        }
        
        // Create appropriate stub responses based on endpoint
        if (endpoint.contains("/contactfields")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 606);
            data.put("contact_id", getValueFromRequest(requestBody, "contactId", "contact_id", 12345));
            data.put("contact_field_type_id", getValueFromRequest(requestBody, "contactFieldTypeId", "contact_field_type_id", 1));
            data.put("data", getValueFromRequest(requestBody, "data", "data", "Field data"));
            data.put("created_at", "2025-09-13T04:00:00Z");
        } else if (endpoint.contains("/setTags")) {
            // Extract contact ID and tags from the endpoint and request
            String contactId = extractContactIdFromEndpoint(endpoint);
            Object tagsObj = getValueFromRequest(requestBody, "tags", "tags", null);
            
            if (contactId != null && tagsObj instanceof List<?> tags && !tags.isEmpty()) {
                String tagId = tags.get(0).toString();
                String assignmentKey = contactId + ":" + tagId;
                
                // Check for duplicate assignment
                if (tagAssignments.contains(assignmentKey)) {
                    throw new RuntimeException("Tag is already assigned to this contact");
                } else {
                    // Add the assignment to track it
                    tagAssignments.add(assignmentKey);
                    data.put("status", "success");
                    data.put("message", "Tag operation completed successfully");
                }
            } else {
                data.put("status", "success");
                data.put("message", "Tag operation completed successfully");
            }
        } else if (endpoint.contains("/contacts") && !endpoint.contains("/contactfields") && !endpoint.contains("/setTags")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 123);
            data.put("first_name", getValueFromRequest(requestBody, "firstName", "first_name", "John"));
            data.put("last_name", getValueFromRequest(requestBody, "lastName", "last_name", "Doe"));
            data.put("email", getValueFromRequest(requestBody, "email", "email", "john.doe@example.com"));
            data.put("gender_id", getValueFromRequest(requestBody, "genderId", "gender_id", 1));
            data.put("is_deceased", getValueFromRequest(requestBody, "isDeceased", "is_deceased", false));
            data.put("is_birthdate_known", getValueFromRequest(requestBody, "isBirthdateKnown", "is_birthdate_known", false));
            data.put("is_deceased_date_known", getValueFromRequest(requestBody, "isDeceasedDateKnown", "is_deceased_date_known", false));
            data.put("phone", getValueFromRequest(requestBody, "phone", "phone", "+1-555-0123"));
        } else if (endpoint.contains("/activities")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 456);
            data.put("contact_id", getValueFromRequest(requestBody, "contactId", "contact_id", 12345));
            data.put("activity_type_id", getValueFromRequest(requestBody, "activityTypeId", "activity_type_id", 1));
            data.put("summary", getValueFromRequest(requestBody, "summary", "summary", "Had coffee with John"));
            data.put("description", getValueFromRequest(requestBody, "description", "description", "Discussed upcoming project collaboration"));
            data.put("happened_at", getValueFromRequest(requestBody, "happenedAt", "happened_at", "2025-09-13T10:30:00Z"));
            data.put("created_at", "2025-09-13T10:35:00Z");
        } else if (endpoint.contains("/calls")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 789);
            data.put("contact_id", getValueFromRequest(requestBody, "contactId", "contact_id", 12345));
            data.put("called_at", getValueFromRequest(requestBody, "calledAt", "called_at", "2025-09-13T14:30:00Z"));
            data.put("duration", getValueFromRequest(requestBody, "duration", "duration", 25));
            data.put("description", getValueFromRequest(requestBody, "description", "description", "Discussed project timeline"));
            data.put("created_at", "2025-09-13T14:45:00Z");
        } else if (endpoint.contains("/notes")) {
            Integer contactId = (Integer) getValueFromRequest(requestBody, "contactId", "contact_id", 12345);
            
            // Validate that the contact exists
            if (!validContactIds.contains(contactId)) {
                throw new RuntimeException("Contact with ID " + contactId + " not found");
            }
            
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 101);
            data.put("contact_id", contactId);
            data.put("body", getValueFromRequest(requestBody, "body", "body", "Important note about the contact"));
            data.put("is_favorited", getValueFromRequest(requestBody, "isFavorited", "is_favorited", false));
            data.put("created_at", "2025-09-13T09:00:00Z");
        } else if (endpoint.contains("/tasks")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 202);
            data.put("contact_id", getValueFromRequest(requestBody, "contactId", "contact_id", 12345));
            data.put("title", getValueFromRequest(requestBody, "title", "title", "New task title"));
            data.put("description", getValueFromRequest(requestBody, "description", "description", "Task description"));
            data.put("completed", getValueFromRequest(requestBody, "completed", "completed", false));
            data.put("due_date", getValueFromRequest(requestBody, "dueDate", "due_date", "2025-09-20"));
            data.put("created_at", "2025-09-13T08:00:00Z");
        } else if (endpoint.contains("/tags")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 303);
            data.put("name", getValueFromRequest(requestBody, "name", "name", "Important"));
            data.put("name_slug", getValueFromRequest(requestBody, "nameSlug", "name_slug", "important"));
            data.put("created_at", "2025-09-13T07:00:00Z");
        } else if (endpoint.contains("/reminders")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 404);
            data.put("contact_id", getValueFromRequest(requestBody, "contactId", "contact_id", 12345));
            data.put("title", getValueFromRequest(requestBody, "title", "title", "Birthday reminder"));
            data.put("description", getValueFromRequest(requestBody, "description", "description", "Don't forget John's birthday"));
            data.put("next_expected_date", getValueFromRequest(requestBody, "nextExpectedDate", "next_expected_date", "2025-12-25"));
            data.put("created_at", "2025-09-13T06:00:00Z");
        } else if (endpoint.contains("/journal-entries")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 505);
            data.put("contact_id", getValueFromRequest(requestBody, "contactId", "contact_id", 12345));
            data.put("title", getValueFromRequest(requestBody, "title", "title", "Journal Entry Title"));
            data.put("post", getValueFromRequest(requestBody, "post", "post", "Journal entry content"));
            data.put("created_at", "2025-09-13T05:00:00Z");
        } else if (endpoint.contains("/conversations")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 707);
            data.put("contact_id", getValueFromRequest(requestBody, "contactId", "contact_id", 12345));
            data.put("happened_at", getValueFromRequest(requestBody, "happenedAt", "happened_at", "2025-09-13T15:00:00Z"));
            data.put("created_at", "2025-09-13T15:05:00Z");
        } else if (endpoint.contains("/messages")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 808);
            data.put("conversation_id", getValueFromRequest(requestBody, "conversationId", "conversation_id", 707));
            data.put("contact_id", getValueFromRequest(requestBody, "contactId", "contact_id", 12345));
            data.put("content", getValueFromRequest(requestBody, "content", "content", "Message content"));
            data.put("written_at", getValueFromRequest(requestBody, "writtenAt", "written_at", "2025-09-13T15:10:00Z"));
            data.put("created_at", "2025-09-13T15:15:00Z");
        } else {
            // Generic response for other endpoints
            data.put("id", 999);
            data.put("status", "success");
            data.put("created_at", "2025-09-13T12:00:00Z");
        }
        
        // Handle DELETE operations - return simple success response
        if (method.equals("DELETE")) {
            return Map.of("status", "success");
        }
        
        // Handle list endpoints (GET with no ID in path OR nested list endpoints)
        boolean isListEndpoint = method.equals("GET") && (
            endpoint.equals("/contacts") || endpoint.equals("/activities") 
            || endpoint.equals("/calls") || endpoint.equals("/notes") || endpoint.equals("/tasks")
            || endpoint.equals("/tags") || endpoint.equals("/reminders") || endpoint.equals("/entries")
            || endpoint.equals("/contactfields") || endpoint.equals("/conversations") || endpoint.equals("/messages")
            || (endpoint.contains("/contactfields") && endpoint.endsWith("/contactfields"))
            || (endpoint.contains("/messages") && endpoint.endsWith("/messages"))
        );
        
        if (isListEndpoint) {
            return createListResponse(endpoint, queryParams);
        }
        
        return Map.of("data", data);
    }
    
    private Map<String, Object> createListResponse(String endpoint, Map<String, String> queryParams) {
        List<Map<String, Object>> items = new ArrayList<>();
        
        // Create sample list items based on endpoint
        if (endpoint.equals("/contacts")) {
            items.add(Map.of("id", 123L, "first_name", "John", "last_name", "Doe", "email", "john.doe@example.com"));
            items.add(Map.of("id", 124L, "first_name", "Jane", "last_name", "Smith", "email", "jane.smith@example.com"));
        } else if (endpoint.equals("/activities")) {
            items.add(Map.of("id", 456L, "contact_id", 123L, "summary", "Had coffee", "happened_at", "2025-09-13T10:30:00Z"));
        } else if (endpoint.equals("/calls")) {
            items.add(Map.of("id", 789L, "contact_id", 123L, "duration", 25, "called_at", "2025-09-13T14:30:00Z"));
        } else if (endpoint.equals("/notes")) {
            items.add(Map.of("id", 101L, "contact_id", 123L, "body", "Important note", "is_favorited", false));
        } else if (endpoint.equals("/tasks")) {
            items.add(Map.of("id", 202L, "contact_id", 123L, "title", "Task title", "completed", false));
        } else if (endpoint.equals("/tags")) {
            items.add(Map.of("id", 303L, "name", "Important", "created_at", "2025-09-13T07:00:00Z"));
        } else if (endpoint.equals("/reminders")) {
            items.add(Map.of("id", 404L, "contact_id", 123L, "title", "Birthday reminder", "next_expected_date", "2025-12-25"));
        } else if (endpoint.equals("/contactfields") || endpoint.contains("/contactfields")) {
            items.add(Map.of("id", 606L, "contact_id", 123L, "contact_field_type_id", 1L, "data", "Field data"));
        } else if (endpoint.equals("/conversations")) {
            items.add(Map.of("id", 707L, "contact_id", 123L, "happened_at", "2025-09-13T15:00:00Z"));
        } else if (endpoint.equals("/messages") || endpoint.contains("/messages")) {
            items.add(Map.of("id", 808L, "conversation_id", 707L, "content", "Message content"));
        } else if (endpoint.equals("/entries")) {
            items.add(Map.of("id", 909L, "title", "My Journal Entry", "post", "Today was a great day!", "date", "2025-09-13"));
        }
        
        // Extract pagination params
        int page = queryParams != null && queryParams.containsKey("page") ? 
            Integer.parseInt(queryParams.get("page")) : 1;
        int limit = queryParams != null && queryParams.containsKey("limit") ? 
            Math.min(100, Math.max(1, Integer.parseInt(queryParams.get("limit")))) : 10;
        
        Map<String, Object> meta = Map.of(
            "page", page,
            "limit", limit,
            "total", items.size(),
            "total_pages", 1
        );
        
        return Map.of("data", items, "meta", meta);
    }
    
    private Long extractIdFromEndpoint(String endpoint) {
        // Extract ID from endpoints like "/contacts/123", "/activities/456", etc.
        String[] parts = endpoint.split("/");
        for (int i = parts.length - 1; i >= 0; i--) {
            try {
                return Long.parseLong(parts[i]);
            } catch (NumberFormatException e) {
                // Continue looking for a numeric ID
            }
        }
        return null; // No ID found
    }
    
    private String extractContactIdFromEndpoint(String endpoint) {
        // Extract contact ID from endpoints like "/contacts/12345/setTags"
        String[] parts = endpoint.split("/");
        for (int i = 0; i < parts.length - 1; i++) {
            if ("contacts".equals(parts[i]) && i + 1 < parts.length) {
                return parts[i + 1];
            }
        }
        return null;
    }
    
    private Object getValueFromRequest(Map<String, Object> requestBody, String camelCaseKey, String snakeCaseKey, Object defaultValue) {
        if (requestBody == null) {
            return defaultValue;
        }
        
        // Check camelCase key first, then snake_case key
        if (requestBody.containsKey(camelCaseKey)) {
            return requestBody.get(camelCaseKey);
        }
        if (requestBody.containsKey(snakeCaseKey)) {
            return requestBody.get(snakeCaseKey);
        }
        
        return defaultValue;
    }
}