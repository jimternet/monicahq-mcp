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
        } else if (endpoint.contains("/work")) {
            // Handle /contacts/{id}/work endpoint for career updates
            data.put("job", getValueFromRequest(requestBody, "job", "job", "Software Developer"));
            data.put("company", getValueFromRequest(requestBody, "company", "company", "Tech Corp"));
            data.put("salary", getValueFromRequest(requestBody, "salary", "salary", null));
            data.put("created_at", "2025-09-13T12:00:00Z");
        } else if (endpoint.contains("/contacts") && !endpoint.contains("/contactfields") && !endpoint.contains("/setTags") && !endpoint.contains("/work")) {
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
        } else if (endpoint.contains("/relationships")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 901);
            data.put("contact_is", getValueFromRequest(requestBody, "contactIs", "contact_is", 123));
            data.put("of_contact", getValueFromRequest(requestBody, "ofContact", "of_contact", 124));
            data.put("relationship_type_id", getValueFromRequest(requestBody, "relationshipTypeId", "relationship_type_id", 1));
            data.put("notes", getValueFromRequest(requestBody, "notes", "notes", "Test relationship"));
            data.put("created_at", "2025-09-13T16:00:00Z");
        } else if (endpoint.contains("/relationshiptypes")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 1);
            data.put("name", "Partner");
            data.put("name_reverse", "Partner of");
            data.put("relationship_type_group_id", 1);
            data.put("created_at", "2025-09-13T16:00:00Z");
        } else if (endpoint.contains("/relationshiptypegroups")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 1);
            data.put("name", "Family");
            data.put("created_at", "2025-09-13T16:00:00Z");
        } else if (endpoint.contains("/companies")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 902);
            data.put("name", getValueFromRequest(requestBody, "name", "name", "Test Company"));
            data.put("website", getValueFromRequest(requestBody, "website", "website", "https://example.com"));
            data.put("number_of_employees", getValueFromRequest(requestBody, "numberOfEmployees", "number_of_employees", 50));
            data.put("created_at", "2025-09-13T17:00:00Z");
        } else if (endpoint.contains("/debts")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 1001);
            data.put("contact_id", getValueFromRequest(requestBody, "contactId", "contact_id", 123));
            data.put("amount", getValueFromRequest(requestBody, "amount", "amount", 500.00));
            data.put("currency", getValueFromRequest(requestBody, "currency", "currency", "USD"));
            data.put("in_debt", getValueFromRequest(requestBody, "inDebt", "in_debt", "contact"));
            data.put("status", getValueFromRequest(requestBody, "status", "status", "pending"));
            data.put("reason", getValueFromRequest(requestBody, "reason", "reason", "Test debt"));
            data.put("created_at", "2025-09-13T18:00:00Z");
        } else if (endpoint.contains("/documents")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 1002);
            data.put("contact_id", getValueFromRequest(requestBody, "contactId", "contact_id", 123));
            data.put("filename", getValueFromRequest(requestBody, "filename", "filename", "document.pdf"));
            data.put("original_filename", getValueFromRequest(requestBody, "originalFilename", "original_filename", "test_document.pdf"));
            data.put("mime_type", getValueFromRequest(requestBody, "mimeType", "mime_type", "application/pdf"));
            data.put("size", getValueFromRequest(requestBody, "size", "size", 204800));
            data.put("description", getValueFromRequest(requestBody, "description", "description", "Test document"));
            data.put("created_at", "2025-09-13T19:00:00Z");
        } else if (endpoint.contains("/photos")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 1003);
            data.put("contact_id", getValueFromRequest(requestBody, "contactId", "contact_id", 123));
            data.put("filename", getValueFromRequest(requestBody, "filename", "filename", "photo.jpg"));
            data.put("original_filename", getValueFromRequest(requestBody, "originalFilename", "original_filename", "test_photo.jpg"));
            data.put("width", getValueFromRequest(requestBody, "width", "width", 800));
            data.put("height", getValueFromRequest(requestBody, "height", "height", 600));
            data.put("filesize", getValueFromRequest(requestBody, "filesize", "filesize", 102400));
            data.put("mime_type", getValueFromRequest(requestBody, "mimeType", "mime_type", "image/jpeg"));
            data.put("created_at", "2025-09-13T20:00:00Z");
        } else if (endpoint.contains("/gifts")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 1004);
            data.put("contact_id", getValueFromRequest(requestBody, "contactId", "contact_id", 123));
            data.put("name", getValueFromRequest(requestBody, "name", "name", "Gift name"));
            data.put("comment", getValueFromRequest(requestBody, "comment", "comment", "Gift comment"));
            data.put("url", getValueFromRequest(requestBody, "url", "url", "https://example.com/gift"));
            data.put("value", getValueFromRequest(requestBody, "value", "value", 99.99));
            data.put("status", getValueFromRequest(requestBody, "status", "status", "idea"));
            data.put("date", getValueFromRequest(requestBody, "date", "date", "2025-12-25"));
            data.put("is_for", getValueFromRequest(requestBody, "isFor", "is_for", "Birthday"));
            data.put("created_at", "2025-09-13T21:00:00Z");
        } else if (endpoint.contains("/auditlogs") || endpoint.contains("/logs")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 1005);
            data.put("action", "create");
            data.put("auditable_type", "Contact");
            data.put("auditable_id", 123);
            data.put("user_id", 1);
            data.put("user_name", "Test User");
            data.put("ip_address", "192.168.1.1");
            data.put("user_agent", "Mozilla/5.0");
            data.put("old_values", Map.of());
            data.put("new_values", Map.of("name", "John Doe"));
            data.put("created_at", "2025-09-13T22:00:00Z");
        } else if (endpoint.contains("/countries")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 1006);
            data.put("name", "United States");
            data.put("country_code", "US");
            data.put("created_at", "2025-09-13T23:00:00Z");
        } else if (endpoint.contains("/currencies")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 1007);
            data.put("code", "USD");
            data.put("name", "US Dollar");
            data.put("symbol", "$");
            data.put("exchange_rate", 1.0);
            data.put("created_at", "2025-09-13T23:30:00Z");
        } else if (endpoint.contains("/users")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 1);
            data.put("first_name", getValueFromRequest(requestBody, "firstName", "first_name", "John"));
            data.put("last_name", getValueFromRequest(requestBody, "lastName", "last_name", "Doe"));
            data.put("email", getValueFromRequest(requestBody, "email", "email", "john.doe@example.com"));
            data.put("is_administrator", getValueFromRequest(requestBody, "isAdministrator", "is_administrator", false));
            data.put("created_at", "2025-09-13T10:00:00Z");
        } else if (endpoint.contains("/compliance")) {
            data.put("id", idFromEndpoint != null ? idFromEndpoint : 1);
            data.put("type", getValueFromRequest(requestBody, "type", "type", "terms_of_service"));
            data.put("description", getValueFromRequest(requestBody, "description", "description", "Terms of Service"));
            data.put("content", getValueFromRequest(requestBody, "content", "content", "Legal compliance document content"));
            data.put("version", "1.0");
            data.put("created_at", "2025-09-13T09:00:00Z");
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
            || endpoint.equals("/relationships") || endpoint.equals("/relationshiptypes") || endpoint.equals("/relationshiptypegroups")
            || endpoint.equals("/companies")
            || endpoint.equals("/debts") || endpoint.equals("/documents") || endpoint.equals("/photos")
            || endpoint.equals("/gifts") || endpoint.equals("/auditlogs") || endpoint.equals("/countries")
            || endpoint.contains("/logs")
            || endpoint.equals("/currencies") || endpoint.equals("/users") || endpoint.equals("/compliance")
            || (endpoint.contains("/contactfields") && endpoint.endsWith("/contactfields"))
            || (endpoint.contains("/messages") && endpoint.endsWith("/messages"))
            || (endpoint.contains("/tags") && endpoint.contains("/contacts"))
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
        } else if (endpoint.equals("/relationships")) {
            items.add(Map.of("id", 901L, "contact_is", 123L, "of_contact", 124L, "relationship_type_id", 1L, "notes", "Test relationship"));
        } else if (endpoint.equals("/relationshiptypes")) {
            items.add(Map.of("id", 1L, "name", "Partner", "name_reverse", "Partner of", "relationship_type_group_id", 1L));
        } else if (endpoint.equals("/relationshiptypegroups")) {
            items.add(Map.of("id", 1L, "name", "Family"));
        } else if (endpoint.equals("/companies")) {
            items.add(Map.of("id", 902L, "name", "Test Company", "website", "https://example.com", "number_of_employees", 50));
        } else if (endpoint.equals("/debts")) {
            items.add(Map.of("id", 1001L, "contact_id", 123L, "amount", 500.00, "currency", "USD", "in_debt", "contact", "status", "pending"));
        } else if (endpoint.equals("/documents")) {
            items.add(Map.of("id", 1002L, "contact_id", 123L, "filename", "document.pdf", "mime_type", "application/pdf", "size", 204800));
        } else if (endpoint.equals("/photos")) {
            items.add(Map.of("id", 1003L, "contact_id", 123L, "filename", "photo.jpg", "width", 800, "height", 600, "filesize", 102400));
        } else if (endpoint.equals("/gifts")) {
            items.add(Map.of("id", 1004L, "contact_id", 123L, "name", "Gift name", "value", 99.99, "status", "idea", "date", "2025-12-25"));
        } else if (endpoint.equals("/auditlogs") || endpoint.contains("/logs")) {
            items.add(Map.of("id", 1005L, "action", "create", "auditable_type", "Contact", "auditable_id", 123L, "user_id", 1L, "user_name", "Test User", "created_at", "2025-09-13T22:00:00Z"));
        } else if (endpoint.equals("/countries")) {
            items.add(Map.of("id", 1006L, "name", "United States", "country_code", "US"));
        } else if (endpoint.equals("/currencies")) {
            items.add(Map.of("id", 1007L, "code", "USD", "name", "US Dollar", "symbol", "$", "exchange_rate", 1.0));
        } else if (endpoint.contains("/tags") && endpoint.contains("/contacts")) {
            // For /tags/{id}/contacts endpoint - return contacts associated with a tag
            items.add(Map.of("id", 123L, "first_name", "John", "last_name", "Doe", "email", "john.doe@example.com"));
            items.add(Map.of("id", 124L, "first_name", "Jane", "last_name", "Smith", "email", "jane.smith@example.com"));
        } else if (endpoint.equals("/users")) {
            items.add(Map.of("id", 1L, "first_name", "John", "last_name", "Admin", "email", "admin@example.com", "is_administrator", true));
            items.add(Map.of("id", 2L, "first_name", "Jane", "last_name", "User", "email", "user@example.com", "is_administrator", false));
        } else if (endpoint.equals("/compliance")) {
            items.add(Map.of("id", 1L, "type", "terms_of_service", "description", "Terms of Service", "version", "1.0"));
            items.add(Map.of("id", 2L, "type", "privacy_policy", "description", "Privacy Policy", "version", "1.0"));
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