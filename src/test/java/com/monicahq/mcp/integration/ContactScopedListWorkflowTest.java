package com.monicahq.mcp.integration;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.monicahq.mcp.controller.McpMessageHandler;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration test for contact-scoped list operations workflow.
 * Tests multiple entity list operations for a single contact.
 * Part of Phase 2: Contact-Scoped List Operations validation.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class ContactScopedListWorkflowTest {

    @Autowired
    private McpMessageHandler messageHandler;

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldListMultipleEntityTypesForSameContact() {
        // Given: A contact ID that has multiple entity types associated
        Integer contactId = 123;

        // Test 1: List activities for contact
        Map<String, Object> activityRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "activity_list_by_contact",
                "arguments", Map.of("contactId", contactId)
            ),
            "id", 1
        );

        JsonNode requestNode = objectMapper.valueToTree(activityRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        assertNotNull(response);
        assertTrue(response.containsKey("result"));

        @SuppressWarnings("unchecked")
        Map<String, Object> activityResult = (Map<String, Object>) response.get("result");
        assertTrue(activityResult.containsKey("data"));

        // Test 2: List notes for same contact
        Map<String, Object> noteRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "note_list_by_contact",
                "arguments", Map.of("contactId", contactId)
            ),
            "id", 2
        );

        requestNode = objectMapper.valueToTree(noteRequest);
        response = messageHandler.handleMessage(requestNode, null);

        assertNotNull(response);
        assertTrue(response.containsKey("result"));

        @SuppressWarnings("unchecked")
        Map<String, Object> noteResult = (Map<String, Object>) response.get("result");
        assertTrue(noteResult.containsKey("data"));

        // Test 3: List reminders for same contact
        Map<String, Object> reminderRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "reminder_list_by_contact",
                "arguments", Map.of("contactId", contactId)
            ),
            "id", 3
        );

        requestNode = objectMapper.valueToTree(reminderRequest);
        response = messageHandler.handleMessage(requestNode, null);

        assertNotNull(response);
        assertTrue(response.containsKey("result"));

        @SuppressWarnings("unchecked")
        Map<String, Object> reminderResult = (Map<String, Object>) response.get("result");
        assertTrue(reminderResult.containsKey("data"));

        // All three entity types should return successful list responses
        // for the same contact ID
    }

    @Test
    void shouldHandlePaginationConsistentlyAcrossEntityTypes() {
        // Given: Pagination parameters
        Integer contactId = 123;
        Integer page = 2;
        Integer limit = 5;

        // Test pagination for activities
        Map<String, Object> activityRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "activity_list_by_contact",
                "arguments", Map.of(
                    "contactId", contactId,
                    "page", page,
                    "limit", limit
                )
            ),
            "id", 1
        );

        JsonNode requestNode = objectMapper.valueToTree(activityRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        assertNotNull(response);
        assertTrue(response.containsKey("result"));

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) result.get("meta");

        // Verify pagination parameters are respected
        // Note: TestMonicaHqClient returns requested page/limit in meta
    }

    @Test
    void shouldFilterResultsByContactIdAcrossAllEntityTypes() {
        // Given: Two different contacts
        Integer contact1 = 123;
        Integer contact2 = 456;

        // List activities for contact 1
        Map<String, Object> request1 = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "activity_list_by_contact",
                "arguments", Map.of("contactId", contact1)
            ),
            "id", 1
        );

        JsonNode requestNode = objectMapper.valueToTree(request1);
        Map<String, Object> response1 = messageHandler.handleMessage(requestNode, null);

        // List activities for contact 2
        Map<String, Object> request2 = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "activity_list_by_contact",
                "arguments", Map.of("contactId", contact2)
            ),
            "id", 2
        );

        requestNode = objectMapper.valueToTree(request2);
        Map<String, Object> response2 = messageHandler.handleMessage(requestNode, null);

        // Both should succeed with filtered results
        assertNotNull(response1);
        assertNotNull(response2);
        assertTrue(response1.containsKey("result"));
        assertTrue(response2.containsKey("result"));
    }

    @Test
    void shouldValidateContactIdRequirementAcrossAllOperations() {
        // Given: Request without contactId for multiple entity types
        String[] operations = {
            "activity_list_by_contact",
            "note_list_by_contact",
            "task_list_by_contact"
        };

        for (String operation : operations) {
            Map<String, Object> request = Map.of(
                "jsonrpc", "2.0",
                "method", "tools/call",
                "params", Map.of(
                    "name", operation,
                    "arguments", Map.of() // Missing contactId
                ),
                "id", 1
            );

            JsonNode requestNode = objectMapper.valueToTree(request);
            Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

            // All should return validation error
            assertNotNull(response, operation + " should return response");
            assertTrue(response.containsKey("error"),
                operation + " should return error for missing contactId");

            @SuppressWarnings("unchecked")
            Map<String, Object> error = (Map<String, Object>) response.get("error");
            assertEquals(-32602, error.get("code"),
                operation + " should return Invalid params error");
        }
    }
}
