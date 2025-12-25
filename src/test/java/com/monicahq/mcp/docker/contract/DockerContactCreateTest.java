package com.monicahq.mcp.docker.contract;

import com.monicahq.mcp.docker.DockerBaseTest;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Docker integration test for contact_create MCP operation.
 * Tests contact creation against the real Monica API.
 * Extends DockerBaseTest for health checks and automatic cleanup.
 */
public class DockerContactCreateTest extends DockerBaseTest {

    @Test
    void shouldCreateContactViaMcpProtocol() throws Exception {
        // Given: Unique test data to avoid conflicts
        String timestamp = String.valueOf(Instant.now().toEpochMilli());
        String firstName = "DockerTest" + timestamp.substring(timestamp.length() - 6);
        String lastName = "Contact";

        // When: Send MCP tools/call request to create contact
        Map<String, Object> response = callTool("contact_create", Map.of(
            "firstName", firstName,
            "lastName", lastName,
            "genderId", 1L,
            "isBirthdateKnown", false,
            "isDeceased", false,
            "isDeceasedDateKnown", false
        ), 1);

        // Then: Verify response structure
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(1L, response.get("id"));
        assertTrue(isSuccessResponse(response), "Expected success response but got: " + response);

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertNotNull(result);
        assertTrue(result.containsKey("content"), "Response should contain content");

        // Extract and track contact ID for cleanup
        Long contactId = extractContactId(response);
        assertNotNull(contactId, "Should be able to extract contact ID from response");
        createdContactIds.add(contactId);
    }

    @Test
    void shouldCreateContactWithMinimalFields() throws Exception {
        // Given: Only required fields
        String timestamp = String.valueOf(Instant.now().toEpochMilli());
        String firstName = "Minimal" + timestamp.substring(timestamp.length() - 6);

        // When: Create contact with minimal required fields
        Map<String, Object> response = callTool("contact_create", Map.of(
            "firstName", firstName,
            "genderId", 1L
        ), 2);

        // Then: Verify success
        assertNotNull(response);
        assertTrue(isSuccessResponse(response), "Expected success response but got: " + response);

        // Track for cleanup
        Long contactId = extractContactId(response);
        assertNotNull(contactId, "Should be able to extract contact ID from response");
        createdContactIds.add(contactId);
    }

    @Test
    void shouldValidateRequiredFieldsForContactCreate() throws Exception {
        // Given: MCP request missing required fields
        Map<String, Object> response = callTool("contact_create", Map.of(
            "lastName", "Doe"  // Missing required firstName
        ), 3);

        // Then: Expect validation error response
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(3L, response.get("id"));
        assertTrue(isErrorResponse(response), "Expected error response for missing required fields");

        Integer errorCode = getErrorCode(response);
        assertNotNull(errorCode, "Error response should contain error code");
        assertEquals(-32602, errorCode, "Should return invalid params error code");

        String errorMessage = getErrorMessage(response);
        assertNotNull(errorMessage, "Error response should contain error message");
        assertTrue(errorMessage.contains("Invalid params"), "Error message should indicate invalid params");
    }

    @Test
    void shouldCreateContactWithFullDetails() throws Exception {
        // Given: Contact with all optional fields
        String timestamp = String.valueOf(Instant.now().toEpochMilli());
        String firstName = "FullTest" + timestamp.substring(timestamp.length() - 6);
        String lastName = "Complete";
        String nickname = "Testy";

        // When: Create contact with full details
        Map<String, Object> response = callTool("contact_create", Map.of(
            "firstName", firstName,
            "lastName", lastName,
            "nickname", nickname,
            "genderId", 1L,
            "isBirthdateKnown", false,
            "isDeceased", false,
            "isDeceasedDateKnown", false
        ), 4);

        // Then: Verify success
        assertNotNull(response);
        assertTrue(isSuccessResponse(response), "Expected success response but got: " + response);

        // Track for cleanup
        Long contactId = extractContactId(response);
        assertNotNull(contactId, "Should be able to extract contact ID from response");
        createdContactIds.add(contactId);
    }
}
