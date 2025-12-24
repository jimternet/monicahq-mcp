package com.monicahq.mcp.docker.contract;

import com.monicahq.mcp.docker.DockerBaseTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Docker integration test for contact_get MCP operation.
 * Tests contact retrieval against the real Monica API.
 * Extends DockerBaseTest for health checks and automatic cleanup.
 */
public class DockerContactGetTest extends DockerBaseTest {

    private Long testContactId;

    @BeforeEach
    void createTestContact() throws Exception {
        // Create a contact to ensure we have data to retrieve
        String timestamp = String.valueOf(Instant.now().toEpochMilli());
        String firstName = "GetTest" + timestamp.substring(timestamp.length() - 6);
        testContactId = createContactAndTrack(firstName, "Contact", 1L);
        assertNotNull(testContactId, "Test contact should be created successfully");
    }

    @Test
    void shouldGetContactByIdViaMcpProtocol() throws Exception {
        // Given: A contact exists (created in setup)

        // When: Send MCP tools/call request to get contact
        Map<String, Object> response = callTool("contact_get", Map.of(
            "id", testContactId
        ), 1);

        // Then: Verify response structure
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(1L, response.get("id"));
        assertTrue(isSuccessResponse(response), "Expected success response but got: " + response);

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertNotNull(result, "Result should not be null");
        assertTrue(result.containsKey("content"), "Response should contain content");
    }

    @Test
    void shouldHandleContactNotFound() throws Exception {
        // Given: A non-existent contact ID
        Long nonExistentId = 99999999L;

        // When: Send MCP tools/call request for non-existent contact
        Map<String, Object> response = callTool("contact_get", Map.of(
            "id", nonExistentId
        ), 2);

        // Then: Expect error response for 404
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(2L, response.get("id"));

        // Should have error response for non-existent contact
        assertTrue(isErrorResponse(response), "Expected error response for non-existent contact but got: " + response);

        Integer errorCode = getErrorCode(response);
        assertNotNull(errorCode, "Error response should contain error code");

        String errorMessage = getErrorMessage(response);
        assertNotNull(errorMessage, "Error response should contain error message");
    }

    @Test
    void shouldValidateIdParameter() throws Exception {
        // Given: MCP request without id

        // When: Send MCP request with missing required parameter
        Map<String, Object> response = callTool("contact_get", Map.of(), 3);

        // Then: Expect validation error response
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(3L, response.get("id"));
        assertTrue(isErrorResponse(response), "Expected error response for missing required id");

        Integer errorCode = getErrorCode(response);
        assertNotNull(errorCode, "Error response should contain error code");
        assertEquals(-32602, errorCode, "Should return invalid params error code");

        String errorMessage = getErrorMessage(response);
        assertNotNull(errorMessage, "Error response should contain error message");
        assertTrue(errorMessage.contains("Invalid params"), "Error message should indicate invalid params");
    }

    @Test
    void shouldReturnContactWithExpectedFields() throws Exception {
        // Given: A contact exists (created in setup)

        // When: Get the contact
        Map<String, Object> response = callTool("contact_get", Map.of(
            "id", testContactId
        ), 4);

        // Then: Verify contact data fields
        assertNotNull(response);
        assertTrue(isSuccessResponse(response), "Expected success response but got: " + response);

        // Extract contact ID to verify it matches
        Long extractedId = extractContactId(response);
        assertNotNull(extractedId, "Should be able to extract contact ID from response");
        assertEquals(testContactId, extractedId, "Retrieved contact ID should match the requested ID");
    }
}
