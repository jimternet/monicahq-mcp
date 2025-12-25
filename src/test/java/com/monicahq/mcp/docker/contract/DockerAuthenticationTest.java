package com.monicahq.mcp.docker.contract;

import com.monicahq.mcp.docker.DockerBaseTest;
import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Docker integration test for authentication and authorization.
 * Tests JWT token validation and 401 error handling against the real Monica API.
 * Extends DockerBaseTest for health checks and automatic cleanup.
 */
public class DockerAuthenticationTest extends DockerBaseTest {

    @Test
    void shouldValidateJwtTokenWithSuccessfulApiCall() throws Exception {
        // Given: A valid JWT token is configured via environment (MONICA_API_TOKEN)

        // When: Send MCP tools/call request to list contacts
        Map<String, Object> response = callTool("contact_list", Map.of(
            "page", 1,
            "limit", 10
        ), 1);

        // Then: Verify successful authentication
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(1L, response.get("id"));
        assertTrue(isSuccessResponse(response),
            "Expected success response with valid JWT token but got: " + response);
    }

    @Test
    void shouldAuthenticateForProtectedToolOperations() throws Exception {
        // Given: Valid authentication configured

        // When: Call tools/list to verify authentication works for metadata operations
        Map<String, Object> response = sendMcpRequest("tools/list", Map.of(), 2);

        // Then: Verify tools list is accessible
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(isSuccessResponse(response),
            "Expected success response for tools/list but got: " + response);
    }

    @Test
    void shouldAuthenticateForContactCreateOperation() throws Exception {
        // Given: Valid authentication configured

        // When: Create a contact requiring authentication
        Long contactId = createContactAndTrack("AuthTest", "User", 1L);

        // Then: Verify contact was created successfully
        assertNotNull(contactId, "Contact creation should succeed with valid authentication");
    }

    @Test
    void shouldHandleInvalidToolGracefully() throws Exception {
        // Given: Valid authentication but requesting non-existent tool

        // When: Call a non-existent tool
        Map<String, Object> response = callTool("nonexistent_tool", Map.of(), 3);

        // Then: Should return error response (not authentication error)
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(isErrorResponse(response),
            "Expected error response for invalid tool but got: " + response);

        // Verify it's a method/tool not found error, not auth error
        Integer errorCode = getErrorCode(response);
        assertNotNull(errorCode, "Error code should be present");
        // MCP error codes: -32601 is method not found, -32602 is invalid params
        assertTrue(errorCode == -32601 || errorCode == -32602,
            "Expected method not found or invalid params error code, got: " + errorCode);
    }

    @Test
    void shouldHandleNonExistentResourceWithAuthentication() throws Exception {
        // Given: Valid authentication but requesting non-existent resource

        // When: Get a contact with very high ID that doesn't exist
        Map<String, Object> response = callTool("contact_get", Map.of(
            "id", 99999999L
        ), 4);

        // Then: Should return error or empty result (not authentication error)
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));

        // Either error (not found) or result is valid - but should not be 401
        assertTrue(response.containsKey("error") || response.containsKey("result"),
            "Response should have either error or result, got: " + response);

        if (isErrorResponse(response)) {
            String errorMessage = getErrorMessage(response);
            assertNotNull(errorMessage, "Error message should be present");
            // Verify it's not an authentication error
            assertFalse(errorMessage.toLowerCase().contains("unauthorized"),
                "Error should not be authentication related: " + errorMessage);
            assertFalse(errorMessage.toLowerCase().contains("401"),
                "Error should not be 401: " + errorMessage);
        }
    }

    @Test
    void shouldAuthenticateForTagOperations() throws Exception {
        // Given: Valid authentication configured

        // When: List tags (a protected operation)
        Map<String, Object> response = callTool("tag_list", Map.of(
            "page", 1,
            "limit", 5
        ), 5);

        // Then: Verify successful authentication for tag operations
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(isSuccessResponse(response),
            "Expected success response for tag_list but got: " + response);
    }

    @Test
    void shouldHandleInvalidMethodGracefully() throws Exception {
        // Given: Valid authentication

        // When: Send invalid MCP method
        Map<String, Object> response = sendMcpRequest("invalid_method", Map.of(), 6);

        // Then: Should return method not found error
        assertNotNull(response);
        assertTrue(isErrorResponse(response),
            "Expected error response for invalid method but got: " + response);

        Integer errorCode = getErrorCode(response);
        assertNotNull(errorCode, "Error code should be present");
        assertEquals(-32601, errorCode.intValue(),
            "Expected method not found error code (-32601)");
    }

    @Test
    void shouldMaintainAuthenticationAcrossMultipleRequests() throws Exception {
        // Given: Valid authentication configured

        // When: Make multiple sequential authenticated requests
        Map<String, Object> response1 = callTool("contact_list", Map.of(
            "page", 1,
            "limit", 5
        ), 7);

        Map<String, Object> response2 = callTool("tag_list", Map.of(
            "page", 1,
            "limit", 5
        ), 8);

        Map<String, Object> response3 = sendMcpRequest("tools/list", Map.of(), 9);

        // Then: All requests should succeed
        assertTrue(isSuccessResponse(response1),
            "First request should succeed: " + response1);
        assertTrue(isSuccessResponse(response2),
            "Second request should succeed: " + response2);
        assertTrue(isSuccessResponse(response3),
            "Third request should succeed: " + response3);
    }

    @Test
    void shouldAuthenticateForNoteOperations() throws Exception {
        // Given: Valid authentication and a test contact
        Long contactId = createContactAndTrack("NoteAuthTest", "Contact", 1L);
        assertNotNull(contactId, "Test contact should be created");

        // When: Create a note (requires authentication)
        Long noteId = createNoteAndTrack(contactId, "Test note for authentication verification");

        // Then: Note creation should succeed
        assertNotNull(noteId, "Note creation should succeed with valid authentication");
    }

    @Test
    void shouldReturnProperJsonRpcErrorStructure() throws Exception {
        // Given: Valid authentication

        // When: Send request that will result in an error
        Map<String, Object> response = sendMcpRequest("invalid/method", Map.of(), 10);

        // Then: Verify error response follows JSON-RPC 2.0 structure
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(10L, response.get("id"));
        assertTrue(isErrorResponse(response),
            "Expected error response but got: " + response);

        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertNotNull(error, "Error object should be present");
        assertTrue(error.containsKey("code"), "Error should have code");
        assertTrue(error.containsKey("message"), "Error should have message");
    }
}
