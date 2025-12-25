package com.monicahq.mcp.docker.contract;

import com.monicahq.mcp.docker.DockerBaseTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Docker integration test for contact_list MCP operation.
 * Tests contact listing against the real Monica API.
 * Extends DockerBaseTest for health checks and automatic cleanup.
 */
public class DockerContactListTest extends DockerBaseTest {

    private Long testContactId;

    @BeforeEach
    void createTestContact() throws Exception {
        // Create a contact to ensure we have data to list
        String timestamp = String.valueOf(Instant.now().toEpochMilli());
        String firstName = "ListTest" + timestamp.substring(timestamp.length() - 6);
        testContactId = createContactAndTrack(firstName, "Contact", 1L);
        assertNotNull(testContactId, "Test contact should be created successfully");
    }

    @Test
    void shouldListContactsViaMcpProtocol() throws Exception {
        // Given: At least one contact exists (created in setup)

        // When: Send MCP tools/call request to list contacts
        Map<String, Object> response = callTool("contact_list", Map.of(
            "page", 1,
            "limit", 10
        ), 1);

        // Then: Verify response structure
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(1L, response.get("id"));
        assertTrue(isSuccessResponse(response), "Expected success response but got: " + response);

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertNotNull(result, "Result should not be null");
        assertTrue(result.containsKey("data"), "Response should contain data array");
        assertTrue(result.containsKey("meta"), "Response should contain meta");

        // Verify data is a list
        Object data = result.get("data");
        assertTrue(data instanceof List, "Data should be a list");

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> contacts = (List<Map<String, Object>>) data;
        assertFalse(contacts.isEmpty(), "Contact list should not be empty");

        // Verify pagination metadata
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) result.get("meta");
        assertEquals(1, meta.get("page"), "Page should be 1");
        assertEquals(10, meta.get("limit"), "Limit should be 10");
    }

    @Test
    void shouldListContactsWithSearch() throws Exception {
        // Given: Test contact exists with known name prefix

        // When: Send MCP tools/call request with search parameter
        Map<String, Object> response = callTool("contact_list", Map.of(
            "page", 1,
            "limit", 10,
            "search", "ListTest"
        ), 2);

        // Then: Verify filtered results
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(isSuccessResponse(response), "Expected success response but got: " + response);

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertNotNull(result);
        assertTrue(result.containsKey("data"), "Response should contain data array");

        Object data = result.get("data");
        assertTrue(data instanceof List, "Data should be a list");
    }

    @Test
    void shouldUseDefaultPaginationValues() throws Exception {
        // Given: Test contact exists

        // When: Send MCP tools/call request without pagination params
        Map<String, Object> response = callTool("contact_list", Map.of(), 3);

        // Then: Should use defaults (page=1, limit=10)
        assertNotNull(response);
        assertTrue(isSuccessResponse(response), "Expected success response but got: " + response);

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertNotNull(result);
        assertTrue(result.containsKey("meta"), "Response should contain meta");

        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) result.get("meta");
        assertEquals(1, meta.get("page"), "Default page should be 1");
        assertEquals(10, meta.get("limit"), "Default limit should be 10");
    }

    @Test
    void shouldValidatePaginationLimits() throws Exception {
        // Given: Test contact exists

        // When: Send MCP tools/call request with limit exceeding maximum (>100)
        Map<String, Object> response = callTool("contact_list", Map.of(
            "page", 1,
            "limit", 150  // Exceeds max limit of 100
        ), 4);

        // Then: Should cap at 100
        assertNotNull(response);
        assertTrue(isSuccessResponse(response), "Expected success response but got: " + response);

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertNotNull(result);
        assertTrue(result.containsKey("meta"), "Response should contain meta");

        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) result.get("meta");
        assertEquals(100, meta.get("limit"), "Limit should be capped at 100");
    }

    @Test
    void shouldReturnCorrectDataArrayStructure() throws Exception {
        // Given: At least one contact exists

        // When: List contacts
        Map<String, Object> response = callTool("contact_list", Map.of(
            "page", 1,
            "limit", 10
        ), 5);

        // Then: Verify data array structure
        assertNotNull(response);
        assertTrue(isSuccessResponse(response), "Expected success response but got: " + response);

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> contacts = (List<Map<String, Object>>) result.get("data");
        assertFalse(contacts.isEmpty(), "Contact list should not be empty");

        // Verify first contact has expected fields
        Map<String, Object> firstContact = contacts.get(0);
        assertTrue(firstContact.containsKey("id"), "Contact should have id field");
        assertTrue(firstContact.containsKey("first_name"), "Contact should have first_name field");
    }

    @Test
    void shouldHandlePaginationBeyondAvailableData() throws Exception {
        // Given: Test contact exists

        // When: Request page beyond available data
        Map<String, Object> response = callTool("contact_list", Map.of(
            "page", 9999,
            "limit", 10
        ), 6);

        // Then: Should return empty data array
        assertNotNull(response);
        assertTrue(isSuccessResponse(response), "Expected success response but got: " + response);

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertNotNull(result);
        assertTrue(result.containsKey("data"), "Response should contain data array");

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> contacts = (List<Map<String, Object>>) result.get("data");
        assertTrue(contacts.isEmpty(), "Contact list should be empty for page beyond available data");
    }
}
