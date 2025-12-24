package com.monicahq.mcp.docker;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.monicahq.mcp.controller.McpMessageHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.TestPropertySource;
import org.springframework.web.client.RestTemplate;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Abstract base class for Docker integration tests.
 *
 * Provides:
 * - Health check verification in @BeforeAll to ensure API availability
 * - Shared test utilities (McpMessageHandler, ObjectMapper)
 * - Cleanup infrastructure for test data in @AfterEach
 *
 * All Docker integration tests should extend this class to inherit
 * common setup, teardown, and utility methods.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=docker-test",
    "spring.main.web-application-type=none"
})
public abstract class DockerBaseTest {

    private static final Logger log = LoggerFactory.getLogger(DockerBaseTest.class);

    private static final String DEFAULT_HEALTH_URL = "http://localhost:8080/actuator/health";

    @Autowired
    protected McpMessageHandler messageHandler;

    @Autowired
    protected ObjectMapper objectMapper;

    @Value("${monica.api.url:https://app.monicahq.com/api}")
    protected String monicaApiUrl;

    /**
     * Track created contact IDs for cleanup in @AfterEach.
     * Subclasses should add contact IDs here after creation.
     */
    protected List<Long> createdContactIds = new ArrayList<>();

    /**
     * Track created note IDs for cleanup in @AfterEach.
     * Subclasses should add note IDs here after creation.
     */
    protected List<Long> createdNoteIds = new ArrayList<>();

    /**
     * Verify that the Monica API and MCP server are healthy before running tests.
     * This prevents tests from running against an unavailable service.
     */
    @BeforeAll
    static void verifyApiHealth() {
        String healthUrl = System.getenv("DOCKER_TEST_HEALTH_URL");
        if (healthUrl == null || healthUrl.isEmpty()) {
            healthUrl = DEFAULT_HEALTH_URL;
        }

        log.info("Verifying API health at: {}", healthUrl);

        RestTemplate restTemplate = new RestTemplate();
        try {
            ResponseEntity<Map> response = restTemplate.getForEntity(healthUrl, Map.class);

            if (response.getStatusCode() != HttpStatus.OK) {
                throw new IllegalStateException(
                    "Health check failed with status: " + response.getStatusCode() +
                    ". Ensure the MCP server is running at " + healthUrl);
            }

            Map<String, Object> body = response.getBody();
            if (body != null) {
                Object status = body.get("status");
                if (!"UP".equals(status)) {
                    throw new IllegalStateException(
                        "Health check returned unhealthy status: " + status +
                        ". Check MCP server logs for errors.");
                }
            }

            log.info("API health check passed. Status: UP");
        } catch (Exception e) {
            String errorMessage = String.format(
                "Monica API not available at %s. " +
                "Ensure the MCP server is running before executing Docker tests. " +
                "Start the server with: docker-compose up monicahq-mcp or ./gradlew bootRun. " +
                "Error: %s",
                healthUrl, e.getMessage());

            log.error(errorMessage);
            throw new IllegalStateException(errorMessage, e);
        }
    }

    /**
     * Clean up test data created during test execution.
     * Deletes contacts and notes that were tracked during the test.
     */
    @AfterEach
    void cleanupTestData() {
        // First delete notes (they depend on contacts)
        for (Long noteId : createdNoteIds) {
            deleteNote(noteId);
        }
        createdNoteIds.clear();

        // Then delete contacts
        for (Long contactId : createdContactIds) {
            deleteContact(contactId);
        }
        createdContactIds.clear();
    }

    /**
     * Helper method to send MCP requests through the message handler.
     *
     * @param method The MCP method to call (e.g., "tools/call", "tools/list")
     * @param params The parameters for the method
     * @param requestId The request ID
     * @return The response map from the handler
     */
    protected Map<String, Object> sendMcpRequest(String method, Map<String, Object> params, int requestId) {
        Map<String, Object> request = Map.of(
            "jsonrpc", "2.0",
            "method", method,
            "params", params,
            "id", requestId
        );

        JsonNode requestNode = objectMapper.valueToTree(request);
        return messageHandler.handleMessage(requestNode, null);
    }

    /**
     * Helper method to call an MCP tool with arguments.
     *
     * @param toolName The name of the tool to call
     * @param arguments The arguments for the tool
     * @param requestId The request ID
     * @return The response map from the handler
     */
    protected Map<String, Object> callTool(String toolName, Map<String, Object> arguments, int requestId) {
        return sendMcpRequest("tools/call", Map.of(
            "name", toolName,
            "arguments", arguments
        ), requestId);
    }

    /**
     * Create a contact and track it for automatic cleanup.
     *
     * @param firstName The first name of the contact
     * @param lastName The last name of the contact
     * @param genderId The gender ID
     * @return The created contact ID, or null if creation failed
     */
    protected Long createContactAndTrack(String firstName, String lastName, long genderId) {
        Map<String, Object> response = callTool("contact_create", Map.of(
            "firstName", firstName,
            "lastName", lastName,
            "genderId", genderId
        ), 1);

        Long contactId = extractContactId(response);
        if (contactId != null) {
            createdContactIds.add(contactId);
            log.debug("Created contact with ID: {}", contactId);
        }
        return contactId;
    }

    /**
     * Create a note and track it for automatic cleanup.
     *
     * @param contactId The contact ID to associate the note with
     * @param body The note body text
     * @return The created note ID, or null if creation failed
     */
    protected Long createNoteAndTrack(Long contactId, String body) {
        Map<String, Object> response = callTool("note_create", Map.of(
            "contactId", contactId,
            "body", body
        ), 2);

        Long noteId = extractNoteId(response);
        if (noteId != null) {
            createdNoteIds.add(noteId);
            log.debug("Created note with ID: {}", noteId);
        }
        return noteId;
    }

    /**
     * Delete a contact by ID. Used for cleanup.
     *
     * @param contactId The ID of the contact to delete
     */
    private void deleteContact(Long contactId) {
        try {
            Map<String, Object> response = callTool("contact_delete", Map.of(
                "contactId", contactId
            ), 999);
            log.debug("Deleted contact with ID: {}", contactId);
        } catch (Exception e) {
            log.warn("Failed to cleanup contact {}: {}", contactId, e.getMessage());
        }
    }

    /**
     * Delete a note by ID. Used for cleanup.
     *
     * @param noteId The ID of the note to delete
     */
    private void deleteNote(Long noteId) {
        try {
            Map<String, Object> response = callTool("note_delete", Map.of(
                "noteId", noteId
            ), 998);
            log.debug("Deleted note with ID: {}", noteId);
        } catch (Exception e) {
            log.warn("Failed to cleanup note {}: {}", noteId, e.getMessage());
        }
    }

    /**
     * Extract the contact ID from an MCP response.
     *
     * @param response The MCP response map
     * @return The contact ID, or null if not found
     */
    @SuppressWarnings("unchecked")
    protected Long extractContactId(Map<String, Object> response) {
        try {
            if (response.containsKey("result")) {
                Object result = response.get("result");
                if (result instanceof Map) {
                    Map<String, Object> resultMap = (Map<String, Object>) result;
                    if (resultMap.containsKey("content")) {
                        Object content = resultMap.get("content");
                        if (content instanceof List) {
                            List<Map<String, Object>> contentList = (List<Map<String, Object>>) content;
                            if (!contentList.isEmpty()) {
                                Map<String, Object> firstContent = contentList.get(0);
                                Object text = firstContent.get("text");
                                if (text instanceof String) {
                                    JsonNode json = objectMapper.readTree((String) text);
                                    if (json.has("id")) {
                                        return json.get("id").asLong();
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.warn("Failed to extract contact ID from response: {}", e.getMessage());
        }
        return null;
    }

    /**
     * Extract the note ID from an MCP response.
     *
     * @param response The MCP response map
     * @return The note ID, or null if not found
     */
    @SuppressWarnings("unchecked")
    protected Long extractNoteId(Map<String, Object> response) {
        // Note extraction follows the same pattern as contact extraction
        return extractContactId(response);
    }

    /**
     * Check if an MCP response is successful (contains "result" key).
     *
     * @param response The MCP response map
     * @return true if the response contains a result, false otherwise
     */
    protected boolean isSuccessResponse(Map<String, Object> response) {
        return response != null && response.containsKey("result");
    }

    /**
     * Check if an MCP response is an error (contains "error" key).
     *
     * @param response The MCP response map
     * @return true if the response contains an error, false otherwise
     */
    protected boolean isErrorResponse(Map<String, Object> response) {
        return response != null && response.containsKey("error");
    }

    /**
     * Extract the error code from an MCP error response.
     *
     * @param response The MCP response map
     * @return The error code, or null if not found
     */
    @SuppressWarnings("unchecked")
    protected Integer getErrorCode(Map<String, Object> response) {
        try {
            if (response.containsKey("error")) {
                Object error = response.get("error");
                if (error instanceof Map) {
                    Map<String, Object> errorMap = (Map<String, Object>) error;
                    Object code = errorMap.get("code");
                    if (code instanceof Number) {
                        return ((Number) code).intValue();
                    }
                }
            }
        } catch (Exception e) {
            log.warn("Failed to extract error code from response: {}", e.getMessage());
        }
        return null;
    }

    /**
     * Extract the error message from an MCP error response.
     *
     * @param response The MCP response map
     * @return The error message, or null if not found
     */
    @SuppressWarnings("unchecked")
    protected String getErrorMessage(Map<String, Object> response) {
        try {
            if (response.containsKey("error")) {
                Object error = response.get("error");
                if (error instanceof Map) {
                    Map<String, Object> errorMap = (Map<String, Object>) error;
                    Object message = errorMap.get("message");
                    if (message instanceof String) {
                        return (String) message;
                    }
                }
            }
        } catch (Exception e) {
            log.warn("Failed to extract error message from response: {}", e.getMessage());
        }
        return null;
    }
}
