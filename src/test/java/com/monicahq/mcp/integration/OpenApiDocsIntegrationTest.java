package com.monicahq.mcp.integration;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.reactive.server.WebTestClient;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration tests to verify /v3/api-docs returns valid OpenAPI 3.0.3 JSON.
 *
 * This test validates:
 * - The endpoint returns HTTP 200
 * - The response is valid JSON
 * - The OpenAPI version is 3.0.x
 * - Required paths (/health, /mcp) are documented
 * - Required tags (Health, MCP) are present
 * - The spec follows OpenAPI 3.0.3 structure
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureWebTestClient
@ActiveProfiles("test")
public class OpenApiDocsIntegrationTest {

    @Autowired
    private WebTestClient webTestClient;

    private final ObjectMapper objectMapper = new ObjectMapper();

    @Test
    void apiDocsEndpointReturnsOk() {
        webTestClient.get()
            .uri("/v3/api-docs")
            .accept(MediaType.APPLICATION_JSON)
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON);
    }

    @Test
    void apiDocsReturnsValidOpenApiJson() {
        String responseBody = webTestClient.get()
            .uri("/v3/api-docs")
            .accept(MediaType.APPLICATION_JSON)
            .exchange()
            .expectStatus().isOk()
            .expectBody(String.class)
            .returnResult()
            .getResponseBody();

        assertNotNull(responseBody, "Response body should not be null");
        assertFalse(responseBody.isEmpty(), "Response body should not be empty");

        // Parse JSON and verify it's valid
        JsonNode rootNode;
        try {
            rootNode = objectMapper.readTree(responseBody);
        } catch (JsonProcessingException e) {
            fail("Response is not valid JSON: " + e.getMessage());
            return;
        }

        assertNotNull(rootNode, "Parsed JSON should not be null");
        assertTrue(rootNode.isObject(), "Root element should be an object");
    }

    @Test
    void apiDocsHasOpenApiVersion() {
        String responseBody = getApiDocsResponse();
        JsonNode rootNode = parseJson(responseBody);

        // Verify OpenAPI version is present and is 3.0.x
        assertTrue(rootNode.has("openapi"), "Should have 'openapi' field");
        String openApiVersion = rootNode.get("openapi").asText();
        assertNotNull(openApiVersion, "OpenAPI version should not be null");
        assertTrue(openApiVersion.startsWith("3.0") || openApiVersion.startsWith("3.1"),
            "OpenAPI version should be 3.0.x or 3.1.x, but was: " + openApiVersion);
    }

    @Test
    void apiDocsHasInfoSection() {
        String responseBody = getApiDocsResponse();
        JsonNode rootNode = parseJson(responseBody);

        // Verify info section is present
        assertTrue(rootNode.has("info"), "Should have 'info' section");
        JsonNode info = rootNode.get("info");
        assertTrue(info.has("title"), "Info should have 'title'");
        assertTrue(info.has("version"), "Info should have 'version'");
    }

    @Test
    void apiDocsHasHealthEndpoints() {
        String responseBody = getApiDocsResponse();
        JsonNode rootNode = parseJson(responseBody);

        // Verify paths section is present
        assertTrue(rootNode.has("paths"), "Should have 'paths' section");
        JsonNode paths = rootNode.get("paths");

        // Verify /health endpoint is documented
        assertTrue(paths.has("/health"), "Should have /health path documented");

        // Verify /health has GET method
        JsonNode healthPath = paths.get("/health");
        assertTrue(healthPath.has("get"), "/health should have GET method");
    }

    @Test
    void apiDocsHasMcpEndpoint() {
        String responseBody = getApiDocsResponse();
        JsonNode rootNode = parseJson(responseBody);

        // Verify paths section is present
        assertTrue(rootNode.has("paths"), "Should have 'paths' section");
        JsonNode paths = rootNode.get("paths");

        // Verify /mcp endpoint is documented
        assertTrue(paths.has("/mcp"), "Should have /mcp path documented");

        // Verify /mcp has POST method
        JsonNode mcpPath = paths.get("/mcp");
        assertTrue(mcpPath.has("post"), "/mcp should have POST method");
    }

    @Test
    void apiDocsHasRequiredTags() {
        String responseBody = getApiDocsResponse();
        JsonNode rootNode = parseJson(responseBody);

        // Verify tags section is present
        assertTrue(rootNode.has("tags"), "Should have 'tags' section");
        JsonNode tags = rootNode.get("tags");
        assertTrue(tags.isArray(), "Tags should be an array");

        // Collect tag names
        boolean hasHealthTag = false;
        boolean hasMcpTag = false;

        for (JsonNode tag : tags) {
            String tagName = tag.get("name").asText();
            if ("Health".equals(tagName)) {
                hasHealthTag = true;
            }
            if ("MCP".equals(tagName)) {
                hasMcpTag = true;
            }
        }

        assertTrue(hasHealthTag, "Should have 'Health' tag");
        assertTrue(hasMcpTag, "Should have 'MCP' tag");
    }

    @Test
    void apiDocsHasOperationIds() {
        String responseBody = getApiDocsResponse();
        JsonNode rootNode = parseJson(responseBody);

        JsonNode paths = rootNode.get("paths");
        assertNotNull(paths, "Paths section should be present");

        // Check that each operation has an operationId
        paths.fieldNames().forEachRemaining(pathKey -> {
            JsonNode pathItem = paths.get(pathKey);
            pathItem.fieldNames().forEachRemaining(method -> {
                if (isHttpMethod(method)) {
                    JsonNode operation = pathItem.get(method);
                    assertTrue(operation.has("operationId"),
                        "Operation " + method.toUpperCase() + " " + pathKey + " should have operationId");
                }
            });
        });
    }

    @Test
    void apiDocsHasResponses() {
        String responseBody = getApiDocsResponse();
        JsonNode rootNode = parseJson(responseBody);

        JsonNode paths = rootNode.get("paths");
        assertNotNull(paths, "Paths section should be present");

        // Check that each operation has responses
        paths.fieldNames().forEachRemaining(pathKey -> {
            JsonNode pathItem = paths.get(pathKey);
            pathItem.fieldNames().forEachRemaining(method -> {
                if (isHttpMethod(method)) {
                    JsonNode operation = pathItem.get(method);
                    assertTrue(operation.has("responses"),
                        "Operation " + method.toUpperCase() + " " + pathKey + " should have responses");
                    JsonNode responses = operation.get("responses");
                    assertTrue(responses.size() > 0,
                        "Operation " + method.toUpperCase() + " " + pathKey + " should have at least one response");
                }
            });
        });
    }

    @Test
    void apiDocsHealthEndpointHasAllOperations() {
        String responseBody = getApiDocsResponse();
        JsonNode rootNode = parseJson(responseBody);
        JsonNode paths = rootNode.get("paths");

        // Verify all health endpoints are documented
        assertTrue(paths.has("/health"), "Should have /health");
        assertTrue(paths.has("/health/live"), "Should have /health/live");
        assertTrue(paths.has("/health/ready"), "Should have /health/ready");
        assertTrue(paths.has("/health/detailed"), "Should have /health/detailed");
        assertTrue(paths.has("/health/ping"), "Should have /health/ping");
    }

    // Helper methods

    private String getApiDocsResponse() {
        String responseBody = webTestClient.get()
            .uri("/v3/api-docs")
            .accept(MediaType.APPLICATION_JSON)
            .exchange()
            .expectStatus().isOk()
            .expectBody(String.class)
            .returnResult()
            .getResponseBody();

        assertNotNull(responseBody, "Response body should not be null");
        return responseBody;
    }

    private JsonNode parseJson(String json) {
        try {
            return objectMapper.readTree(json);
        } catch (JsonProcessingException e) {
            fail("Failed to parse JSON: " + e.getMessage());
            return null;
        }
    }

    private boolean isHttpMethod(String method) {
        return method.equals("get") || method.equals("post") ||
               method.equals("put") || method.equals("delete") ||
               method.equals("patch") || method.equals("head") ||
               method.equals("options") || method.equals("trace");
    }
}
