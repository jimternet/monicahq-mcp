package com.monicahq.mcp.contract;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.reactive.server.WebTestClient;

import java.util.Map;

/**
 * Contract test for note_create MCP operation.
 * Tests the MCP WebSocket endpoint for creating notes in MonicaHQ.
 * 
 * This test MUST FAIL initially (RED phase of TDD).
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureWebTestClient
@TestPropertySource(properties = {
    "spring.profiles.active=test"
})
public class NoteCreateTest {

    @Autowired
    private WebTestClient webTestClient;

    @Test
    void shouldCreateNoteViaMcpProtocol() {
        // Given: MCP request to create a note
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "note_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "body", "John mentioned he's interested in the new project. Follow up next week.",
                    "isFavorited", false
                )
            ),
            "id", 1
        );

        // When & Then: Send MCP request and verify response
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(mcpRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.jsonrpc").isEqualTo("2.0")
            .jsonPath("$.id").isEqualTo(1)
            .jsonPath("$.result.data.contactId").isEqualTo(12345)
            .jsonPath("$.result.data.body").value(body -> 
                body.toString().contains("project"));
    }

    @Test
    void shouldValidateRequiredFieldsForNote() {
        // Given: MCP request missing required fields
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "note_create",
                "arguments", Map.of(
                    "body", "Missing contact ID"
                )
            ),
            "id", 2
        );

        // When & Then: Expect validation error
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(mcpRequest)
            .exchange()
            .expectStatus().isBadRequest()
            .expectBody()
            .jsonPath("$.error.code").isEqualTo(-32602);
    }

    @Test
    void shouldCreateNoteWithEmptyBody() {
        // Given: MCP request with empty body (should be allowed)
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "note_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "body", ""
                )
            ),
            "id", 3
        );

        // When & Then: Should be allowed (some notes might be empty)
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(mcpRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.data.contactId").isEqualTo(12345);
    }
}