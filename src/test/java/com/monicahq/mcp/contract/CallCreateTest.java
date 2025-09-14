package com.monicahq.mcp.contract;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.reactive.server.WebTestClient;

import java.util.Map;

/**
 * Contract test for call_create MCP operation.
 * Tests the MCP WebSocket endpoint for creating calls in MonicaHQ.
 * 
 * This test MUST FAIL initially (RED phase of TDD).
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureWebTestClient
@TestPropertySource(properties = {
    "spring.profiles.active=test"
})
public class CallCreateTest {

    @Autowired
    private WebTestClient webTestClient;

    @Test
    void shouldCreateCallViaMcpProtocol() {
        // Given: MCP request to create a call
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "call_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "calledAt", "2025-09-13T14:30:00Z",
                    "description", "Discussed project timeline",
                    "durationInMinutes", 25
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
            .jsonPath("$.result.data.durationInMinutes").isEqualTo(25);
    }

    @Test
    void shouldValidateRequiredFieldsForCall() {
        // Given: MCP request missing required fields
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "call_create",
                "arguments", Map.of(
                    "description", "Missing contact ID"
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
    void shouldValidateCallDuration() {
        // Given: MCP request with invalid duration
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "call_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "calledAt", "2025-09-13T14:30:00Z",
                    "durationInMinutes", 1500  // Exceeds 24 hours
                )
            ),
            "id", 3
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
}