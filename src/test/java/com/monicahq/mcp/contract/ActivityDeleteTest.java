package com.monicahq.mcp.contract;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.reactive.server.WebTestClient;

import java.util.Map;

/**
 * Contract test for activity_delete MCP operation.
 * Tests the MCP WebSocket endpoint for deleting activities from MonicaHQ.
 * 
 * This test MUST FAIL initially (RED phase of TDD).
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureWebTestClient
@TestPropertySource(properties = {
    "spring.profiles.active=test"
})
public class ActivityDeleteTest {

    @Autowired
    private WebTestClient webTestClient;

    @Test
    void shouldDeleteActivityViaMcpProtocol() {
        // Given: TestMonicaHqClient will return stubbed successful deletion
        // Given: MCP request to delete an activity
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "activity_delete",
                "arguments", Map.of(
                    "id", 54321
                )
            ),
            "id", 1
        );

        // When & Then: Send MCP request and verify deletion
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(mcpRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.jsonrpc").isEqualTo("2.0")
            .jsonPath("$.id").isEqualTo(1)
            .jsonPath("$.result.content[0].text").value(text -> 
                text.toString().contains("deleted"));
    }

    @Test
    void shouldValidateDeleteRequiresId() {
        // Given: TestMonicaHqClient handles validation
        // Given: MCP request without id
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "activity_delete",
                "arguments", Map.of()
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
}