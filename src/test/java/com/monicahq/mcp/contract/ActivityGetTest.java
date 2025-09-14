package com.monicahq.mcp.contract;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.reactive.server.WebTestClient;

import java.util.Map;

/**
 * Contract test for activity_get MCP operation.
 * Tests the MCP WebSocket endpoint for retrieving activities from MonicaHQ.
 * 
 * This test MUST FAIL initially (RED phase of TDD).
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureWebTestClient
@TestPropertySource(properties = {
    "spring.profiles.active=test"
})
public class ActivityGetTest {

    @Autowired
    private WebTestClient webTestClient;
    

    @Test
    void shouldGetActivityByIdViaMcpProtocol() {
        // Given: TestMonicaHqClient will return stubbed activity data
        // Given: MCP request to get an activity
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "activity_get",
                "arguments", Map.of(
                    "id", 54321
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
            .jsonPath("$.result.data.id").isEqualTo(54321);
    }

    @Test
    void shouldHandleActivityNotFound() {
        // Given: TestMonicaHqClient will return stubbed 404 for non-existent activity
        // Given: MCP request for non-existent activity
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "activity_get",
                "arguments", Map.of(
                    "id", 99999
                )
            ),
            "id", 2
        );

        // When & Then: Expect error response
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(mcpRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.error.code").isEqualTo(-32000);
    }
}