package com.monicahq.mcp.contract;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.reactive.server.WebTestClient;

import java.util.Map;

/**
 * Contract test for activity_list MCP operation.
 * Tests the MCP WebSocket endpoint for listing activities from MonicaHQ.
 * 
 * This test MUST FAIL initially (RED phase of TDD).
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureWebTestClient
@TestPropertySource(properties = {
    "spring.profiles.active=test"
})
public class ActivityListTest {

    @Autowired
    private WebTestClient webTestClient;

    @Test
    void shouldListActivitiesViaMcpProtocol() {
        // Given: TestMonicaHqClient will return stubbed activity list
        // Given: MCP request to list activities
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "activity_list",
                "arguments", Map.of(
                    "page", 1,
                    "limit", 10
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
            .jsonPath("$.result.data").isArray()
            .jsonPath("$.result.meta.page").isEqualTo(1)
            .jsonPath("$.result.meta.limit").isEqualTo(10);
    }

    @Test
    void shouldListActivitiesForContact() {
        // Given: TestMonicaHqClient will return stubbed filtered activity list
        // Given: MCP request to list activities for a specific contact
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "activity_list",
                "arguments", Map.of(
                    "contactId", 12345,
                    "page", 1,
                    "limit", 10
                )
            ),
            "id", 2
        );

        // When & Then: Send MCP request and verify filtered response
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(mcpRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.data").isArray();
    }

    @Test
    void shouldUseDefaultPagination() {
        // Given: TestMonicaHqClient will return stubbed default paginated response
        // Given: MCP request without pagination
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "activity_list",
                "arguments", Map.of()
            ),
            "id", 3
        );

        // When & Then: Should use defaults
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(mcpRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.meta.page").isEqualTo(1)
            .jsonPath("$.result.meta.limit").isEqualTo(10);
    }
}