package com.monicahq.mcp.contract;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.reactive.server.WebTestClient;

import java.util.List;
import java.util.Map;

/**
 * Contract test for activity_create MCP operation.
 * Tests the MCP WebSocket endpoint for creating activities in MonicaHQ.
 * 
 * This test MUST FAIL initially (RED phase of TDD).
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureWebTestClient
@TestPropertySource(properties = {
    "spring.profiles.active=test"
})
public class ActivityCreateTest {

    @Autowired
    private WebTestClient webTestClient;

    @Test
    void shouldCreateActivityViaMcpProtocol() throws Exception {
        // Given: TestMonicaHqClient will return stubbed successful activity creation response
            
        // Given: MCP request to create an activity
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "activity_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "activityTypeId", 1,
                    "summary", "Had coffee with John",
                    "description", "Discussed upcoming project collaboration",
                    "happenedAt", "2025-09-13T10:30:00Z",
                    "attendees", List.of("John Doe")
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
            .jsonPath("$.result.data.summary").isEqualTo("Had coffee with John");
    }

    @Test
    void shouldValidateRequiredFieldsForActivity() {
        // Given: MCP request missing required fields
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "activity_create",
                "arguments", Map.of(
                    "summary", "Meeting"  // Missing contactId and activityTypeId
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
}