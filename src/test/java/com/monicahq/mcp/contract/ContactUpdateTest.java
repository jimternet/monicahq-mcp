package com.monicahq.mcp.contract;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.reactive.server.WebTestClient;

import java.util.Map;

/**
 * Contract test for contact_update MCP operation.
 * Tests the MCP WebSocket endpoint for updating an existing contact in MonicaHQ.
 * 
 * This test MUST FAIL initially (RED phase of TDD).
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureWebTestClient
@TestPropertySource(properties = {
    "spring.profiles.active=test"
})
public class ContactUpdateTest {

    @Autowired
    private WebTestClient webTestClient;

    

    @Test
    void shouldUpdateContactViaMcpProtocol() throws Exception {
        // Given: TestMonicaHqClient will return stubbed updated contact data
            
        // Given: MCP request to update a contact
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_update",
                "arguments", Map.of(
                    "id", 12345,
                    "firstName", "Jane",
                    "lastName", "Smith",
                    "email", "jane.smith@example.com",
                    "phone", "+1-555-9876"
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
            .jsonPath("$.result.data.id").isEqualTo(12345)
            .jsonPath("$.result.data.firstName").isEqualTo("Jane")
            .jsonPath("$.result.data.lastName").isEqualTo("Smith");
    }

    @Test
    void shouldUpdatePartialContactFields() throws Exception {
        // Given: TestMonicaHqClient will return stubbed partially updated contact data
            
        // Given: MCP request to update only email
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_update",
                "arguments", Map.of(
                    "id", 12345,
                    "email", "newemail@example.com"
                )
            ),
            "id", 2
        );

        // When & Then: Send MCP request and verify partial update
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(mcpRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.data.email").isEqualTo("newemail@example.com");
    }

    @Test
    void shouldValidateUpdateRequiresId() {
        // Given: MCP request without id
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_update",
                "arguments", Map.of(
                    "firstName", "Jane"  // Missing required id
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
            .jsonPath("$.error.code").isEqualTo(-32602)
            .jsonPath("$.error.message").value(msg -> 
                msg.toString().contains("id"));
    }
}