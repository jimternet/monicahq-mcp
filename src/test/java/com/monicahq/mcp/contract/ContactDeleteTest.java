package com.monicahq.mcp.contract;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.reactive.server.WebTestClient;

import java.util.Map;

/**
 * Contract test for contact_delete MCP operation.
 * Tests the MCP WebSocket endpoint for deleting a contact from MonicaHQ.
 * 
 * This test MUST FAIL initially (RED phase of TDD).
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureWebTestClient
@TestPropertySource(properties = {
    "spring.profiles.active=test"
})
public class ContactDeleteTest {

    @Autowired
    private WebTestClient webTestClient;
    

    @Test
    void shouldDeleteContactViaMcpProtocol() throws Exception {
        // Given: TestMonicaHqClient will return stubbed successful deletion
            
        // Given: MCP request to delete a contact
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_delete",
                "arguments", Map.of(
                    "id", 12345
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
    void shouldHandleDeleteNonExistentContact() throws Exception {
        // Given: TestMonicaHqClient will return stubbed 404 for non-existent contact
            
        // Given: MCP request to delete non-existent contact
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_delete",
                "arguments", Map.of(
                    "id", 99999
                )
            ),
            "id", 2
        );

        // When & Then: Expect not found error
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(mcpRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.error.code").isEqualTo(-32000)
            .jsonPath("$.error.message").value(msg -> 
                msg.toString().contains("not found"));
    }

    @Test
    void shouldValidateDeleteRequiresId() {
        // Given: MCP request without id
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_delete",
                "arguments", Map.of()  // Missing required id
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