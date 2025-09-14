package com.monicahq.mcp.contract;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.reactive.server.WebTestClient;

import java.util.Map;

/**
 * Contract test for contact_get MCP operation.
 * Tests the MCP WebSocket endpoint for retrieving a contact from MonicaHQ.
 * 
 * This test MUST FAIL initially (RED phase of TDD).
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureWebTestClient
@TestPropertySource(properties = {
    "spring.profiles.active=test"
})
public class ContactGetTest {

    @Autowired
    private WebTestClient webTestClient;

    

    @Test
    void shouldGetContactByIdViaMcpProtocol() throws Exception {
        // Given: TestMonicaHqClient will return stubbed contact data
            
        // Given: MCP request to get a contact
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_get",
                "arguments", Map.of(
                    "id", 12345
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
            .jsonPath("$.result.data.firstName").isEqualTo("John")
            .jsonPath("$.result.data.lastName").isEqualTo("Doe")
            .jsonPath("$.result.data.email").isEqualTo("john.doe@example.com");
    }

    @Test
    void shouldHandleContactNotFound() throws Exception {
        // Given: TestMonicaHqClient will return stubbed 404 for non-existent contact
            
        // Given: MCP request for non-existent contact
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_get",
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
            .expectStatus().isOk()  // MCP returns 200 with error in body
            .expectBody()
            .jsonPath("$.error.code").isEqualTo(-32000)
            .jsonPath("$.error.message").value(msg -> 
                msg.toString().contains("not found"));
    }

    @Test
    void shouldValidateIdParameter() {
        // Given: MCP request without id
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_get",
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