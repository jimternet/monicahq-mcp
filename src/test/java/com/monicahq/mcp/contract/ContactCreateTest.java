package com.monicahq.mcp.contract;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.reactive.server.WebTestClient;

import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Contract test for contact_create MCP operation.
 * Tests the MCP WebSocket endpoint for creating a new contact in MonicaHQ.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureWebTestClient
@TestPropertySource(properties = {
    "spring.profiles.active=test"
})
public class ContactCreateTest {

    @Autowired
    private WebTestClient webTestClient;

    @Test
    void shouldCreateContactViaMcpProtocol() throws Exception {
        // No mock setup needed - TestMonicaHqClient handles stubbing automatically
            
        // Given: MCP request to create a contact
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_create",
                "arguments", Map.of(
                    "firstName", "John",
                    "lastName", "Doe",
                    "genderId", 1,
                    "isBirthdateKnown", false,
                    "isDeceased", false,
                    "isDeceasedDateKnown", false,
                    "email", "john.doe@example.com",
                    "phone", "+1-555-0123"
                )
            ),
            "id", 1
        );

        // When: Send MCP request via WebSocket endpoint
        // Note: This will fail initially as MCP handler is not implemented
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(mcpRequest)
            .exchange()
            // Then: Expect successful response
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.jsonrpc").isEqualTo("2.0")
            .jsonPath("$.id").isEqualTo(1)
            .jsonPath("$.result.content[0].type").isEqualTo("text")
            .jsonPath("$.result.data.firstName").isEqualTo("John")
            .jsonPath("$.result.data.lastName").isEqualTo("Doe")
            .jsonPath("$.result.data.email").isEqualTo("john.doe@example.com");
    }

    @Test
    void shouldValidateRequiredFieldsForContactCreate() throws Exception {
        // Given: MCP request missing required fields
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_create",
                "arguments", Map.of(
                    "lastName", "Doe"  // Missing required firstName and other fields
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
            .jsonPath("$.error.code").isEqualTo(-32602)
            .jsonPath("$.error.message").value(message -> 
                assertThat(message.toString()).contains("Invalid params"));
    }
}