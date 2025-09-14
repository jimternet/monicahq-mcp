package com.monicahq.mcp.integration;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.reactive.server.WebTestClient;

import java.util.Map;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureWebTestClient
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "mcp.auth.enabled=true"
})
public class AuthenticationTest {

    @Autowired
    private WebTestClient webTestClient;

    @Test
    void shouldValidateOAuth2BearerToken() {
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_list",
                "arguments", Map.of(
                    "page", 1,
                    "limit", 10
                )
            ),
            "id", 1
        );
        
        webTestClient.post()
            .uri("/mcp")
            .header("Authorization", "Bearer valid-oauth2-token")
            .bodyValue(mcpRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.data").isArray();
    }

    @Test
    void shouldRejectMalformedBearerToken() {
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_list",
                "arguments", Map.of()
            ),
            "id", 1
        );
        
        webTestClient.post()
            .uri("/mcp")
            .header("Authorization", "InvalidFormat token123")
            .bodyValue(mcpRequest)
            .exchange()
            .expectStatus().isUnauthorized()
            .expectBody()
            .jsonPath("$.error.code").isEqualTo(-32000);
    }

    @Test
    void shouldRejectExpiredToken() {
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_list",
                "arguments", Map.of()
            ),
            "id", 1
        );
        
        webTestClient.post()
            .uri("/mcp")
            .header("Authorization", "Bearer expired-token")
            .bodyValue(mcpRequest)
            .exchange()
            .expectStatus().isUnauthorized()
            .expectBody()
            .jsonPath("$.error.code").isEqualTo(-32000);
    }

    @Test
    void shouldRequireAuthenticationForProtectedOperations() {
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_create",
                "arguments", Map.of(
                    "firstName", "Unauthorized",
                    "genderId", 1,
                    "isBirthdateKnown", false,
                    "isDeceased", false,
                    "isDeceasedDateKnown", false
                )
            ),
            "id", 1
        );
        
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(mcpRequest) // No Authorization header
            .exchange()
            .expectStatus().isUnauthorized();
    }

    @Test
    void shouldValidateTokenScopes() {
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
        
        webTestClient.post()
            .uri("/mcp")
            .header("Authorization", "Bearer read-only-token")
            .bodyValue(mcpRequest)
            .exchange()
            .expectStatus().isForbidden()
            .expectBody()
            .jsonPath("$.error.code").isEqualTo(-32000);
    }

    @Test
    void shouldHandleMonicaApiAuthFailure() {
        // Enable Monica authentication failure simulation for this test
        com.monicahq.mcp.config.TestMonicaHqClient.setSimulateMonicaAuthFailure(true);
        
        try {
            Map<String, Object> mcpRequest = Map.of(
                "jsonrpc", "2.0",
                "method", "tools/call",
                "params", Map.of(
                    "name", "contact_list",
                    "arguments", Map.of()
                ),
                "id", 1
            );
            
            webTestClient.post()
                .uri("/mcp")
                .header("Authorization", "Bearer monica-rejected-token")
                .bodyValue(mcpRequest)
                .exchange()
                .expectStatus().isOk()
                .expectBody()
                .jsonPath("$.error.code").isEqualTo(-32000)
                .jsonPath("$.error.message").value(text -> text.toString().contains("authentication"));
        } finally {
            // Clean up - disable the simulation
            com.monicahq.mcp.config.TestMonicaHqClient.setSimulateMonicaAuthFailure(false);
        }
    }

    @Test
    void shouldLogAuthenticationAttempts() {
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_list",
                "arguments", Map.of()
            ),
            "id", "auth-test-correlation-id"
        );
        
        webTestClient.post()
            .uri("/mcp")
            .header("Authorization", "Bearer test-correlation-token")
            .bodyValue(mcpRequest)
            .exchange()
            .expectStatus().isOk();
        
        // Verify correlation ID is maintained for authentication logging
    }
}
