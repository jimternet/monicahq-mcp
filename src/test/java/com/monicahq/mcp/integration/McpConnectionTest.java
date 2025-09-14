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
    "spring.profiles.active=test"
})
public class McpConnectionTest {

    @Autowired
    private WebTestClient webTestClient;

    @Test
    void shouldHandleMcpInitializeMessage() {
        Map<String, Object> initRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "initialize",
            "params", Map.of(
                "protocolVersion", "2024-11-05",
                "capabilities", Map.of(
                    "tools", Map.of(),
                    "logging", Map.of()
                ),
                "clientInfo", Map.of(
                    "name", "test-client",
                    "version", "1.0.0"
                )
            ),
            "id", 1
        );
        
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(initRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.protocolVersion").exists()
            .jsonPath("$.result.serverInfo.name").exists()
            .jsonPath("$.result.capabilities.tools").exists();
    }

    @Test
    void shouldListAvailableTools() {
        Map<String, Object> toolsListRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/list",
            "params", Map.of(),
            "id", 1
        );
        
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(toolsListRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.tools").isArray()
            .jsonPath("$.result.tools.length()").isEqualTo(54); // 52 operations + 2 ContactTag operations
    }

    @Test
    void shouldValidateJsonRpcFormat() {
        Map<String, Object> invalidRequest = Map.of(
            "method", "tools/call", // Missing jsonrpc field
            "params", Map.of("name", "contact_list"),
            "id", 1
        );
        
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(invalidRequest)
            .exchange()
            .expectStatus().isBadRequest()
            .expectBody()
            .jsonPath("$.error.code").isEqualTo(-32600); // Invalid Request
    }

    @Test
    void shouldHandleInvalidMethod() {
        Map<String, Object> invalidMethodRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "invalid_method",
            "params", Map.of(),
            "id", 1
        );
        
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(invalidMethodRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.error.code").isEqualTo(-32601); // Method not found
    }

    @Test
    void shouldHandleInvalidToolName() {
        Map<String, Object> invalidToolRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "non_existent_tool",
                "arguments", Map.of()
            ),
            "id", 1
        );
        
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(invalidToolRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.error.code").isEqualTo(-32602); // Invalid params
    }

    @Test
    void shouldMaintainCorrelationIds() {
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/list",
            "params", Map.of(),
            "id", "correlation-test-123"
        );
        
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(mcpRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.id").isEqualTo("correlation-test-123");
    }
}
