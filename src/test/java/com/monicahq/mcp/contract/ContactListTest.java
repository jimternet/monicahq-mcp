package com.monicahq.mcp.contract;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.reactive.server.WebTestClient;

import java.util.Map;

/**
 * Contract test for contact_list MCP operation.
 * Tests the MCP WebSocket endpoint for listing contacts from MonicaHQ.
 * 
 * This test MUST FAIL initially (RED phase of TDD).
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureWebTestClient
@TestPropertySource(properties = {
    "spring.profiles.active=test"
})
public class ContactListTest {

    @Autowired
    private WebTestClient webTestClient;
    

    @Test
    void shouldListContactsViaMcpProtocol() throws Exception {
        // Given: TestMonicaHqClient will return stubbed paginated contact list
            
        // Given: MCP request to list contacts
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
    void shouldListContactsWithSearch() throws Exception {
        // Given: TestMonicaHqClient will return stubbed search results
            
        // Given: MCP request with search parameter
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_list",
                "arguments", Map.of(
                    "page", 1,
                    "limit", 10,
                    "search", "John"
                )
            ),
            "id", 2
        );

        // When & Then: Send MCP request and verify filtered results
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(mcpRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.data").isArray();
    }

    @Test
    void shouldListContactsByTag() throws Exception {
        // Given: TestMonicaHqClient will return stubbed tag-filtered contacts
            
        // Given: MCP request with tag filter
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_list",
                "arguments", Map.of(
                    "page", 1,
                    "limit", 10,
                    "tagId", 5
                )
            ),
            "id", 3
        );

        // When & Then: Send MCP request and verify filtered by tag
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(mcpRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.data").isArray();
    }

    @Test
    void shouldUseDefaultPaginationValues() throws Exception {
        // Given: TestMonicaHqClient will return stubbed default paginated response
            
        // Given: MCP request without pagination params
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_list",
                "arguments", Map.of()  // No pagination params
            ),
            "id", 4
        );

        // When & Then: Should use defaults (page=1, limit=10)
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(mcpRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.meta.page").isEqualTo(1)
            .jsonPath("$.result.meta.limit").isEqualTo(10);
    }

    @Test
    void shouldValidatePaginationLimits() throws Exception {
        // Given: TestMonicaHqClient will return stubbed capped limit response
            
        // Given: MCP request with invalid limit (>100)
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_list",
                "arguments", Map.of(
                    "page", 1,
                    "limit", 150  // Exceeds max limit of 100
                )
            ),
            "id", 5
        );

        // When & Then: Should cap at 100
        webTestClient.post()
            .uri("/mcp")
            .bodyValue(mcpRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.meta.limit").isEqualTo(100);
    }
}