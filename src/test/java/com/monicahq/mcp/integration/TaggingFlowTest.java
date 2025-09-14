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
public class TaggingFlowTest {

    @Autowired
    private WebTestClient webTestClient;

    @Test
    void shouldManageContactTaggingWorkflow() {
        // Step 1: Create tag
        Map<String, Object> tagCreateRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "tag_create",
                "arguments", Map.of(
                    "name", "VIP Client",
                    "nameSlug", "vip-client"
                )
            ),
            "id", 1
        );
        
        webTestClient.post()
            .uri("/mcp")
            .header("Authorization", "Bearer valid-oauth2-token")
            .bodyValue(tagCreateRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.data.id").exists();

        // Step 2: Add tag to contact
        Map<String, Object> tagAddRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contacttag_add",
                "arguments", Map.of(
                    "contactId", 12345,
                    "tagId", 11111
                )
            ),
            "id", 2
        );
        
        webTestClient.post()
            .uri("/mcp")
            .header("Authorization", "Bearer valid-oauth2-token")
            .bodyValue(tagAddRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.content[0].text").value(text -> text.toString().contains("tag added"));

        // Step 3: List contacts by tag
        Map<String, Object> contactListRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_list",
                "arguments", Map.of(
                    "tagId", 11111,
                    "page", 1,
                    "limit", 10
                )
            ),
            "id", 3
        );
        
        webTestClient.post()
            .uri("/mcp")
            .header("Authorization", "Bearer valid-oauth2-token")
            .bodyValue(contactListRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.data").isArray();

        // Step 4: Remove tag from contact
        Map<String, Object> tagRemoveRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contacttag_remove",
                "arguments", Map.of(
                    "contactId", 12345,
                    "tagId", 11111
                )
            ),
            "id", 4
        );
        
        webTestClient.post()
            .uri("/mcp")
            .header("Authorization", "Bearer valid-oauth2-token")
            .bodyValue(tagRemoveRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.content[0].text").value(text -> text.toString().contains("tag removed"));
    }

    @Test
    void shouldHandleTagSearchAndManagement() {
        // Search for tags
        Map<String, Object> tagSearchRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "tag_list",
                "arguments", Map.of(
                    "search", "client",
                    "page", 1,
                    "limit", 10
                )
            ),
            "id", 1
        );
        
        webTestClient.post()
            .uri("/mcp")
            .header("Authorization", "Bearer valid-oauth2-token")
            .bodyValue(tagSearchRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.data").isArray();

        // Update tag
        Map<String, Object> tagUpdateRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "tag_update",
                "arguments", Map.of(
                    "id", 11111,
                    "name", "Premium VIP Client",
                    "nameSlug", "premium-vip-client"
                )
            ),
            "id", 2
        );
        
        webTestClient.post()
            .uri("/mcp")
            .header("Authorization", "Bearer valid-oauth2-token")
            .bodyValue(tagUpdateRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.data.name").isEqualTo("Premium VIP Client");
    }
}
