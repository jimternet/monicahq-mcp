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
public class ContactNoteFlowTest {

    @Autowired
    private WebTestClient webTestClient;

    @Test
    void shouldCreateContactAndAddNoteWorkflow() {
        // Step 1: Create contact
        Map<String, Object> contactRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_create",
                "arguments", Map.of(
                    "firstName", "Alice",
                    "lastName", "Smith", 
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
            .header("Authorization", "Bearer valid-oauth2-token")
            .bodyValue(contactRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.data.id").exists();

        // Step 2: Add note to contact
        Map<String, Object> noteRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "note_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "body", "First meeting went well!",
                    "isFavorited", false
                )
            ),
            "id", 2
        );
        
        webTestClient.post()
            .uri("/mcp")
            .header("Authorization", "Bearer valid-oauth2-token")
            .bodyValue(noteRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.data.id").exists()
            .jsonPath("$.result.data.body").isEqualTo("First meeting went well!");

        // Step 3: List notes for contact
        Map<String, Object> listRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "note_list",
                "arguments", Map.of(
                    "contactId", 12345,
                    "page", 1,
                    "limit", 10
                )
            ),
            "id", 3
        );
        
        webTestClient.post()
            .uri("/mcp")
            .header("Authorization", "Bearer valid-oauth2-token")
            .bodyValue(listRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.data").isArray();
    }

    @Test
    void shouldHandleContactNotFoundForNote() {
        Map<String, Object> noteRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "note_create",
                "arguments", Map.of(
                    "contactId", 99999,
                    "body", "Note for non-existent contact"
                )
            ),
            "id", 1
        );
        
        webTestClient.post()
            .uri("/mcp")
            .header("Authorization", "Bearer valid-oauth2-token")
            .bodyValue(noteRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.error.code").isEqualTo(-32000);
    }
}
