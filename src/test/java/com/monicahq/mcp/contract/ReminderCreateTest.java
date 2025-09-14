package com.monicahq.mcp.contract;

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
public class ReminderCreateTest {

    @Autowired
    private WebTestClient webTestClient;

    @Test
    void shouldCreateReminderViaMcpProtocol() {
        Map<String, Object> mcpRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "reminder_create", "arguments", Map.of("contactId", 12345, "title", "Birthday reminder", "description", "Remember birthday", "nextExpectedDate", "2025-12-25")), "id", 1);
        webTestClient.post().uri("/mcp").bodyValue(mcpRequest).exchange().expectStatus().isOk().expectBody().jsonPath("$.result.data.id").exists().jsonPath("$.result.data.title").isEqualTo("Birthday reminder");
    }

    @Test
    void shouldValidateCreateRequiresContactId() {
        Map<String, Object> mcpRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "reminder_create", "arguments", Map.of("title", "No contact ID")), "id", 2);
        webTestClient.post().uri("/mcp").bodyValue(mcpRequest).exchange().expectStatus().isBadRequest().expectBody().jsonPath("$.error.code").isEqualTo(-32602);
    }

    @Test
    void shouldValidateCreateRequiresTitle() {
        Map<String, Object> mcpRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "reminder_create", "arguments", Map.of("contactId", 12345, "initialDate", "2025-12-25")), "id", 3);
        webTestClient.post().uri("/mcp").bodyValue(mcpRequest).exchange().expectStatus().isBadRequest().expectBody().jsonPath("$.error.code").isEqualTo(-32602);
    }
}
