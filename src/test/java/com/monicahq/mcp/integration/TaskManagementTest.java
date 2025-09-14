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
public class TaskManagementTest {

    @Autowired
    private WebTestClient webTestClient;

    @Test
    void shouldManageTaskLifecycleWorkflow() {
        // Step 1: Create task
        Map<String, Object> createRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "task_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "title", "Call about project update",
                    "description", "Discuss Q4 milestones",
                    "completed", false
                )
            ),
            "id", 1
        );
        
        webTestClient.post()
            .uri("/mcp")
            .header("Authorization", "Bearer valid-oauth2-token")
            .bodyValue(createRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.data.id").exists();

        // Step 2: Update task as completed
        Map<String, Object> updateRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "task_update",
                "arguments", Map.of(
                    "id", 78901,
                    "title", "Call about project update - DONE",
                    "completed", true
                )
            ),
            "id", 2
        );
        
        webTestClient.post()
            .uri("/mcp")
            .header("Authorization", "Bearer valid-oauth2-token")
            .bodyValue(updateRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.data.completed").isEqualTo(true);

        // Step 3: List completed tasks
        Map<String, Object> listRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "task_list",
                "arguments", Map.of(
                    "completed", true,
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

        // Step 4: Delete completed task
        Map<String, Object> deleteRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "task_delete",
                "arguments", Map.of(
                    "id", 78901
                )
            ),
            "id", 4
        );
        
        webTestClient.post()
            .uri("/mcp")
            .header("Authorization", "Bearer valid-oauth2-token")
            .bodyValue(deleteRequest)
            .exchange()
            .expectStatus().isOk()
            .expectBody()
            .jsonPath("$.result.content[0].text").value(text -> text.toString().contains("deleted"));
    }

    @Test
    void shouldValidateTaskRequiredFields() {
        Map<String, Object> invalidRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "task_create",
                "arguments", Map.of(
                    "description", "Missing required fields"
                )
            ),
            "id", 1
        );
        
        webTestClient.post()
            .uri("/mcp")
            .header("Authorization", "Bearer valid-oauth2-token")
            .bodyValue(invalidRequest)
            .exchange()
            .expectStatus().isBadRequest()
            .expectBody()
            .jsonPath("$.error.code").isEqualTo(-32602);
    }
}
