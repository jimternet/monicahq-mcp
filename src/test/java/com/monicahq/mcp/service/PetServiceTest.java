package com.monicahq.mcp.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.monicahq.mcp.client.AuthInterceptor;
import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.PetFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.MockWebServer;
import okhttp3.mockwebserver.RecordedRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.test.StepVerifier;

import java.io.IOException;
import java.time.Duration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for PetService using MockWebServer.
 *
 * <p>Tests the PetService which manages pets associated with contacts in MonicaHQ.
 * This service uses root-level endpoints (/pets) instead of nested endpoints
 * (/contacts/{id}/pets) to avoid HTTP 405 errors. These tests verify:
 * <ul>
 *   <li>Create pet with correct API mapping</li>
 *   <li>Get pet by ID</li>
 *   <li>Update pet</li>
 *   <li>Delete pet</li>
 *   <li>List pets with pagination</li>
 *   <li>Validation error handling</li>
 * </ul>
 */
@DisplayName("PetService Unit Tests")
class PetServiceTest {

    private MockWebServer mockWebServer;
    private MonicaHqClient monicaClient;
    private PetService petService;
    private ContentFormatter contentFormatter;
    private AuthInterceptor authInterceptor;
    private PetFieldMappingConfig fieldMappingConfig;

    @BeforeEach
    void setUp() throws IOException {
        mockWebServer = new MockWebServer();
        mockWebServer.start();

        // Create a mock AuthInterceptor that adds authentication and handles errors
        authInterceptor = new AuthInterceptor();
        // Set a valid token (20+ characters as required by isValidBearerToken)
        ReflectionTestUtils.setField(authInterceptor, "apiToken",
            "valid-test-token-that-is-long-enough-for-validation");

        // Create WebClient with error handling filter
        WebClient webClient = WebClient.builder()
            .baseUrl(mockWebServer.url("/").toString())
            .filter(authInterceptor.addAuthentication())
            .filter(authInterceptor.handleErrors())
            .build();

        monicaClient = new MonicaHqClient(webClient);
        // Set default timeout
        ReflectionTestUtils.setField(monicaClient, "timeout", Duration.ofSeconds(5));

        // Create real ContentFormatter with ObjectMapper
        ObjectMapper objectMapper = new ObjectMapper();
        contentFormatter = new ContentFormatter(objectMapper);

        // Create field mapping config
        fieldMappingConfig = new PetFieldMappingConfig();

        // Create PetService with real dependencies
        petService = new PetService(monicaClient, contentFormatter, fieldMappingConfig);
    }

    @AfterEach
    void tearDown() throws IOException {
        mockWebServer.shutdown();
    }

    @Nested
    @DisplayName("Create Pet")
    class CreatePet {

        @Test
        @DisplayName("Creates pet successfully with required fields")
        void createPetSuccessfully() throws InterruptedException {
            // Given
            String responseBody = """
                {
                    "data": {
                        "id": 123,
                        "contact_id": 456,
                        "pet_category_id": 1,
                        "name": "Fluffy",
                        "pet_category": {
                            "id": 1,
                            "name": "Cat"
                        },
                        "created_at": "2024-01-15T10:00:00Z",
                        "updated_at": "2024-01-15T10:00:00Z"
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(201)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("contactId", 456);
            arguments.put("petCategoryId", 1);
            arguments.put("name", "Fluffy");

            // When & Then
            StepVerifier.create(petService.createPet(arguments))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("data"));
                    assertTrue(response.containsKey("content"));

                    @SuppressWarnings("unchecked")
                    Map<String, Object> data = (Map<String, Object>) response.get("data");
                    assertEquals(123, data.get("id"));
                    assertEquals(456, data.get("contactId"));
                    assertEquals(1, data.get("petCategoryId"));
                    assertEquals("Fluffy", data.get("name"));
                })
                .verifyComplete();

            // Verify request was made correctly - uses root-level /pets endpoint
            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("POST", request.getMethod());
            assertEquals("/pets", request.getPath());

            String requestBody = request.getBody().readUtf8();
            assertTrue(requestBody.contains("\"contact_id\""));
            assertTrue(requestBody.contains("\"pet_category_id\""));
            assertTrue(requestBody.contains("\"name\""));
            assertTrue(requestBody.contains("456"));
            assertTrue(requestBody.contains("Fluffy"));
        }

        @Test
        @DisplayName("Creates pet without optional name")
        void createPetWithoutName() throws InterruptedException {
            // Given
            String responseBody = """
                {
                    "data": {
                        "id": 124,
                        "contact_id": 456,
                        "pet_category_id": 2,
                        "name": null,
                        "pet_category": {
                            "id": 2,
                            "name": "Dog"
                        }
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(201)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("contactId", 456);
            arguments.put("petCategoryId", 2);

            // When & Then
            StepVerifier.create(petService.createPet(arguments))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("data"));
                })
                .verifyComplete();

            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("POST", request.getMethod());
            assertEquals("/pets", request.getPath());
        }

        @Test
        @DisplayName("Fails when contactId is missing")
        void createPetWithoutContactIdFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("petCategoryId", 1);
            arguments.put("name", "Fluffy");

            // When & Then
            StepVerifier.create(petService.createPet(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("contactId is required"))
                .verify();
        }

        @Test
        @DisplayName("Fails when petCategoryId is missing")
        void createPetWithoutPetCategoryIdFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("contactId", 456);
            arguments.put("name", "Fluffy");

            // When & Then
            StepVerifier.create(petService.createPet(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("petCategoryId is required"))
                .verify();
        }
    }

    @Nested
    @DisplayName("Get Pet")
    class GetPet {

        @Test
        @DisplayName("Gets pet by ID successfully")
        void getPetSuccessfully() throws InterruptedException {
            // Given
            String responseBody = """
                {
                    "data": {
                        "id": 123,
                        "contact_id": 456,
                        "pet_category_id": 1,
                        "name": "Fluffy",
                        "pet_category": {
                            "id": 1,
                            "name": "Cat"
                        },
                        "created_at": "2024-01-15T10:00:00Z",
                        "updated_at": "2024-01-15T10:00:00Z"
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 123);

            // When & Then
            StepVerifier.create(petService.getPet(arguments))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("data"));
                    assertTrue(response.containsKey("content"));

                    @SuppressWarnings("unchecked")
                    Map<String, Object> data = (Map<String, Object>) response.get("data");
                    assertEquals(123, data.get("id"));
                    assertEquals("Fluffy", data.get("name"));
                })
                .verifyComplete();

            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("GET", request.getMethod());
            assertEquals("/pets/123", request.getPath());
        }

        @Test
        @DisplayName("Gets pet with numeric string ID")
        void getPetWithStringId() throws InterruptedException {
            // Given
            String responseBody = """
                {
                    "data": {
                        "id": 123,
                        "contact_id": 456,
                        "pet_category_id": 1,
                        "name": "Buddy"
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", "123"); // String ID

            // When & Then
            StepVerifier.create(petService.getPet(arguments))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("data"));
                })
                .verifyComplete();

            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("/pets/123", request.getPath());
        }

        @Test
        @DisplayName("Fails when ID is missing")
        void getPetWithoutIdFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();

            // When & Then
            StepVerifier.create(petService.getPet(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("Pet ID is required"))
                .verify();
        }

        @Test
        @DisplayName("Fails when ID is invalid")
        void getPetWithInvalidIdFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", "invalid");

            // When & Then
            StepVerifier.create(petService.getPet(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("Invalid pet ID format:"))
                .verify();
        }

        @Test
        @DisplayName("Handles 404 not found error")
        void getPetNotFound() {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(404)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("{\"error\": \"Pet not found\"}"));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 99999);

            // When & Then
            StepVerifier.create(petService.getPet(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof RuntimeException &&
                    throwable.getMessage().contains("Resource not found"))
                .verify();
        }
    }

    @Nested
    @DisplayName("Update Pet")
    class UpdatePet {

        @Test
        @DisplayName("Updates pet successfully")
        void updatePetSuccessfully() throws InterruptedException {
            // Given
            String responseBody = """
                {
                    "data": {
                        "id": 123,
                        "contact_id": 456,
                        "pet_category_id": 2,
                        "name": "Buddy Updated",
                        "pet_category": {
                            "id": 2,
                            "name": "Dog"
                        },
                        "updated_at": "2024-01-16T10:00:00Z"
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 123);
            arguments.put("petCategoryId", 2);
            arguments.put("name", "Buddy Updated");

            // When & Then
            StepVerifier.create(petService.updatePet(arguments))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("data"));
                    assertTrue(response.containsKey("content"));

                    @SuppressWarnings("unchecked")
                    Map<String, Object> data = (Map<String, Object>) response.get("data");
                    assertEquals(123, data.get("id"));
                    assertEquals("Buddy Updated", data.get("name"));
                    assertEquals(2, data.get("petCategoryId"));
                })
                .verifyComplete();

            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("PUT", request.getMethod());
            assertEquals("/pets/123", request.getPath());

            String requestBody = request.getBody().readUtf8();
            assertTrue(requestBody.contains("\"pet_category_id\""));
            assertTrue(requestBody.contains("\"name\""));
        }

        @Test
        @DisplayName("Fails when petCategoryId is missing for update")
        void updatePetWithoutPetCategoryIdFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 123);
            arguments.put("name", "New Name");

            // When & Then
            StepVerifier.create(petService.updatePet(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("petCategoryId is required"))
                .verify();
        }

        @Test
        @DisplayName("Fails when ID is missing for update")
        void updatePetWithoutIdFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("petCategoryId", 2);
            arguments.put("name", "New Name");

            // When & Then
            StepVerifier.create(petService.updatePet(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("Pet ID is required"))
                .verify();
        }
    }

    @Nested
    @DisplayName("Delete Pet")
    class DeletePet {

        @Test
        @DisplayName("Deletes pet successfully")
        void deletePetSuccessfully() throws InterruptedException {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("{\"deleted\": true}"));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 123);

            // When & Then
            StepVerifier.create(petService.deletePet(arguments))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("content"));

                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> content = (List<Map<String, Object>>) response.get("content");
                    assertEquals("text", content.get(0).get("type"));
                    assertTrue(content.get(0).get("text").toString().contains("deleted successfully"));
                })
                .verifyComplete();

            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("DELETE", request.getMethod());
            assertEquals("/pets/123", request.getPath());
        }

        @Test
        @DisplayName("Fails when ID is missing for delete")
        void deletePetWithoutIdFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();

            // When & Then
            StepVerifier.create(petService.deletePet(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("Pet ID is required"))
                .verify();
        }
    }

    @Nested
    @DisplayName("List Pets")
    class ListPets {

        @Test
        @DisplayName("Lists pets with default pagination")
        void listPetsWithDefaults() throws InterruptedException {
            // Given
            String responseBody = """
                {
                    "data": [
                        {
                            "id": 1,
                            "contact_id": 100,
                            "pet_category_id": 1,
                            "name": "Fluffy"
                        },
                        {
                            "id": 2,
                            "contact_id": 101,
                            "pet_category_id": 2,
                            "name": "Buddy"
                        }
                    ],
                    "meta": {
                        "total": 2,
                        "current_page": 1,
                        "per_page": 10
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();

            // When & Then
            StepVerifier.create(petService.listPets(arguments))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("data"));
                    assertTrue(response.containsKey("content"));
                    assertTrue(response.containsKey("meta"));

                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> data = (List<Map<String, Object>>) response.get("data");
                    assertEquals(2, data.size());
                })
                .verifyComplete();

            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("GET", request.getMethod());
            assertTrue(request.getPath().startsWith("/pets"));
            assertTrue(request.getPath().contains("limit=10"));
            assertTrue(request.getPath().contains("page=1"));
        }

        @Test
        @DisplayName("Lists pets with custom pagination")
        void listPetsWithCustomPagination() throws InterruptedException {
            // Given
            String responseBody = """
                {
                    "data": [
                        {
                            "id": 11,
                            "contact_id": 110,
                            "pet_category_id": 1,
                            "name": "Max"
                        }
                    ],
                    "meta": {
                        "total": 21,
                        "current_page": 2,
                        "per_page": 20
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("limit", 20);
            arguments.put("page", 2);

            // When & Then
            StepVerifier.create(petService.listPets(arguments))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("data"));
                    assertTrue(response.containsKey("meta"));
                })
                .verifyComplete();

            RecordedRequest request = mockWebServer.takeRequest();
            assertTrue(request.getPath().contains("limit=20"));
            assertTrue(request.getPath().contains("page=2"));
        }

        @Test
        @DisplayName("Lists empty pets")
        void listEmptyPets() throws InterruptedException {
            // Given
            String responseBody = """
                {
                    "data": [],
                    "meta": {
                        "total": 0,
                        "current_page": 1,
                        "per_page": 10
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();

            // When & Then
            StepVerifier.create(petService.listPets(arguments))
                .assertNext(response -> {
                    assertNotNull(response);
                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> data = (List<Map<String, Object>>) response.get("data");
                    assertTrue(data.isEmpty());
                })
                .verifyComplete();
        }
    }

    @Nested
    @DisplayName("API Error Handling")
    class ApiErrorHandling {

        @Test
        @DisplayName("Handles 401 authentication error")
        void handles401Error() {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(401)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("{\"error\": \"Invalid token\"}"));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 123);

            // When & Then
            StepVerifier.create(petService.getPet(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof RuntimeException &&
                    throwable.getMessage().contains("Authentication failed"))
                .verify();
        }

        @Test
        @DisplayName("Handles 422 validation error")
        void handles422Error() {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(422)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("{\"errors\": {\"pet_category_id\": [\"required\"]}}"));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("contactId", 456);
            arguments.put("petCategoryId", 999); // Invalid category

            // When & Then
            StepVerifier.create(petService.createPet(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof RuntimeException &&
                    throwable.getMessage().contains("Validation error"))
                .verify();
        }

        @Test
        @DisplayName("Handles 500 server error")
        void handles500Error() {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(500)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("{\"error\": \"Internal server error\"}"));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 123);

            // When & Then
            StepVerifier.create(petService.getPet(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof RuntimeException &&
                    throwable.getMessage().contains("MonicaHQ API server error"))
                .verify();
        }
    }

    @Nested
    @DisplayName("Request Headers")
    class RequestHeaders {

        @Test
        @DisplayName("Includes correct headers in requests")
        void includesCorrectHeaders() throws InterruptedException {
            // Given
            String responseBody = """
                {
                    "data": {
                        "id": 123,
                        "contact_id": 456,
                        "pet_category_id": 1,
                        "name": "Fluffy"
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 123);

            // When
            petService.getPet(arguments).block();

            // Then
            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("application/json", request.getHeader(HttpHeaders.ACCEPT));
            assertEquals("application/json", request.getHeader(HttpHeaders.CONTENT_TYPE));
            assertTrue(request.getHeader(HttpHeaders.AUTHORIZATION).startsWith("Bearer "));
            assertEquals("MonicaHQ-MCP-Server/1.0", request.getHeader(HttpHeaders.USER_AGENT));
        }
    }

    @Nested
    @DisplayName("Content Formatting")
    class ContentFormatting {

        @Test
        @DisplayName("Response includes formatted content field")
        void responseIncludesFormattedContent() throws InterruptedException {
            // Given
            String responseBody = """
                {
                    "data": {
                        "id": 123,
                        "contact_id": 456,
                        "pet_category_id": 1,
                        "name": "Fluffy"
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 123);

            // When & Then
            StepVerifier.create(petService.getPet(arguments))
                .assertNext(response -> {
                    assertTrue(response.containsKey("content"));

                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> content = (List<Map<String, Object>>) response.get("content");
                    assertEquals(1, content.size());
                    assertEquals("text", content.get(0).get("type"));

                    String text = (String) content.get(0).get("text");
                    assertNotNull(text);
                    // Content should be escaped JSON containing the pet data
                    assertTrue(text.contains("123") || text.contains("Fluffy"));
                })
                .verifyComplete();
        }
    }
}
