package com.monicahq.mcp.service.base;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.ServiceTestBase;
import com.monicahq.mcp.util.ContentFormatter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Mono;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Comprehensive unit tests for AbstractCrudService base class.
 * Uses a test implementation (TestEntityService) to validate common CRUD patterns.
 */
@ExtendWith(MockitoExtension.class)
class AbstractCrudServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private TestEntityService testService;

    @BeforeEach
    void setUp() {
        testService = new TestEntityService(monicaClient, contentFormatter);
    }

    // ========================================================================================
    // CREATE OPERATION TESTS
    // ========================================================================================

    @Nested
    @DisplayName("create()")
    class CreateTests {

        @Test
        @DisplayName("should create entity with valid arguments")
        void validArguments_CreatesEntity() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("contactId", 10L);
            arguments.put("body", "Test entity");

            Map<String, Object> apiResponse = createApiResponse(1L, "Test entity");

            when(monicaClient.post(eq("/testentities"), any())).thenReturn(Mono.just(apiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("{}");

            // When
            Map<String, Object> result = testService.create(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"));
            assertTrue(result.containsKey("content"));
            verify(monicaClient).post(eq("/testentities"), any());
        }

        @Test
        @DisplayName("should map field names to API format")
        void fieldsAreMapped_ToApiFormat() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("contactId", 10L);
            arguments.put("body", "Test entity");
            arguments.put("isFavorited", true);

            Map<String, Object> apiResponse = createApiResponse(1L, "Test entity");

            when(monicaClient.post(eq("/testentities"), any())).thenReturn(Mono.just(apiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("{}");

            // When
            testService.create(arguments).block();

            // Then
            verify(monicaClient).post(eq("/testentities"), argThat(data ->
                data.get("contact_id").equals(10L) &&
                Boolean.TRUE.equals(data.get("is_favorited")) &&
                !data.containsKey("contactId") &&
                !data.containsKey("isFavorited")
            ));
        }

        @Test
        @DisplayName("should apply default values")
        void defaultValues_Applied() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("contactId", 10L);
            arguments.put("body", "Test entity");
            // Note: isFavorited is not provided, should get default

            Map<String, Object> apiResponse = createApiResponse(1L, "Test entity");

            when(monicaClient.post(eq("/testentities"), any())).thenReturn(Mono.just(apiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("{}");

            // When
            testService.create(arguments).block();

            // Then
            verify(monicaClient).post(eq("/testentities"), argThat(data ->
                Boolean.FALSE.equals(data.get("is_favorited"))
            ));
        }

        @Test
        @DisplayName("should throw when required field is missing")
        void missingRequiredField_ThrowsException() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("body", "Test entity");
            // contactId is missing

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
                testService.create(arguments).block()
            );
            assertEquals("contactId is required", exception.getMessage());
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("should throw when arguments are empty")
        void emptyArguments_ThrowsException() {
            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
                testService.create(new HashMap<>()).block()
            );
            assertEquals("TestEntity arguments cannot be empty", exception.getMessage());
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("should throw when arguments are null")
        void nullArguments_ThrowsException() {
            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
                testService.create(null).block()
            );
            assertEquals("TestEntity arguments cannot be empty", exception.getMessage());
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("should format response for Claude Desktop visibility")
        void response_FormattedForClaudeDesktop() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("contactId", 10L);
            arguments.put("body", "Test entity");

            Map<String, Object> apiResponse = createApiResponse(1L, "Test entity");

            when(monicaClient.post(eq("/testentities"), any())).thenReturn(Mono.just(apiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("{\"id\":1}");

            // When
            Map<String, Object> result = testService.create(arguments).block();

            // Then
            assertNotNull(result);
            @SuppressWarnings("unchecked")
            List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
            assertEquals(1, content.size());
            assertEquals("text", content.get(0).get("type"));
            assertEquals("{\"id\":1}", content.get(0).get("text"));
        }
    }

    // ========================================================================================
    // GET OPERATION TESTS
    // ========================================================================================

    @Nested
    @DisplayName("get()")
    class GetTests {

        @Test
        @DisplayName("should get entity by ID")
        void validId_ReturnsEntity() {
            // Given
            Map<String, Object> arguments = Map.of("id", 1L);
            Map<String, Object> apiResponse = createApiResponse(1L, "Test entity");

            when(monicaClient.get(eq("/testentities/1"), any())).thenReturn(Mono.just(apiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("{}");

            // When
            Map<String, Object> result = testService.get(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"));
            verify(monicaClient).get(eq("/testentities/1"), any());
        }

        @Test
        @DisplayName("should parse String ID")
        void stringId_ParsedCorrectly() {
            // Given
            Map<String, Object> arguments = Map.of("id", "42");
            Map<String, Object> apiResponse = createApiResponse(42L, "Test entity");

            when(monicaClient.get(eq("/testentities/42"), any())).thenReturn(Mono.just(apiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("{}");

            // When
            testService.get(arguments).block();

            // Then
            verify(monicaClient).get(eq("/testentities/42"), any());
        }

        @Test
        @DisplayName("should parse Integer ID")
        void integerId_ParsedCorrectly() {
            // Given
            Map<String, Object> arguments = Map.of("id", 99);
            Map<String, Object> apiResponse = createApiResponse(99L, "Test entity");

            when(monicaClient.get(eq("/testentities/99"), any())).thenReturn(Mono.just(apiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("{}");

            // When
            testService.get(arguments).block();

            // Then
            verify(monicaClient).get(eq("/testentities/99"), any());
        }

        @Test
        @DisplayName("should throw when ID is missing")
        void missingId_ThrowsException() {
            // Given
            Map<String, Object> arguments = Map.of("body", "Test");

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
                testService.get(arguments).block()
            );
            assertEquals("TestEntity ID is required", exception.getMessage());
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("should throw when ID format is invalid")
        void invalidIdFormat_ThrowsException() {
            // Given
            Map<String, Object> arguments = Map.of("id", "not-a-number");

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
                testService.get(arguments).block()
            );
            assertTrue(exception.getMessage().contains("Invalid testentity ID format"));
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("should map response fields from API format")
        void responseFields_MappedFromApiFormat() {
            // Given
            Map<String, Object> arguments = Map.of("id", 1L);
            Map<String, Object> apiData = new HashMap<>();
            apiData.put("id", 1L);
            apiData.put("body", "Test");
            apiData.put("contact_id", 10L);
            apiData.put("is_favorited", true);
            apiData.put("created_at", "2024-01-15T10:00:00Z");
            apiData.put("updated_at", "2024-01-15T11:00:00Z");
            Map<String, Object> apiResponse = createSingleEntityResponse(apiData);

            when(monicaClient.get(eq("/testentities/1"), any())).thenReturn(Mono.just(apiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("{}");

            // When
            Map<String, Object> result = testService.get(arguments).block();

            // Then
            assertNotNull(result);
            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) result.get("data");
            assertEquals(10L, data.get("contactId"));
            assertEquals(true, data.get("isFavorited"));
            assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
            assertEquals("2024-01-15T11:00:00Z", data.get("updatedAt"));
        }
    }

    // ========================================================================================
    // UPDATE OPERATION TESTS
    // ========================================================================================

    @Nested
    @DisplayName("update()")
    class UpdateTests {

        @Test
        @DisplayName("should update entity with valid arguments")
        void validArguments_UpdatesEntity() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 1L);
            arguments.put("body", "Updated content");

            Map<String, Object> apiResponse = createApiResponse(1L, "Updated content");

            when(monicaClient.put(eq("/testentities/1"), any())).thenReturn(Mono.just(apiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("{}");

            // When
            Map<String, Object> result = testService.update(arguments).block();

            // Then
            assertNotNull(result);
            verify(monicaClient).put(eq("/testentities/1"), any());
        }

        @Test
        @DisplayName("should remove ID from update payload")
        void idField_RemovedFromPayload() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 1L);
            arguments.put("body", "Updated content");

            Map<String, Object> apiResponse = createApiResponse(1L, "Updated content");

            when(monicaClient.put(eq("/testentities/1"), any())).thenReturn(Mono.just(apiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("{}");

            // When
            testService.update(arguments).block();

            // Then
            verify(monicaClient).put(eq("/testentities/1"), argThat(data ->
                !data.containsKey("id")
            ));
        }

        @Test
        @DisplayName("should map field names to API format")
        void fieldsAreMapped_ToApiFormat() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 1L);
            arguments.put("isFavorited", true);

            Map<String, Object> apiResponse = createApiResponse(1L, "Updated");

            when(monicaClient.put(eq("/testentities/1"), any())).thenReturn(Mono.just(apiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("{}");

            // When
            testService.update(arguments).block();

            // Then
            verify(monicaClient).put(eq("/testentities/1"), argThat(data ->
                Boolean.TRUE.equals(data.get("is_favorited")) &&
                !data.containsKey("isFavorited")
            ));
        }

        @Test
        @DisplayName("should throw when ID is missing")
        void missingId_ThrowsException() {
            // Given
            Map<String, Object> arguments = Map.of("body", "Updated");

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
                testService.update(arguments).block()
            );
            assertEquals("TestEntity ID is required", exception.getMessage());
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("should parse String ID")
        void stringId_ParsedCorrectly() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", "42");
            arguments.put("body", "Updated");

            Map<String, Object> apiResponse = createApiResponse(42L, "Updated");

            when(monicaClient.put(eq("/testentities/42"), any())).thenReturn(Mono.just(apiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("{}");

            // When
            testService.update(arguments).block();

            // Then
            verify(monicaClient).put(eq("/testentities/42"), any());
        }
    }

    // ========================================================================================
    // DELETE OPERATION TESTS
    // ========================================================================================

    @Nested
    @DisplayName("delete()")
    class DeleteTests {

        @Test
        @DisplayName("should delete entity by ID")
        void validId_DeletesEntity() {
            // Given
            Map<String, Object> arguments = Map.of("id", 1L);
            Map<String, Object> deleteResponse = createDeleteResponse(1L);

            when(monicaClient.delete(eq("/testentities/1"))).thenReturn(Mono.just(deleteResponse));
            when(contentFormatter.formatOperationResult(
                eq("Delete"), eq("TestEntity"), eq(1L), eq(true), anyString()
            )).thenReturn("Deleted successfully");

            // When
            Map<String, Object> result = testService.delete(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("content"));
            verify(monicaClient).delete(eq("/testentities/1"));
        }

        @Test
        @DisplayName("should throw when ID is missing")
        void missingId_ThrowsException() {
            // Given
            Map<String, Object> arguments = Map.of("body", "Test");

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
                testService.delete(arguments).block()
            );
            assertEquals("TestEntity ID is required", exception.getMessage());
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("should parse String ID")
        void stringId_ParsedCorrectly() {
            // Given
            Map<String, Object> arguments = Map.of("id", "99");
            Map<String, Object> deleteResponse = createDeleteResponse(99L);

            when(monicaClient.delete(eq("/testentities/99"))).thenReturn(Mono.just(deleteResponse));
            when(contentFormatter.formatOperationResult(
                eq("Delete"), eq("TestEntity"), eq(99L), eq(true), anyString()
            )).thenReturn("Deleted successfully");

            // When
            testService.delete(arguments).block();

            // Then
            verify(monicaClient).delete(eq("/testentities/99"));
        }

        @Test
        @DisplayName("should format delete response for Claude Desktop visibility")
        void response_FormattedForClaudeDesktop() {
            // Given
            Map<String, Object> arguments = Map.of("id", 1L);
            Map<String, Object> deleteResponse = createDeleteResponse(1L);

            when(monicaClient.delete(eq("/testentities/1"))).thenReturn(Mono.just(deleteResponse));
            when(contentFormatter.formatOperationResult(
                eq("Delete"), eq("TestEntity"), eq(1L), eq(true), anyString()
            )).thenReturn("TestEntity deleted");

            // When
            Map<String, Object> result = testService.delete(arguments).block();

            // Then
            assertNotNull(result);
            @SuppressWarnings("unchecked")
            List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
            assertEquals(1, content.size());
            assertEquals("text", content.get(0).get("type"));
            assertEquals("TestEntity deleted", content.get(0).get("text"));
        }
    }

    // ========================================================================================
    // LIST OPERATION TESTS
    // ========================================================================================

    @Nested
    @DisplayName("list()")
    class ListTests {

        @Test
        @DisplayName("should list entities with default pagination")
        void defaultPagination_Applied() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            List<Map<String, Object>> entities = List.of(
                createEntityData(1L, "Entity 1"),
                createEntityData(2L, "Entity 2")
            );
            Map<String, Object> listResponse = createListResponse(entities, 1, 10, 2);

            when(monicaClient.get(eq("/testentities"), any())).thenReturn(Mono.just(listResponse));
            when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

            // When
            Map<String, Object> result = testService.list(arguments).block();

            // Then
            assertNotNull(result);
            verify(monicaClient).get(eq("/testentities"), argThat(params ->
                "1".equals(params.get("page")) &&
                "10".equals(params.get("limit"))
            ));
        }

        @Test
        @DisplayName("should use provided pagination values")
        void customPagination_Applied() {
            // Given
            Map<String, Object> arguments = Map.of("page", 3, "limit", 25);
            List<Map<String, Object>> entities = List.of(createEntityData(1L, "Entity"));
            Map<String, Object> listResponse = createListResponse(entities, 3, 25, 75);

            when(monicaClient.get(eq("/testentities"), any())).thenReturn(Mono.just(listResponse));
            when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

            // When
            testService.list(arguments).block();

            // Then
            verify(monicaClient).get(eq("/testentities"), argThat(params ->
                "3".equals(params.get("page")) &&
                "25".equals(params.get("limit"))
            ));
        }

        @Test
        @DisplayName("should clamp limit to maximum of 100")
        void limitAboveMax_ClampedTo100() {
            // Given
            Map<String, Object> arguments = Map.of("limit", 200);
            List<Map<String, Object>> entities = List.of(createEntityData(1L, "Entity"));
            Map<String, Object> listResponse = createListResponse(entities);

            when(monicaClient.get(eq("/testentities"), any())).thenReturn(Mono.just(listResponse));
            when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

            // When
            testService.list(arguments).block();

            // Then
            verify(monicaClient).get(eq("/testentities"), argThat(params ->
                "100".equals(params.get("limit"))
            ));
        }

        @Test
        @DisplayName("should clamp limit to minimum of 1")
        void limitBelowMin_ClampedTo1() {
            // Given
            Map<String, Object> arguments = Map.of("limit", 0);
            List<Map<String, Object>> entities = List.of(createEntityData(1L, "Entity"));
            Map<String, Object> listResponse = createListResponse(entities);

            when(monicaClient.get(eq("/testentities"), any())).thenReturn(Mono.just(listResponse));
            when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

            // When
            testService.list(arguments).block();

            // Then
            verify(monicaClient).get(eq("/testentities"), argThat(params ->
                "1".equals(params.get("limit"))
            ));
        }

        @Test
        @DisplayName("should include filter fields in query params")
        void filterFields_IncludedInQueryParams() {
            // Given
            Map<String, Object> arguments = Map.of("contactId", 5L);
            List<Map<String, Object>> entities = List.of(createEntityData(1L, "Entity"));
            Map<String, Object> listResponse = createListResponse(entities);

            when(monicaClient.get(eq("/testentities"), any())).thenReturn(Mono.just(listResponse));
            when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

            // When
            testService.list(arguments).block();

            // Then
            verify(monicaClient).get(eq("/testentities"), argThat(params ->
                "5".equals(params.get("contact_id"))
            ));
        }

        @Test
        @DisplayName("should return formatted list with data and meta")
        void response_ContainsDataAndMeta() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            List<Map<String, Object>> entities = List.of(
                createEntityData(1L, "Entity 1"),
                createEntityData(2L, "Entity 2")
            );
            Map<String, Object> listResponse = createListResponse(entities, 1, 10, 50);

            when(monicaClient.get(eq("/testentities"), any())).thenReturn(Mono.just(listResponse));
            when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

            // When
            Map<String, Object> result = testService.list(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"));
            assertTrue(result.containsKey("meta"));
            assertTrue(result.containsKey("content"));

            @SuppressWarnings("unchecked")
            List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
            assertEquals(2, data.size());

            @SuppressWarnings("unchecked")
            Map<String, Object> meta = (Map<String, Object>) result.get("meta");
            assertEquals(1, meta.get("current_page"));
            assertEquals(10, meta.get("per_page"));
            assertEquals(50, meta.get("total"));
        }

        @Test
        @DisplayName("should map list response fields from API format")
        void listResponseFields_MappedFromApiFormat() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            Map<String, Object> apiEntity = new HashMap<>();
            apiEntity.put("id", 1L);
            apiEntity.put("body", "Test");
            apiEntity.put("contact_id", 10L);
            apiEntity.put("is_favorited", true);
            apiEntity.put("created_at", "2024-01-15T10:00:00Z");

            Map<String, Object> listResponse = createListResponse(List.of(apiEntity));

            when(monicaClient.get(eq("/testentities"), any())).thenReturn(Mono.just(listResponse));
            when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

            // When
            Map<String, Object> result = testService.list(arguments).block();

            // Then
            assertNotNull(result);
            @SuppressWarnings("unchecked")
            List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
            assertEquals(1, data.size());
            assertEquals(10L, data.get(0).get("contactId"));
            assertEquals(true, data.get(0).get("isFavorited"));
            assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        }

        @Test
        @DisplayName("should handle empty list response")
        void emptyList_Handled() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            Map<String, Object> listResponse = createListResponse(List.of(), 1, 10, 0);

            when(monicaClient.get(eq("/testentities"), any())).thenReturn(Mono.just(listResponse));
            when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

            // When
            Map<String, Object> result = testService.list(arguments).block();

            // Then
            assertNotNull(result);
            @SuppressWarnings("unchecked")
            List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
            assertTrue(data.isEmpty());
        }
    }

    // ========================================================================================
    // FIELD MAPPING TESTS
    // ========================================================================================

    @Nested
    @DisplayName("Field Mapping")
    class FieldMappingTests {

        @Test
        @DisplayName("mapToApiFormat should map all configured fields")
        void mapToApiFormat_MapsConfiguredFields() {
            // Given
            Map<String, Object> input = new HashMap<>();
            input.put("contactId", 1L);
            input.put("isFavorited", true);
            input.put("body", "test");

            // When - using the protected method via create
            Map<String, Object> apiResponse = createApiResponse(1L, "test");
            when(monicaClient.post(eq("/testentities"), any())).thenReturn(Mono.just(apiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("{}");

            testService.create(input).block();

            // Then
            verify(monicaClient).post(eq("/testentities"), argThat(data ->
                data.get("contact_id").equals(1L) &&
                Boolean.TRUE.equals(data.get("is_favorited")) &&
                "test".equals(data.get("body")) // unmapped field passes through
            ));
        }

        @Test
        @DisplayName("mapFromApiFormat should map all configured fields")
        void mapFromApiFormat_MapsConfiguredFields() {
            // Given
            Map<String, Object> apiData = new HashMap<>();
            apiData.put("id", 1L);
            apiData.put("contact_id", 10L);
            apiData.put("is_favorited", true);
            apiData.put("body", "test");
            apiData.put("created_at", "2024-01-15T10:00:00Z");
            apiData.put("updated_at", "2024-01-15T11:00:00Z");

            Map<String, Object> apiResponse = createSingleEntityResponse(apiData);
            when(monicaClient.get(eq("/testentities/1"), any())).thenReturn(Mono.just(apiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("{}");

            // When
            Map<String, Object> result = testService.get(Map.of("id", 1L)).block();

            // Then
            assertNotNull(result);
            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) result.get("data");
            assertEquals(10L, data.get("contactId"));
            assertEquals(true, data.get("isFavorited"));
            assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
            assertEquals("2024-01-15T11:00:00Z", data.get("updatedAt"));
        }

        @Test
        @DisplayName("should always map created_at and updated_at even if not configured")
        void timestampFields_AlwaysMapped() {
            // Given
            Map<String, Object> apiData = new HashMap<>();
            apiData.put("id", 1L);
            apiData.put("created_at", "2024-01-15T10:00:00Z");
            apiData.put("updated_at", "2024-01-15T11:00:00Z");

            Map<String, Object> apiResponse = createSingleEntityResponse(apiData);
            when(monicaClient.get(eq("/testentities/1"), any())).thenReturn(Mono.just(apiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("{}");

            // When
            Map<String, Object> result = testService.get(Map.of("id", 1L)).block();

            // Then
            assertNotNull(result);
            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) result.get("data");
            assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
            assertEquals("2024-01-15T11:00:00Z", data.get("updatedAt"));
        }

        @Test
        @DisplayName("unmapped fields should pass through unchanged")
        void unmappedFields_PassThrough() {
            // Given
            Map<String, Object> apiData = new HashMap<>();
            apiData.put("id", 1L);
            apiData.put("custom_field", "custom_value");

            Map<String, Object> apiResponse = createSingleEntityResponse(apiData);
            when(monicaClient.get(eq("/testentities/1"), any())).thenReturn(Mono.just(apiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("{}");

            // When
            Map<String, Object> result = testService.get(Map.of("id", 1L)).block();

            // Then
            assertNotNull(result);
            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) result.get("data");
            assertEquals("custom_value", data.get("custom_field"));
        }
    }

    // ========================================================================================
    // UNSUPPORTED OPERATIONS TESTS
    // ========================================================================================

    @Nested
    @DisplayName("Unsupported Operations")
    class UnsupportedOperationsTests {

        @Test
        @DisplayName("create should throw when not supported")
        void createNotSupported_ThrowsException() {
            // Given
            TestReadOnlyService readOnlyService = new TestReadOnlyService(monicaClient, contentFormatter);
            Map<String, Object> arguments = Map.of("contactId", 1L, "body", "test");

            // When & Then
            UnsupportedOperationException exception = assertThrows(UnsupportedOperationException.class, () ->
                readOnlyService.create(arguments).block()
            );
            assertEquals("ReadOnlyEntity does not support create operations", exception.getMessage());
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("update should throw when not supported")
        void updateNotSupported_ThrowsException() {
            // Given
            TestReadOnlyService readOnlyService = new TestReadOnlyService(monicaClient, contentFormatter);
            Map<String, Object> arguments = Map.of("id", 1L, "body", "test");

            // When & Then
            UnsupportedOperationException exception = assertThrows(UnsupportedOperationException.class, () ->
                readOnlyService.update(arguments).block()
            );
            assertEquals("ReadOnlyEntity does not support update operations", exception.getMessage());
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("delete should throw when not supported")
        void deleteNotSupported_ThrowsException() {
            // Given
            TestReadOnlyService readOnlyService = new TestReadOnlyService(monicaClient, contentFormatter);
            Map<String, Object> arguments = Map.of("id", 1L);

            // When & Then
            UnsupportedOperationException exception = assertThrows(UnsupportedOperationException.class, () ->
                readOnlyService.delete(arguments).block()
            );
            assertEquals("ReadOnlyEntity does not support delete operations", exception.getMessage());
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("list should throw when not supported")
        void listNotSupported_ThrowsException() {
            // Given
            TestNoListService noListService = new TestNoListService(monicaClient, contentFormatter);
            Map<String, Object> arguments = new HashMap<>();

            // When & Then
            UnsupportedOperationException exception = assertThrows(UnsupportedOperationException.class, () ->
                noListService.list(arguments).block()
            );
            assertEquals("NoListEntity does not support list operations", exception.getMessage());
            verifyNoInteractions(monicaClient);
        }
    }

    // ========================================================================================
    // HELPER METHODS
    // ========================================================================================

    private Map<String, Object> createApiResponse(Long id, String body) {
        Map<String, Object> data = new HashMap<>();
        data.put("id", id);
        data.put("body", body);
        data.put("contact_id", 10L);
        data.put("is_favorited", false);
        data.put("created_at", "2024-01-15T10:00:00Z");
        data.put("updated_at", "2024-01-15T10:00:00Z");
        return createSingleEntityResponse(data);
    }

    private Map<String, Object> createEntityData(Long id, String body) {
        Map<String, Object> data = new HashMap<>();
        data.put("id", id);
        data.put("body", body);
        data.put("contact_id", 10L);
        data.put("is_favorited", false);
        data.put("created_at", "2024-01-15T10:00:00Z");
        data.put("updated_at", "2024-01-15T10:00:00Z");
        return data;
    }

    // ========================================================================================
    // TEST SERVICE IMPLEMENTATIONS
    // ========================================================================================

    /**
     * A concrete test implementation of AbstractCrudService for testing.
     */
    static class TestEntityService extends AbstractCrudService<Object> {
        private final TestFieldMappingConfig config = new TestFieldMappingConfig();

        TestEntityService(MonicaHqClient monicaClient, ContentFormatter contentFormatter) {
            super(monicaClient, contentFormatter);
        }

        @Override
        protected FieldMappingConfig getFieldMappingConfig() {
            return config;
        }
    }

    /**
     * A test implementation for testing read-only services.
     */
    static class TestReadOnlyService extends AbstractCrudService<Object> {
        private final ReadOnlyFieldMappingConfig config = new ReadOnlyFieldMappingConfig();

        TestReadOnlyService(MonicaHqClient monicaClient, ContentFormatter contentFormatter) {
            super(monicaClient, contentFormatter);
        }

        @Override
        protected FieldMappingConfig getFieldMappingConfig() {
            return config;
        }
    }

    /**
     * A test implementation for testing services without list support.
     */
    static class TestNoListService extends AbstractCrudService<Object> {
        private final NoListFieldMappingConfig config = new NoListFieldMappingConfig();

        TestNoListService(MonicaHqClient monicaClient, ContentFormatter contentFormatter) {
            super(monicaClient, contentFormatter);
        }

        @Override
        protected FieldMappingConfig getFieldMappingConfig() {
            return config;
        }
    }

    /**
     * A test implementation of FieldMappingConfig for testing.
     */
    static class TestFieldMappingConfig implements FieldMappingConfig {
        @Override
        public String getEndpointPath() {
            return "/testentities";
        }

        @Override
        public String getEntityName() {
            return "TestEntity";
        }

        @Override
        public Map<String, String> getToApiMappings() {
            return Map.of(
                "contactId", "contact_id",
                "isFavorited", "is_favorited"
            );
        }

        @Override
        public Map<String, String> getFromApiMappings() {
            return Map.of(
                "contact_id", "contactId",
                "is_favorited", "isFavorited"
            );
        }

        @Override
        public Set<String> getRequiredCreateFields() {
            return Set.of("contactId", "body");
        }

        @Override
        public List<String> getListFilterFields() {
            return List.of("contactId");
        }

        @Override
        public Map<String, Object> getCreateDefaults() {
            return Map.of("isFavorited", false);
        }
    }

    /**
     * A read-only field mapping config for testing unsupported operations.
     */
    static class ReadOnlyFieldMappingConfig implements FieldMappingConfig {
        @Override
        public String getEndpointPath() {
            return "/readonly";
        }

        @Override
        public String getEntityName() {
            return "ReadOnlyEntity";
        }

        @Override
        public Map<String, String> getToApiMappings() {
            return Map.of();
        }

        @Override
        public Map<String, String> getFromApiMappings() {
            return Map.of();
        }

        @Override
        public Set<String> getRequiredCreateFields() {
            return Set.of();
        }

        @Override
        public boolean supportsCreate() {
            return false;
        }

        @Override
        public boolean supportsUpdate() {
            return false;
        }

        @Override
        public boolean supportsDelete() {
            return false;
        }
    }

    /**
     * A field mapping config without list support for testing.
     */
    static class NoListFieldMappingConfig implements FieldMappingConfig {
        @Override
        public String getEndpointPath() {
            return "/nolist";
        }

        @Override
        public String getEntityName() {
            return "NoListEntity";
        }

        @Override
        public Map<String, String> getToApiMappings() {
            return Map.of();
        }

        @Override
        public Map<String, String> getFromApiMappings() {
            return Map.of();
        }

        @Override
        public Set<String> getRequiredCreateFields() {
            return Set.of();
        }

        @Override
        public boolean supportsList() {
            return false;
        }
    }
}
