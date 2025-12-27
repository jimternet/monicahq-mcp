package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.TagFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Unit tests for TagService covering CRUD operations, contact association,
 * list operations, validation, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class TagServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private TagFieldMappingConfig tagFieldMappingConfig;

    private TagService tagService;

    private Map<String, Object> mockTagData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        tagFieldMappingConfig = new TagFieldMappingConfig();
        tagService = new TagService(monicaClient, contentFormatter, tagFieldMappingConfig);

        mockTagData = tagBuilder()
            .id(1L)
            .name("Family")
            .contactCount(5)
            .build();

        mockApiResponse = createSingleEntityResponse(mockTagData);
    }

    // ========================================================================================
    // CREATE TAG TESTS
    // ========================================================================================

    @Test
    void createTag_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Work");

        when(monicaClient.post(eq("/tags"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted tag JSON");

        // When
        Map<String, Object> result = tagService.createTag(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted tag JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/tags"), argThat(data ->
            "Work".equals(data.get("name"))
        ));
    }

    @Test
    void createTag_MissingName_ThrowsException() {
        // Given - has other field but no name
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("description", "some description");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            tagService.createTag(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createTag_NullName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            tagService.createTag(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createTag_EmptyName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            tagService.createTag(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createTag_WhitespaceName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "   ");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            tagService.createTag(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createTag_EmptyArguments_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            tagService.createTag(arguments).block();
        });
        assertEquals("Tag arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createTag_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            tagService.createTag(null).block();
        });
        assertEquals("Tag arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createTag_WithNameSlug_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Work Contacts");
        arguments.put("nameSlug", "work-contacts");

        when(monicaClient.post(eq("/tags"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted tag JSON");

        // When
        tagService.createTag(arguments).block();

        // Then - verify nameSlug is mapped to name_slug
        verify(monicaClient).post(eq("/tags"), argThat(data ->
            "work-contacts".equals(data.get("name_slug")) &&
            !data.containsKey("nameSlug")
        ));
    }

    // ========================================================================================
    // GET TAG TESTS
    // ========================================================================================

    @Test
    void getTag_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/tags/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted tag JSON");

        // When
        Map<String, Object> result = tagService.getTag(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/tags/1"), any());
    }

    @Test
    void getTag_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            tagBuilder().id(42L).name("Test Tag").build()
        );

        when(monicaClient.get(eq("/tags/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted tag JSON");

        // When
        Map<String, Object> result = tagService.getTag(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/tags/42"), any());
    }

    @Test
    void getTag_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            tagBuilder().id(123L).name("Test Tag").build()
        );

        when(monicaClient.get(eq("/tags/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted tag JSON");

        // When
        Map<String, Object> result = tagService.getTag(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/tags/123"), any());
    }

    @Test
    void getTag_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("name", "Test");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            tagService.getTag(arguments).block();
        });
        assertEquals("Tag ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getTag_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            tagService.getTag(null).block();
        });
        assertEquals("Tag ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getTag_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            tagService.getTag(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid tag ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getTag_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("name", "Family");
        apiData.put("name_slug", "family");
        apiData.put("contact_count", 10);
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T09:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/tags/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = tagService.getTag(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals("family", data.get("nameSlug"));
        assertEquals(10, data.get("contactCount"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T09:00:00Z", data.get("updatedAt"));
    }

    // ========================================================================================
    // UPDATE TAG TESTS
    // ========================================================================================

    @Test
    void updateTag_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("name", "Updated Tag Name");

        when(monicaClient.put(eq("/tags/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted tag JSON");

        // When
        Map<String, Object> result = tagService.updateTag(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/tags/1"), argThat(data ->
            "Updated Tag Name".equals(data.get("name"))
        ));
    }

    @Test
    void updateTag_RemovesIdFromUpdateData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 5L);
        arguments.put("name", "Updated Tag");

        when(monicaClient.put(eq("/tags/5"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted tag JSON");

        // When
        tagService.updateTag(arguments).block();

        // Then - verify that id is NOT included in the request body
        verify(monicaClient).put(eq("/tags/5"), argThat(data ->
            !data.containsKey("id")
        ));
    }

    @Test
    void updateTag_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("name", "Updated Tag");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            tagService.updateTag(arguments).block();
        });
        assertEquals("Tag ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateTag_WithNameSlug_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("nameSlug", "new-slug");

        when(monicaClient.put(eq("/tags/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted tag JSON");

        // When
        tagService.updateTag(arguments).block();

        // Then - verify nameSlug is mapped to name_slug
        verify(monicaClient).put(eq("/tags/1"), argThat(data ->
            "new-slug".equals(data.get("name_slug")) &&
            !data.containsKey("nameSlug")
        ));
    }

    @Test
    void updateTag_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "42");
        arguments.put("name", "Updated Tag");

        when(monicaClient.put(eq("/tags/42"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted tag JSON");

        // When
        tagService.updateTag(arguments).block();

        // Then
        verify(monicaClient).put(eq("/tags/42"), any());
    }

    @Test
    void updateTag_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");
        arguments.put("name", "Updated Tag");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            tagService.updateTag(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid tag ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateTag_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            tagService.updateTag(null).block();
        });
        assertEquals("Tag ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // DELETE TAG TESTS
    // ========================================================================================

    @Test
    void deleteTag_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/tags/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Tag"), eq(1L), eq(true), anyString()
        )).thenReturn("Tag deleted successfully");

        // When
        Map<String, Object> result = tagService.deleteTag(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));

        verify(monicaClient).delete(eq("/tags/1"));
    }

    @Test
    void deleteTag_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/tags/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Tag"), eq(99L), eq(true), anyString()
        )).thenReturn("Tag deleted successfully");

        // When
        Map<String, Object> result = tagService.deleteTag(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/tags/99"));
    }

    @Test
    void deleteTag_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 55);
        Map<String, Object> deleteResponse = createDeleteResponse(55L);

        when(monicaClient.delete(eq("/tags/55"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Tag"), eq(55L), eq(true), anyString()
        )).thenReturn("Tag deleted successfully");

        // When
        Map<String, Object> result = tagService.deleteTag(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/tags/55"));
    }

    @Test
    void deleteTag_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("name", "Test");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            tagService.deleteTag(arguments).block();
        });
        assertEquals("Tag ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteTag_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            tagService.deleteTag(null).block();
        });
        assertEquals("Tag ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteTag_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            tagService.deleteTag(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid tag ID format"));
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST TAGS TESTS
    // ========================================================================================

    @Test
    void listTags_WithPagination_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> tags = List.of(
            tagBuilder().id(1L).name("Family").build(),
            tagBuilder().id(2L).name("Work").build()
        );
        Map<String, Object> listResponse = createListResponse(tags, 2, 20, 50);

        when(monicaClient.get(eq("/tags"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = tagService.listTags(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/tags"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listTags_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> tags = List.of(
            tagBuilder().id(1L).name("Family").build()
        );
        Map<String, Object> listResponse = createListResponse(tags);

        when(monicaClient.get(eq("/tags"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        tagService.listTags(arguments).block();

        // Then - verify default pagination values
        verify(monicaClient).get(eq("/tags"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listTags_WithSearch_IncludesQueryParam() {
        // Given
        Map<String, Object> arguments = Map.of("search", "family");

        List<Map<String, Object>> tags = List.of(
            tagBuilder().id(1L).name("Family Friends").build()
        );
        Map<String, Object> listResponse = createListResponse(tags);

        when(monicaClient.get(eq("/tags"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        tagService.listTags(arguments).block();

        // Then - verify search is mapped to "query" param
        verify(monicaClient).get(eq("/tags"), argThat(params ->
            "family".equals(params.get("query"))
        ));
    }

    @Test
    void listTags_LimitAboveMaximum_ClampsTo100() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 200);

        List<Map<String, Object>> tags = List.of(
            tagBuilder().id(1L).name("Tag 1").build()
        );
        Map<String, Object> listResponse = createListResponse(tags);

        when(monicaClient.get(eq("/tags"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        tagService.listTags(arguments).block();

        // Then - verify limit is clamped to 100
        verify(monicaClient).get(eq("/tags"), argThat(params ->
            "100".equals(params.get("limit"))
        ));
    }

    @Test
    void listTags_LimitBelowMinimum_ClampsTo1() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 0);

        List<Map<String, Object>> tags = List.of(
            tagBuilder().id(1L).name("Tag 1").build()
        );
        Map<String, Object> listResponse = createListResponse(tags);

        when(monicaClient.get(eq("/tags"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        tagService.listTags(arguments).block();

        // Then - verify limit is clamped to 1
        verify(monicaClient).get(eq("/tags"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listTags_NegativeLimit_ClampsTo1() {
        // Given
        Map<String, Object> arguments = Map.of("limit", -5);

        List<Map<String, Object>> tags = List.of(
            tagBuilder().id(1L).name("Tag 1").build()
        );
        Map<String, Object> listResponse = createListResponse(tags);

        when(monicaClient.get(eq("/tags"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        tagService.listTags(arguments).block();

        // Then - verify limit is clamped to 1
        verify(monicaClient).get(eq("/tags"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listTags_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> tags = List.of(
            tagBuilder().id(1L).name("Tag 1").build()
        );
        Map<String, Object> listResponse = createListResponse(tags, 1, 10, 100);

        when(monicaClient.get(eq("/tags"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = tagService.listTags(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) result.get("meta");
        assertEquals(1, meta.get("current_page"));
        assertEquals(10, meta.get("per_page"));
        assertEquals(100, meta.get("total"));
        assertEquals(10, meta.get("last_page"));
    }

    @Test
    void listTags_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/tags"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = tagService.listTags(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listTags_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> tags = List.of(
            tagBuilder().id(1L).name("Tag 1").build()
        );
        Map<String, Object> listResponse = createListResponse(tags);

        when(monicaClient.get(eq("/tags"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        tagService.listTags(arguments).block();

        // Then
        verify(monicaClient).get(eq("/tags"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listTags_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> tagWithSnakeCase = new HashMap<>();
        tagWithSnakeCase.put("id", 1L);
        tagWithSnakeCase.put("name", "Work");
        tagWithSnakeCase.put("name_slug", "work");
        tagWithSnakeCase.put("contact_count", 10);
        tagWithSnakeCase.put("created_at", "2024-01-15T10:00:00Z");
        tagWithSnakeCase.put("updated_at", "2024-01-15T10:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(tagWithSnakeCase));

        when(monicaClient.get(eq("/tags"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = tagService.listTags(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify snake_case is mapped to camelCase
        assertEquals("work", data.get(0).get("nameSlug"));
        assertEquals(10, data.get(0).get("contactCount"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("updatedAt"));
    }

    @Test
    void listTags_WithStringPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("page", "3");

        List<Map<String, Object>> tags = List.of(
            tagBuilder().id(1L).name("Tag 1").build()
        );
        Map<String, Object> listResponse = createListResponse(tags, 3, 10, 30);

        when(monicaClient.get(eq("/tags"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        tagService.listTags(arguments).block();

        // Then
        verify(monicaClient).get(eq("/tags"), argThat(params ->
            "3".equals(params.get("page"))
        ));
    }

    @Test
    void listTags_NullSearchValue_DoesNotIncludeQueryParam() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("search", null);

        List<Map<String, Object>> tags = List.of(
            tagBuilder().id(1L).name("Tag 1").build()
        );
        Map<String, Object> listResponse = createListResponse(tags);

        when(monicaClient.get(eq("/tags"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        tagService.listTags(arguments).block();

        // Then - verify query param is not included
        verify(monicaClient).get(eq("/tags"), argThat(params ->
            !params.containsKey("query")
        ));
    }

    // ========================================================================================
    // LIST CONTACTS BY TAG TESTS
    // ========================================================================================

    @Test
    void listContactsByTag_ValidArgs_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        List<Map<String, Object>> contacts = List.of(
            contactBuilder().id(10L).firstName("John").lastName("Doe").build(),
            contactBuilder().id(11L).firstName("Jane").lastName("Smith").build()
        );
        Map<String, Object> listResponse = createListResponse(contacts);

        when(monicaClient.get(eq("/tags/1/contacts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted contacts JSON");

        // When
        Map<String, Object> result = tagService.listContactsByTag(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/tags/1/contacts"), any());
    }

    @Test
    void listContactsByTag_WithPagination_IncludesQueryParams() {
        // Given
        Map<String, Object> arguments = Map.of(
            "id", 1L,
            "page", 2,
            "limit", 15
        );

        List<Map<String, Object>> contacts = List.of(
            contactBuilder().id(10L).firstName("John").build()
        );
        Map<String, Object> listResponse = createListResponse(contacts, 2, 15, 30);

        when(monicaClient.get(eq("/tags/1/contacts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted contacts JSON");

        // When
        tagService.listContactsByTag(arguments).block();

        // Then
        verify(monicaClient).get(eq("/tags/1/contacts"), argThat(params ->
            "2".equals(params.get("page")) &&
            "15".equals(params.get("limit"))
        ));
    }

    @Test
    void listContactsByTag_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            tagService.listContactsByTag(arguments).block();
        });
        assertEquals("Tag ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void listContactsByTag_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        List<Map<String, Object>> contacts = List.of(
            contactBuilder().id(10L).firstName("John").build()
        );
        Map<String, Object> listResponse = createListResponse(contacts);

        when(monicaClient.get(eq("/tags/42/contacts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted contacts JSON");

        // When
        tagService.listContactsByTag(arguments).block();

        // Then
        verify(monicaClient).get(eq("/tags/42/contacts"), any());
    }

    @Test
    void listContactsByTag_MapsContactFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> contactWithSnakeCase = new HashMap<>();
        contactWithSnakeCase.put("id", 10L);
        contactWithSnakeCase.put("first_name", "John");
        contactWithSnakeCase.put("last_name", "Doe");
        contactWithSnakeCase.put("gender_id", 1);
        contactWithSnakeCase.put("is_birthdate_known", true);
        contactWithSnakeCase.put("is_deceased", false);
        contactWithSnakeCase.put("is_deceased_date_known", false);
        contactWithSnakeCase.put("job_title", "Engineer");
        contactWithSnakeCase.put("created_at", "2024-01-15T10:00:00Z");
        contactWithSnakeCase.put("updated_at", "2024-01-15T10:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(contactWithSnakeCase));

        when(monicaClient.get(eq("/tags/1/contacts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted contacts JSON");

        // When
        Map<String, Object> result = tagService.listContactsByTag(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        Map<String, Object> contact = data.get(0);
        assertEquals("John", contact.get("firstName"));
        assertEquals("Doe", contact.get("lastName"));
        assertEquals(1, contact.get("genderId"));
        assertEquals(true, contact.get("isBirthdateKnown"));
        assertEquals(false, contact.get("isDeceased"));
        assertEquals(false, contact.get("isDeceasedDateKnown"));
        assertEquals("Engineer", contact.get("jobTitle"));
        assertEquals("2024-01-15T10:00:00Z", contact.get("createdAt"));
        assertEquals("2024-01-15T10:00:00Z", contact.get("updatedAt"));
    }

    @Test
    void listContactsByTag_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L, "page", 1, "limit", 10);

        List<Map<String, Object>> contacts = List.of(
            contactBuilder().id(10L).firstName("John").build()
        );
        Map<String, Object> listResponse = createListResponse(contacts, 1, 10, 25);

        when(monicaClient.get(eq("/tags/1/contacts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted contacts JSON");

        // When
        Map<String, Object> result = tagService.listContactsByTag(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) result.get("meta");
        assertEquals(1, meta.get("current_page"));
        assertEquals(10, meta.get("per_page"));
        assertEquals(25, meta.get("total"));
    }

    @Test
    void listContactsByTag_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            tagService.listContactsByTag(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid tag ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void listContactsByTag_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/tags/1/contacts"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = tagService.listContactsByTag(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listContactsByTag_LimitClamping_AppliesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of(
            "id", 1L,
            "limit", 200  // Above maximum
        );

        List<Map<String, Object>> contacts = List.of(
            contactBuilder().id(10L).firstName("John").build()
        );
        Map<String, Object> listResponse = createListResponse(contacts);

        when(monicaClient.get(eq("/tags/1/contacts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted contacts JSON");

        // When
        tagService.listContactsByTag(arguments).block();

        // Then - verify limit is clamped to 100
        verify(monicaClient).get(eq("/tags/1/contacts"), argThat(params ->
            "100".equals(params.get("limit"))
        ));
    }
}
