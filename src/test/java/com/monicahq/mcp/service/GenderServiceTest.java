package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.GenderFieldMappingConfig;
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
 * Unit tests for GenderService covering listGenders operation,
 * response formatting, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class GenderServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private GenderService genderService;

    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        GenderFieldMappingConfig fieldMappingConfig = new GenderFieldMappingConfig();
        genderService = new GenderService(monicaClient, contentFormatter, fieldMappingConfig);

        List<Map<String, Object>> genders = List.of(
            genderBuilder().id(1L).name("Male").build(),
            genderBuilder().id(2L).name("Female").build(),
            genderBuilder().id(3L).name("Other").build()
        );
        mockApiResponse = createListResponse(genders, 1, 10, 3);
    }

    // ========================================================================================
    // LIST GENDERS TESTS
    // ========================================================================================

    @Test
    void listGenders_EmptyArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        when(monicaClient.get(eq("/genders"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted genders JSON");

        // When
        Map<String, Object> result = genderService.listGenders(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted genders JSON", content.get(0).get("text"));

        verify(monicaClient).get(eq("/genders"), any());
    }

    @Test
    void listGenders_NullArgs_ReturnsFormattedResponse() {
        // Given
        when(monicaClient.get(eq("/genders"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted genders JSON");

        // When
        Map<String, Object> result = genderService.listGenders(null).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        verify(monicaClient).get(eq("/genders"), any());
    }

    @Test
    void listGenders_ReturnsDataList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> genders = List.of(
            genderBuilder().id(1L).name("Male").build(),
            genderBuilder().id(2L).name("Female").build()
        );
        Map<String, Object> apiResponse = createListResponse(genders, 1, 10, 2);

        when(monicaClient.get(eq("/genders"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted genders JSON");

        // When
        Map<String, Object> result = genderService.listGenders(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());
    }

    @Test
    void listGenders_EmptyResult_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> genders = List.of();
        Map<String, Object> apiResponse = createListResponse(genders, 1, 10, 0);

        when(monicaClient.get(eq("/genders"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = genderService.listGenders(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listGenders_IncludesMetadata_WhenPresent() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> genders = List.of(
            genderBuilder().id(1L).name("Male").build(),
            genderBuilder().id(2L).name("Female").build(),
            genderBuilder().id(3L).name("Other").build()
        );
        Map<String, Object> apiResponse = createListResponse(genders, 1, 10, 3);

        when(monicaClient.get(eq("/genders"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted genders JSON");

        // When
        Map<String, Object> result = genderService.listGenders(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("meta"));
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) result.get("meta");
        assertEquals(1, meta.get("current_page"));
        assertEquals(10, meta.get("per_page"));
        assertEquals(3, meta.get("total"));
    }

    @Test
    void listGenders_NoMeta_HandlesGracefully() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> genders = List.of(
            genderBuilder().id(1L).name("Male").build()
        );
        Map<String, Object> apiResponse = new HashMap<>();
        apiResponse.put("data", genders);
        // No meta field

        when(monicaClient.get(eq("/genders"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted genders JSON");

        // When
        Map<String, Object> result = genderService.listGenders(arguments).block();

        // Then
        assertNotNull(result);
        assertFalse(result.containsKey("meta"));
    }

    @Test
    void listGenders_ContentFieldFormat_IsCorrect() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        when(monicaClient.get(eq("/genders"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list content");

        // When
        Map<String, Object> result = genderService.listGenders(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted list content", content.get(0).get("text"));
    }

    @Test
    void listGenders_MultipleGenders_ReturnsAllItems() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> genders = List.of(
            genderBuilder().id(1L).name("Male").build(),
            genderBuilder().id(2L).name("Female").build(),
            genderBuilder().id(3L).name("Other").build(),
            genderBuilder().id(4L).name("Rather not say").build()
        );
        Map<String, Object> apiResponse = createListResponse(genders, 1, 10, 4);

        when(monicaClient.get(eq("/genders"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted genders JSON");

        // When
        Map<String, Object> result = genderService.listGenders(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(4, data.size());
    }

    @Test
    void listGenders_GenderFields_ArePresentInResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> gender = new HashMap<>();
        gender.put("id", 1L);
        gender.put("name", "Male");
        gender.put("type", "M");

        List<Map<String, Object>> genders = List.of(gender);
        Map<String, Object> apiResponse = createListResponse(genders, 1, 10, 1);

        when(monicaClient.get(eq("/genders"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted genders JSON");

        // When
        Map<String, Object> result = genderService.listGenders(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());
        assertEquals(1L, data.get(0).get("id"));
        assertEquals("Male", data.get(0).get("name"));
        assertEquals("M", data.get(0).get("type"));
    }

    @Test
    void listGenders_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        when(monicaClient.get(eq("/genders"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted genders JSON");

        // When
        genderService.listGenders(arguments).block();

        // Then
        verify(monicaClient).get(eq("/genders"), any());
        verifyNoMoreInteractions(monicaClient);
    }

    @Test
    void listGenders_FormatsApiResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        when(monicaClient.get(eq("/genders"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted genders JSON");

        // When
        genderService.listGenders(arguments).block();

        // Then
        verify(contentFormatter).formatListAsEscapedJson(eq(mockApiResponse));
    }

    @Test
    void listGenders_WithExtraArgs_IgnoresUnknownArgs() {
        // Given - service doesn't use args, but should handle them gracefully
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("unknown", "value");
        arguments.put("filter", "male");

        when(monicaClient.get(eq("/genders"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted genders JSON");

        // When
        Map<String, Object> result = genderService.listGenders(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/genders"), any());
    }

    // ========================================================================================
    // GENDER DATA BUILDER
    // ========================================================================================

    /**
     * Builder for creating mock gender data.
     */
    protected GenderDataBuilder genderBuilder() {
        return new GenderDataBuilder();
    }

    public static class GenderDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public GenderDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("name", "Male");
        }

        public GenderDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public GenderDataBuilder name(String name) {
            data.put("name", name);
            return this;
        }

        public GenderDataBuilder type(String type) {
            data.put("type", type);
            return this;
        }

        public GenderDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }
}
