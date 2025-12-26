package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.util.ContentFormatter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
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
 * Unit tests for ActivityTypeService covering activity type CRUD operations.
 */
@ExtendWith(MockitoExtension.class)
class ActivityTypeServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    @InjectMocks
    private ActivityTypeService activityTypeService;

    private Map<String, Object> mockActivityTypeData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        mockActivityTypeData = activityTypeBuilder()
            .id(1L)
            .name("Coffee Meeting")
            .categoryId(5)
            .build();

        mockApiResponse = createSingleEntityResponse(mockActivityTypeData);
    }

    // ========================================================================================
    // CREATE ACTIVITY TYPE TESTS
    // ========================================================================================

    @Test
    void createActivityType_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Coffee Meeting");
        arguments.put("categoryId", 5);

        when(monicaClient.post(eq("/activitytypes"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity type JSON");

        // When
        Map<String, Object> result = activityTypeService.createActivityType(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted activity type JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/activitytypes"), argThat(data ->
            "Coffee Meeting".equals(data.get("name")) &&
            Integer.valueOf(5).equals(data.get("category_id"))
        ));
    }

    @Test
    void createActivityType_MissingName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("categoryId", 5);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeService.createActivityType(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createActivityType_NullName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", null);
        arguments.put("categoryId", 5);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeService.createActivityType(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createActivityType_EmptyName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "");
        arguments.put("categoryId", 5);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeService.createActivityType(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createActivityType_WhitespaceName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "   ");
        arguments.put("categoryId", 5);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeService.createActivityType(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createActivityType_MapsCategoryIdField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Coffee Meeting");
        arguments.put("categoryId", 10);

        when(monicaClient.post(eq("/activitytypes"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity type JSON");

        // When
        activityTypeService.createActivityType(arguments).block();

        // Then - verify categoryId is mapped to category_id
        verify(monicaClient).post(eq("/activitytypes"), argThat(data ->
            Integer.valueOf(10).equals(data.get("category_id")) &&
            !data.containsKey("categoryId")
        ));
    }

    @Test
    void createActivityType_WithDescription_PassesThroughUnchanged() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Coffee Meeting");
        arguments.put("description", "Meet for coffee and chat");

        when(monicaClient.post(eq("/activitytypes"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity type JSON");

        // When
        activityTypeService.createActivityType(arguments).block();

        // Then - verify description passes through unchanged
        verify(monicaClient).post(eq("/activitytypes"), argThat(data ->
            "Meet for coffee and chat".equals(data.get("description"))
        ));
    }

    @Test
    void createActivityType_WithIcon_PassesThroughUnchanged() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Coffee Meeting");
        arguments.put("icon", "fa-coffee");

        when(monicaClient.post(eq("/activitytypes"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity type JSON");

        // When
        activityTypeService.createActivityType(arguments).block();

        // Then - verify icon passes through unchanged
        verify(monicaClient).post(eq("/activitytypes"), argThat(data ->
            "fa-coffee".equals(data.get("icon"))
        ));
    }

    @Test
    void createActivityType_WithAllFields_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Lunch Meeting");
        arguments.put("categoryId", 3);
        arguments.put("description", "Lunch with contact");
        arguments.put("icon", "fa-utensils");

        when(monicaClient.post(eq("/activitytypes"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity type JSON");

        // When
        activityTypeService.createActivityType(arguments).block();

        // Then - verify all fields are mapped correctly
        verify(monicaClient).post(eq("/activitytypes"), argThat(data ->
            "Lunch Meeting".equals(data.get("name")) &&
            Integer.valueOf(3).equals(data.get("category_id")) &&
            "Lunch with contact".equals(data.get("description")) &&
            "fa-utensils".equals(data.get("icon"))
        ));
    }

    // ========================================================================================
    // GET ACTIVITY TYPE TESTS
    // ========================================================================================

    @Test
    void getActivityType_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/activitytypes/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity type JSON");

        // When
        Map<String, Object> result = activityTypeService.getActivityType(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/activitytypes/1"), any());
    }

    @Test
    void getActivityType_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            activityTypeBuilder().id(42L).build()
        );

        when(monicaClient.get(eq("/activitytypes/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity type JSON");

        // When
        Map<String, Object> result = activityTypeService.getActivityType(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/activitytypes/42"), any());
    }

    @Test
    void getActivityType_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            activityTypeBuilder().id(123L).build()
        );

        when(monicaClient.get(eq("/activitytypes/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity type JSON");

        // When
        Map<String, Object> result = activityTypeService.getActivityType(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/activitytypes/123"), any());
    }

    @Test
    void getActivityType_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("name", "Coffee Meeting");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeService.getActivityType(arguments).block();
        });
        assertEquals("id is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getActivityType_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeService.getActivityType(arguments).block();
        });
        assertEquals("id is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getActivityType_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeService.getActivityType(arguments).block();
        });
        assertEquals("id must be a valid number", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getActivityType_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("name", "Coffee Meeting");
        apiData.put("category_id", 5);
        apiData.put("description", "Meeting for coffee");
        apiData.put("icon", "fa-coffee");
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T09:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/activitytypes/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = activityTypeService.getActivityType(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals(5, data.get("categoryId"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T09:00:00Z", data.get("updatedAt"));
        // Name should pass through unchanged
        assertEquals("Coffee Meeting", data.get("name"));
        // Description and icon should pass through unchanged
        assertEquals("Meeting for coffee", data.get("description"));
        assertEquals("fa-coffee", data.get("icon"));
    }

    @Test
    void getActivityType_WithoutDataWrapper_StillWorks() {
        // Given - response without "data" wrapper (edge case)
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> directResponse = new HashMap<>();
        directResponse.put("id", 1L);
        directResponse.put("name", "Coffee Meeting");
        directResponse.put("category_id", 5);

        when(monicaClient.get(eq("/activitytypes/1"), any())).thenReturn(Mono.just(directResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = activityTypeService.getActivityType(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertEquals(5, data.get("categoryId"));
    }

    // ========================================================================================
    // UPDATE ACTIVITY TYPE TESTS
    // ========================================================================================

    @Test
    void updateActivityType_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("name", "Updated Coffee Meeting");
        arguments.put("categoryId", 6);

        when(monicaClient.put(eq("/activitytypes/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity type JSON");

        // When
        Map<String, Object> result = activityTypeService.updateActivityType(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/activitytypes/1"), argThat(data ->
            "Updated Coffee Meeting".equals(data.get("name")) &&
            Integer.valueOf(6).equals(data.get("category_id"))
        ));
    }

    @Test
    void updateActivityType_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Updated Coffee Meeting");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeService.updateActivityType(arguments).block();
        });
        assertEquals("id is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateActivityType_MissingName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("categoryId", 5);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeService.updateActivityType(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateActivityType_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "42");
        arguments.put("name", "Updated Activity Type");

        when(monicaClient.put(eq("/activitytypes/42"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity type JSON");

        // When
        activityTypeService.updateActivityType(arguments).block();

        // Then
        verify(monicaClient).put(eq("/activitytypes/42"), any());
    }

    @Test
    void updateActivityType_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");
        arguments.put("name", "Updated Activity Type");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeService.updateActivityType(arguments).block();
        });
        assertEquals("id must be a valid number", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateActivityType_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("name", "Dinner Meeting");
        arguments.put("categoryId", 8);
        arguments.put("description", "Updated description");
        arguments.put("icon", "fa-utensils");

        when(monicaClient.put(eq("/activitytypes/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity type JSON");

        // When
        activityTypeService.updateActivityType(arguments).block();

        // Then - verify all fields are mapped correctly
        verify(monicaClient).put(eq("/activitytypes/1"), argThat(data ->
            "Dinner Meeting".equals(data.get("name")) &&
            Integer.valueOf(8).equals(data.get("category_id")) &&
            !data.containsKey("categoryId") &&
            "Updated description".equals(data.get("description")) &&
            "fa-utensils".equals(data.get("icon"))
        ));
    }

    // ========================================================================================
    // DELETE ACTIVITY TYPE TESTS
    // ========================================================================================

    @Test
    void deleteActivityType_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/activitytypes/1"))).thenReturn(Mono.just(deleteResponse));

        // When
        Map<String, Object> result = activityTypeService.deleteActivityType(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("data"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertTrue(content.get(0).get("text").toString().contains("deleted successfully"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertEquals(true, data.get("deleted"));
        assertEquals(1L, data.get("id"));

        verify(monicaClient).delete(eq("/activitytypes/1"));
    }

    @Test
    void deleteActivityType_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/activitytypes/99"))).thenReturn(Mono.just(deleteResponse));

        // When
        Map<String, Object> result = activityTypeService.deleteActivityType(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/activitytypes/99"));
    }

    @Test
    void deleteActivityType_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 55);
        Map<String, Object> deleteResponse = createDeleteResponse(55L);

        when(monicaClient.delete(eq("/activitytypes/55"))).thenReturn(Mono.just(deleteResponse));

        // When
        Map<String, Object> result = activityTypeService.deleteActivityType(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/activitytypes/55"));
    }

    @Test
    void deleteActivityType_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("name", "Coffee Meeting");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeService.deleteActivityType(arguments).block();
        });
        assertEquals("id is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteActivityType_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeService.deleteActivityType(arguments).block();
        });
        assertEquals("id is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteActivityType_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeService.deleteActivityType(arguments).block();
        });
        assertEquals("id must be a valid number", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST ACTIVITY TYPES TESTS
    // ========================================================================================

    @Test
    void listActivityTypes_WithPagination_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> activityTypes = List.of(
            activityTypeBuilder().id(1L).name("Coffee Meeting").build(),
            activityTypeBuilder().id(2L).name("Lunch Meeting").build()
        );
        Map<String, Object> listResponse = createListResponse(activityTypes, 2, 20, 50);

        when(monicaClient.get(eq("/activitytypes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = activityTypeService.listActivityTypes(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/activitytypes"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listActivityTypes_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> activityTypes = List.of(
            activityTypeBuilder().id(1L).build()
        );
        Map<String, Object> listResponse = createListResponse(activityTypes);

        when(monicaClient.get(eq("/activitytypes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        activityTypeService.listActivityTypes(arguments).block();

        // Then - verify default pagination values
        verify(monicaClient).get(eq("/activitytypes"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listActivityTypes_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> activityTypes = List.of(
            activityTypeBuilder().id(1L).build()
        );
        Map<String, Object> listResponse = createListResponse(activityTypes, 1, 10, 100);

        when(monicaClient.get(eq("/activitytypes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = activityTypeService.listActivityTypes(arguments).block();

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
    void listActivityTypes_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/activitytypes"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = activityTypeService.listActivityTypes(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listActivityTypes_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> activityTypes = List.of(
            activityTypeBuilder().id(1L).build()
        );
        Map<String, Object> listResponse = createListResponse(activityTypes);

        when(monicaClient.get(eq("/activitytypes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        activityTypeService.listActivityTypes(arguments).block();

        // Then
        verify(monicaClient).get(eq("/activitytypes"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listActivityTypes_StringPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("page", "3");

        List<Map<String, Object>> activityTypes = List.of(
            activityTypeBuilder().id(1L).build()
        );
        Map<String, Object> listResponse = createListResponse(activityTypes, 3, 10, 30);

        when(monicaClient.get(eq("/activitytypes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        activityTypeService.listActivityTypes(arguments).block();

        // Then
        verify(monicaClient).get(eq("/activitytypes"), argThat(params ->
            "3".equals(params.get("page"))
        ));
    }

    @Test
    void listActivityTypes_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> activityTypeWithSnakeCaseFields = new HashMap<>();
        activityTypeWithSnakeCaseFields.put("id", 1L);
        activityTypeWithSnakeCaseFields.put("name", "Coffee Meeting");
        activityTypeWithSnakeCaseFields.put("category_id", 5);
        activityTypeWithSnakeCaseFields.put("description", "Meeting for coffee");
        activityTypeWithSnakeCaseFields.put("icon", "fa-coffee");
        activityTypeWithSnakeCaseFields.put("created_at", "2024-01-15T10:00:00Z");
        activityTypeWithSnakeCaseFields.put("updated_at", "2024-01-15T10:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(activityTypeWithSnakeCaseFields));

        when(monicaClient.get(eq("/activitytypes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = activityTypeService.listActivityTypes(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify field mapping from snake_case to camelCase
        assertEquals(5, data.get(0).get("categoryId"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("updatedAt"));
        // Name, description, and icon should pass through unchanged
        assertEquals("Coffee Meeting", data.get(0).get("name"));
        assertEquals("Meeting for coffee", data.get(0).get("description"));
        assertEquals("fa-coffee", data.get(0).get("icon"));
    }

    @Test
    void listActivityTypes_IntegerLimit_ConvertsToString() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 50);

        List<Map<String, Object>> activityTypes = List.of(
            activityTypeBuilder().id(1L).build()
        );
        Map<String, Object> listResponse = createListResponse(activityTypes);

        when(monicaClient.get(eq("/activitytypes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        activityTypeService.listActivityTypes(arguments).block();

        // Then
        verify(monicaClient).get(eq("/activitytypes"), argThat(params ->
            "50".equals(params.get("limit"))
        ));
    }

    @Test
    void listActivityTypes_IntegerPage_ConvertsToString() {
        // Given
        Map<String, Object> arguments = Map.of("page", 5);

        List<Map<String, Object>> activityTypes = List.of(
            activityTypeBuilder().id(1L).build()
        );
        Map<String, Object> listResponse = createListResponse(activityTypes, 5, 10, 50);

        when(monicaClient.get(eq("/activitytypes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        activityTypeService.listActivityTypes(arguments).block();

        // Then
        verify(monicaClient).get(eq("/activitytypes"), argThat(params ->
            "5".equals(params.get("page"))
        ));
    }
}
