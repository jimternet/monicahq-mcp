package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.ActivityTypeCategoryFieldMappingConfig;
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
 * Unit tests for ActivityTypeCategoryService covering activity type category CRUD operations.
 */
@ExtendWith(MockitoExtension.class)
class ActivityTypeCategoryServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private ActivityTypeCategoryService activityTypeCategoryService;

    private Map<String, Object> mockCategoryData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        ActivityTypeCategoryFieldMappingConfig fieldMappingConfig = new ActivityTypeCategoryFieldMappingConfig();
        activityTypeCategoryService = new ActivityTypeCategoryService(monicaClient, contentFormatter, fieldMappingConfig);

        mockCategoryData = activityTypeCategoryBuilder()
            .id(1L)
            .name("Social Activities")
            .parentId(null)
            .build();

        mockApiResponse = createSingleEntityResponse(mockCategoryData);
    }

    // ========================================================================================
    // CREATE ACTIVITY TYPE CATEGORY TESTS
    // ========================================================================================

    @Test
    void createActivityTypeCategory_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Social Activities");
        arguments.put("description", "Activities with friends and family");

        when(monicaClient.post(eq("/activitytypecategories"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted category JSON");

        // When
        Map<String, Object> result = activityTypeCategoryService.createActivityTypeCategory(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted category JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/activitytypecategories"), argThat(data ->
            "Social Activities".equals(data.get("name")) &&
            "Activities with friends and family".equals(data.get("description"))
        ));
    }

    @Test
    void createActivityTypeCategory_MissingName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("description", "Test description");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeCategoryService.createActivityTypeCategory(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createActivityTypeCategory_NullName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", null);
        arguments.put("description", "Test description");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeCategoryService.createActivityTypeCategory(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createActivityTypeCategory_EmptyName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "");
        arguments.put("description", "Test description");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeCategoryService.createActivityTypeCategory(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createActivityTypeCategory_WhitespaceName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "   ");
        arguments.put("description", "Test description");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeCategoryService.createActivityTypeCategory(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createActivityTypeCategory_MapsParentIdField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Sub Category");
        arguments.put("parentId", 10L);

        when(monicaClient.post(eq("/activitytypecategories"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted category JSON");

        // When
        activityTypeCategoryService.createActivityTypeCategory(arguments).block();

        // Then - verify parentId is mapped to parent_id
        verify(monicaClient).post(eq("/activitytypecategories"), argThat(data ->
            Long.valueOf(10L).equals(data.get("parent_id")) &&
            !data.containsKey("parentId")
        ));
    }

    @Test
    void createActivityTypeCategory_MapsSortOrderField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Social Activities");
        arguments.put("sortOrder", 5);

        when(monicaClient.post(eq("/activitytypecategories"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted category JSON");

        // When
        activityTypeCategoryService.createActivityTypeCategory(arguments).block();

        // Then - verify sortOrder is mapped to sort_order
        verify(monicaClient).post(eq("/activitytypecategories"), argThat(data ->
            Integer.valueOf(5).equals(data.get("sort_order")) &&
            !data.containsKey("sortOrder")
        ));
    }

    @Test
    void createActivityTypeCategory_WithDescription_PassesThroughUnchanged() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Social Activities");
        arguments.put("description", "Activities with friends and family");

        when(monicaClient.post(eq("/activitytypecategories"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted category JSON");

        // When
        activityTypeCategoryService.createActivityTypeCategory(arguments).block();

        // Then - verify description passes through unchanged
        verify(monicaClient).post(eq("/activitytypecategories"), argThat(data ->
            "Activities with friends and family".equals(data.get("description"))
        ));
    }

    @Test
    void createActivityTypeCategory_WithAllFields_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Social Activities");
        arguments.put("parentId", 5L);
        arguments.put("description", "Activities with friends");
        arguments.put("sortOrder", 10);

        when(monicaClient.post(eq("/activitytypecategories"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted category JSON");

        // When
        activityTypeCategoryService.createActivityTypeCategory(arguments).block();

        // Then - verify all fields are mapped correctly
        verify(monicaClient).post(eq("/activitytypecategories"), argThat(data ->
            "Social Activities".equals(data.get("name")) &&
            Long.valueOf(5L).equals(data.get("parent_id")) &&
            "Activities with friends".equals(data.get("description")) &&
            Integer.valueOf(10).equals(data.get("sort_order"))
        ));
    }

    // ========================================================================================
    // GET ACTIVITY TYPE CATEGORY TESTS
    // ========================================================================================

    @Test
    void getActivityTypeCategory_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/activitytypecategories/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted category JSON");

        // When
        Map<String, Object> result = activityTypeCategoryService.getActivityTypeCategory(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/activitytypecategories/1"), any());
    }

    @Test
    void getActivityTypeCategory_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            activityTypeCategoryBuilder().id(42L).build()
        );

        when(monicaClient.get(eq("/activitytypecategories/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted category JSON");

        // When
        Map<String, Object> result = activityTypeCategoryService.getActivityTypeCategory(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/activitytypecategories/42"), any());
    }

    @Test
    void getActivityTypeCategory_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            activityTypeCategoryBuilder().id(123L).build()
        );

        when(monicaClient.get(eq("/activitytypecategories/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted category JSON");

        // When
        Map<String, Object> result = activityTypeCategoryService.getActivityTypeCategory(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/activitytypecategories/123"), any());
    }

    @Test
    void getActivityTypeCategory_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("name", "Social Activities");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeCategoryService.getActivityTypeCategory(arguments).block();
        });
        assertEquals("Activity Type Category ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getActivityTypeCategory_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeCategoryService.getActivityTypeCategory(arguments).block();
        });
        assertEquals("Activity Type Category ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getActivityTypeCategory_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeCategoryService.getActivityTypeCategory(arguments).block();
        });
        assertTrue(exception.getMessage().startsWith("Invalid activity type category ID format:"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getActivityTypeCategory_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("name", "Social Activities");
        apiData.put("parent_id", 5L);
        apiData.put("sort_order", 10);
        apiData.put("description", "Activities with friends");
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T09:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/activitytypecategories/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = activityTypeCategoryService.getActivityTypeCategory(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals(5L, data.get("parentId"));
        assertEquals(10, data.get("sortOrder"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T09:00:00Z", data.get("updatedAt"));
        // Name and description should pass through unchanged
        assertEquals("Social Activities", data.get("name"));
        assertEquals("Activities with friends", data.get("description"));
    }

    @Test
    void getActivityTypeCategory_WithoutDataWrapper_StillWorks() {
        // Given - response without "data" wrapper (edge case)
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> directResponse = new HashMap<>();
        directResponse.put("id", 1L);
        directResponse.put("name", "Social Activities");
        directResponse.put("parent_id", 5L);
        directResponse.put("sort_order", 10);

        when(monicaClient.get(eq("/activitytypecategories/1"), any())).thenReturn(Mono.just(directResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = activityTypeCategoryService.getActivityTypeCategory(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertEquals(5L, data.get("parentId"));
        assertEquals(10, data.get("sortOrder"));
    }

    // ========================================================================================
    // UPDATE ACTIVITY TYPE CATEGORY TESTS
    // ========================================================================================

    @Test
    void updateActivityTypeCategory_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("name", "Updated Social Activities");
        arguments.put("sortOrder", 15);

        when(monicaClient.put(eq("/activitytypecategories/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted category JSON");

        // When
        Map<String, Object> result = activityTypeCategoryService.updateActivityTypeCategory(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/activitytypecategories/1"), argThat(data ->
            "Updated Social Activities".equals(data.get("name")) &&
            Integer.valueOf(15).equals(data.get("sort_order"))
        ));
    }

    @Test
    void updateActivityTypeCategory_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Updated Social Activities");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeCategoryService.updateActivityTypeCategory(arguments).block();
        });
        assertEquals("Activity Type Category ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateActivityTypeCategory_MissingName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("sortOrder", 10);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeCategoryService.updateActivityTypeCategory(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateActivityTypeCategory_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "42");
        arguments.put("name", "Updated Category");

        when(monicaClient.put(eq("/activitytypecategories/42"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted category JSON");

        // When
        activityTypeCategoryService.updateActivityTypeCategory(arguments).block();

        // Then
        verify(monicaClient).put(eq("/activitytypecategories/42"), any());
    }

    @Test
    void updateActivityTypeCategory_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");
        arguments.put("name", "Updated Category");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeCategoryService.updateActivityTypeCategory(arguments).block();
        });
        assertTrue(exception.getMessage().startsWith("Invalid activity type category ID format:"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateActivityTypeCategory_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("name", "Work Activities");
        arguments.put("parentId", 8L);
        arguments.put("description", "Updated description");
        arguments.put("sortOrder", 20);

        when(monicaClient.put(eq("/activitytypecategories/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted category JSON");

        // When
        activityTypeCategoryService.updateActivityTypeCategory(arguments).block();

        // Then - verify all fields are mapped correctly
        verify(monicaClient).put(eq("/activitytypecategories/1"), argThat(data ->
            "Work Activities".equals(data.get("name")) &&
            Long.valueOf(8L).equals(data.get("parent_id")) &&
            !data.containsKey("parentId") &&
            "Updated description".equals(data.get("description")) &&
            Integer.valueOf(20).equals(data.get("sort_order")) &&
            !data.containsKey("sortOrder")
        ));
    }

    // ========================================================================================
    // DELETE ACTIVITY TYPE CATEGORY TESTS
    // ========================================================================================

    @Test
    void deleteActivityTypeCategory_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/activitytypecategories/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Activity Type Category"), eq(1L), eq(true), anyString()
        )).thenReturn("Activity Type Category with ID 1 has been deleted successfully");

        // When
        Map<String, Object> result = activityTypeCategoryService.deleteActivityTypeCategory(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertTrue(content.get(0).get("text").toString().contains("deleted successfully"));

        verify(monicaClient).delete(eq("/activitytypecategories/1"));
    }

    @Test
    void deleteActivityTypeCategory_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/activitytypecategories/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Activity Type Category"), eq(99L), eq(true), anyString()
        )).thenReturn("Activity Type Category with ID 99 has been deleted successfully");

        // When
        Map<String, Object> result = activityTypeCategoryService.deleteActivityTypeCategory(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/activitytypecategories/99"));
    }

    @Test
    void deleteActivityTypeCategory_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 55);
        Map<String, Object> deleteResponse = createDeleteResponse(55L);

        when(monicaClient.delete(eq("/activitytypecategories/55"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Activity Type Category"), eq(55L), eq(true), anyString()
        )).thenReturn("Activity Type Category with ID 55 has been deleted successfully");

        // When
        Map<String, Object> result = activityTypeCategoryService.deleteActivityTypeCategory(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/activitytypecategories/55"));
    }

    @Test
    void deleteActivityTypeCategory_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("name", "Social Activities");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeCategoryService.deleteActivityTypeCategory(arguments).block();
        });
        assertEquals("Activity Type Category ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteActivityTypeCategory_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeCategoryService.deleteActivityTypeCategory(arguments).block();
        });
        assertEquals("Activity Type Category ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteActivityTypeCategory_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityTypeCategoryService.deleteActivityTypeCategory(arguments).block();
        });
        assertTrue(exception.getMessage().startsWith("Invalid activity type category ID format:"));
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST ACTIVITY TYPE CATEGORIES TESTS
    // ========================================================================================

    @Test
    void listActivityTypeCategories_WithPagination_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> categories = List.of(
            activityTypeCategoryBuilder().id(1L).name("Social Activities").build(),
            activityTypeCategoryBuilder().id(2L).name("Work Activities").build()
        );
        Map<String, Object> listResponse = createListResponse(categories, 2, 20, 50);

        when(monicaClient.get(eq("/activitytypecategories"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = activityTypeCategoryService.listActivityTypeCategories(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/activitytypecategories"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listActivityTypeCategories_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> categories = List.of(
            activityTypeCategoryBuilder().id(1L).build()
        );
        Map<String, Object> listResponse = createListResponse(categories);

        when(monicaClient.get(eq("/activitytypecategories"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        activityTypeCategoryService.listActivityTypeCategories(arguments).block();

        // Then - verify default pagination values
        verify(monicaClient).get(eq("/activitytypecategories"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listActivityTypeCategories_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> categories = List.of(
            activityTypeCategoryBuilder().id(1L).build()
        );
        Map<String, Object> listResponse = createListResponse(categories, 1, 10, 100);

        when(monicaClient.get(eq("/activitytypecategories"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = activityTypeCategoryService.listActivityTypeCategories(arguments).block();

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
    void listActivityTypeCategories_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/activitytypecategories"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = activityTypeCategoryService.listActivityTypeCategories(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listActivityTypeCategories_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> categories = List.of(
            activityTypeCategoryBuilder().id(1L).build()
        );
        Map<String, Object> listResponse = createListResponse(categories);

        when(monicaClient.get(eq("/activitytypecategories"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        activityTypeCategoryService.listActivityTypeCategories(arguments).block();

        // Then
        verify(monicaClient).get(eq("/activitytypecategories"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listActivityTypeCategories_StringPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("page", "3");

        List<Map<String, Object>> categories = List.of(
            activityTypeCategoryBuilder().id(1L).build()
        );
        Map<String, Object> listResponse = createListResponse(categories, 3, 10, 30);

        when(monicaClient.get(eq("/activitytypecategories"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        activityTypeCategoryService.listActivityTypeCategories(arguments).block();

        // Then
        verify(monicaClient).get(eq("/activitytypecategories"), argThat(params ->
            "3".equals(params.get("page"))
        ));
    }

    @Test
    void listActivityTypeCategories_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> categoryWithSnakeCaseFields = new HashMap<>();
        categoryWithSnakeCaseFields.put("id", 1L);
        categoryWithSnakeCaseFields.put("name", "Social Activities");
        categoryWithSnakeCaseFields.put("parent_id", 5L);
        categoryWithSnakeCaseFields.put("sort_order", 10);
        categoryWithSnakeCaseFields.put("description", "Activities with friends");
        categoryWithSnakeCaseFields.put("created_at", "2024-01-15T10:00:00Z");
        categoryWithSnakeCaseFields.put("updated_at", "2024-01-15T10:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(categoryWithSnakeCaseFields));

        when(monicaClient.get(eq("/activitytypecategories"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = activityTypeCategoryService.listActivityTypeCategories(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify field mapping from snake_case to camelCase
        assertEquals(5L, data.get(0).get("parentId"));
        assertEquals(10, data.get(0).get("sortOrder"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("updatedAt"));
        // Name and description should pass through unchanged
        assertEquals("Social Activities", data.get(0).get("name"));
        assertEquals("Activities with friends", data.get(0).get("description"));
    }

    @Test
    void listActivityTypeCategories_IntegerLimit_ConvertsToString() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 50);

        List<Map<String, Object>> categories = List.of(
            activityTypeCategoryBuilder().id(1L).build()
        );
        Map<String, Object> listResponse = createListResponse(categories);

        when(monicaClient.get(eq("/activitytypecategories"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        activityTypeCategoryService.listActivityTypeCategories(arguments).block();

        // Then
        verify(monicaClient).get(eq("/activitytypecategories"), argThat(params ->
            "50".equals(params.get("limit"))
        ));
    }

    @Test
    void listActivityTypeCategories_IntegerPage_ConvertsToString() {
        // Given
        Map<String, Object> arguments = Map.of("page", 5);

        List<Map<String, Object>> categories = List.of(
            activityTypeCategoryBuilder().id(1L).build()
        );
        Map<String, Object> listResponse = createListResponse(categories, 5, 10, 50);

        when(monicaClient.get(eq("/activitytypecategories"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        activityTypeCategoryService.listActivityTypeCategories(arguments).block();

        // Then
        verify(monicaClient).get(eq("/activitytypecategories"), argThat(params ->
            "5".equals(params.get("page"))
        ));
    }
}
