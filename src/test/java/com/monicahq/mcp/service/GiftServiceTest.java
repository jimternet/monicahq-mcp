package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.GiftFieldMappingConfig;
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
 * Unit tests for GiftService covering CRUD operations,
 * status tracking, monetary value handling, field mapping, validation, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class GiftServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private GiftService giftService;

    private Map<String, Object> mockGiftData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        // Create GiftService with real GiftFieldMappingConfig (no dependencies to mock)
        GiftFieldMappingConfig fieldMappingConfig = new GiftFieldMappingConfig();
        giftService = new GiftService(monicaClient, contentFormatter, fieldMappingConfig);

        mockGiftData = giftBuilder()
            .id(1L)
            .name("Birthday Present")
            .contactId(42L)
            .status("idea")
            .value("50.00")
            .build();

        mockApiResponse = createSingleEntityResponse(mockGiftData);
    }

    // ========================================================================================
    // CREATE GIFT TESTS
    // ========================================================================================

    @Test
    void createGift_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("name", "Birthday Present");

        when(monicaClient.post(eq("/gifts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        Map<String, Object> result = giftService.createGift(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted gift JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/gifts"), argThat(data ->
            Long.valueOf(42L).equals(data.get("contact_id")) &&
            "Birthday Present".equals(data.get("name"))
        ));
    }

    @Test
    void createGift_MissingContactId_ThrowsException() {
        // Given - has other field but no contactId
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Test Gift");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            giftService.createGift(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createGift_NullContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", null);
        arguments.put("name", "Test Gift");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            giftService.createGift(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createGift_MissingName_ThrowsException() {
        // Given - has contactId but no name
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            giftService.createGift(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createGift_NullName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("name", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            giftService.createGift(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createGift_EmptyName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("name", "");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            giftService.createGift(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createGift_WhitespaceName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("name", "   ");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            giftService.createGift(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createGift_WithValue_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("name", "Expensive Watch");
        arguments.put("value", "199.99");

        when(monicaClient.post(eq("/gifts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.createGift(arguments).block();

        // Then - verify value is passed through
        verify(monicaClient).post(eq("/gifts"), argThat(data ->
            "199.99".equals(data.get("value"))
        ));
    }

    @Test
    void createGift_WithStatus_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("name", "Book");
        arguments.put("status", "offered");

        when(monicaClient.post(eq("/gifts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.createGift(arguments).block();

        // Then - verify status is passed through
        verify(monicaClient).post(eq("/gifts"), argThat(data ->
            "offered".equals(data.get("status"))
        ));
    }

    @Test
    void createGift_WithIsFor_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("name", "Anniversary Gift");
        arguments.put("isFor", "spouse");

        when(monicaClient.post(eq("/gifts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.createGift(arguments).block();

        // Then - verify isFor is mapped to is_for
        verify(monicaClient).post(eq("/gifts"), argThat(data ->
            "spouse".equals(data.get("is_for")) &&
            !data.containsKey("isFor")
        ));
    }

    @Test
    void createGift_WithComment_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("name", "Gift Card");
        arguments.put("comment", "For their birthday");

        when(monicaClient.post(eq("/gifts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.createGift(arguments).block();

        // Then - verify comment is passed through
        verify(monicaClient).post(eq("/gifts"), argThat(data ->
            "For their birthday".equals(data.get("comment"))
        ));
    }

    @Test
    void createGift_WithUrl_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("name", "Online Purchase");
        arguments.put("url", "https://example.com/product");

        when(monicaClient.post(eq("/gifts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.createGift(arguments).block();

        // Then - verify url is passed through
        verify(monicaClient).post(eq("/gifts"), argThat(data ->
            "https://example.com/product".equals(data.get("url"))
        ));
    }

    @Test
    void createGift_WithDate_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("name", "Holiday Gift");
        arguments.put("date", "2024-12-25");

        when(monicaClient.post(eq("/gifts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.createGift(arguments).block();

        // Then - verify date is passed through
        verify(monicaClient).post(eq("/gifts"), argThat(data ->
            "2024-12-25".equals(data.get("date"))
        ));
    }

    @Test
    void createGift_WithAllFields_MapsAllFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("name", "Complete Gift");
        arguments.put("comment", "A complete gift entry");
        arguments.put("url", "https://gift.example.com");
        arguments.put("value", "75.50");
        arguments.put("status", "received");
        arguments.put("date", "2024-01-15");
        arguments.put("isFor", "friend");

        when(monicaClient.post(eq("/gifts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.createGift(arguments).block();

        // Then
        verify(monicaClient).post(eq("/gifts"), argThat(data ->
            Long.valueOf(42L).equals(data.get("contact_id")) &&
            "Complete Gift".equals(data.get("name")) &&
            "A complete gift entry".equals(data.get("comment")) &&
            "https://gift.example.com".equals(data.get("url")) &&
            "75.50".equals(data.get("value")) &&
            "received".equals(data.get("status")) &&
            "2024-01-15".equals(data.get("date")) &&
            "friend".equals(data.get("is_for"))
        ));
    }

    @Test
    void createGift_StringContactId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", "99");
        arguments.put("name", "String ID Gift");

        when(monicaClient.post(eq("/gifts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.createGift(arguments).block();

        // Then
        verify(monicaClient).post(eq("/gifts"), argThat(data ->
            "99".equals(data.get("contact_id").toString())
        ));
    }

    @Test
    void createGift_IntegerContactId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 123);
        arguments.put("name", "Integer ID Gift");

        when(monicaClient.post(eq("/gifts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.createGift(arguments).block();

        // Then
        verify(monicaClient).post(eq("/gifts"), argThat(data ->
            Integer.valueOf(123).equals(data.get("contact_id"))
        ));
    }

    @Test
    void createGift_EmptyArguments_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            giftService.createGift(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createGift_NullArguments_ThrowsException() {
        // Given
        Map<String, Object> arguments = null;

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            giftService.createGift(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // GET GIFT TESTS
    // ========================================================================================

    @Test
    void getGift_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/gifts/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        Map<String, Object> result = giftService.getGift(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/gifts/1"), any());
    }

    @Test
    void getGift_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            giftBuilder().id(42L).name("Test Gift").build()
        );

        when(monicaClient.get(eq("/gifts/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        Map<String, Object> result = giftService.getGift(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/gifts/42"), any());
    }

    @Test
    void getGift_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            giftBuilder().id(123L).name("Test Gift").build()
        );

        when(monicaClient.get(eq("/gifts/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        Map<String, Object> result = giftService.getGift(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/gifts/123"), any());
    }

    @Test
    void getGift_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("name", "Test");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            giftService.getGift(arguments).block();
        });
        assertEquals("Gift ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getGift_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            giftService.getGift(arguments).block();
        });
        assertEquals("Gift ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getGift_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            giftService.getGift(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid gift ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getGift_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("name", "Test Gift");
        apiData.put("contact_id", 42L);
        apiData.put("value", "100.00");
        apiData.put("value_in_base_currency", "100.00");
        apiData.put("status", "offered");
        apiData.put("is_for", "spouse");
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T09:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/gifts/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = giftService.getGift(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals(42L, data.get("contactId"));
        assertEquals("100.00", data.get("valueInBaseCurrency"));
        assertEquals("spouse", data.get("isFor"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T09:00:00Z", data.get("updatedAt"));
        // These should remain unchanged
        assertEquals(1L, data.get("id"));
        assertEquals("Test Gift", data.get("name"));
        assertEquals("100.00", data.get("value"));
        assertEquals("offered", data.get("status"));
    }

    @Test
    void getGift_DirectResponseWithoutDataWrapper_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        // API response without "data" wrapper
        Map<String, Object> directResponse = new HashMap<>();
        directResponse.put("id", 1L);
        directResponse.put("name", "Direct Gift");
        directResponse.put("contact_id", 55L);
        directResponse.put("created_at", "2024-01-20T10:00:00Z");
        directResponse.put("updated_at", "2024-01-20T10:00:00Z");

        when(monicaClient.get(eq("/gifts/1"), any())).thenReturn(Mono.just(directResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = giftService.getGift(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        assertEquals("Direct Gift", data.get("name"));
        assertEquals(55L, data.get("contactId"));
    }

    // ========================================================================================
    // UPDATE GIFT TESTS
    // ========================================================================================

    @Test
    void updateGift_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("name", "Updated Gift Name");

        when(monicaClient.put(eq("/gifts/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        Map<String, Object> result = giftService.updateGift(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/gifts/1"), argThat(data ->
            "Updated Gift Name".equals(data.get("name"))
        ));
    }

    @Test
    void updateGift_RemovesIdFromUpdateData() {
        // Given - id should not be in the request body
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 5L);
        arguments.put("name", "Updated Gift");

        when(monicaClient.put(eq("/gifts/5"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.updateGift(arguments).block();

        // Then - verify that id is NOT included in the request body
        verify(monicaClient).put(eq("/gifts/5"), argThat(data ->
            !data.containsKey("id")
        ));
    }

    @Test
    void updateGift_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("name", "Updated Gift");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            giftService.updateGift(arguments).block();
        });
        assertEquals("Gift ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateGift_WithStatus_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("status", "received");

        when(monicaClient.put(eq("/gifts/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.updateGift(arguments).block();

        // Then - verify status is passed through
        verify(monicaClient).put(eq("/gifts/1"), argThat(data ->
            "received".equals(data.get("status"))
        ));
    }

    @Test
    void updateGift_WithValue_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("value", "299.99");

        when(monicaClient.put(eq("/gifts/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.updateGift(arguments).block();

        // Then - verify value is passed through
        verify(monicaClient).put(eq("/gifts/1"), argThat(data ->
            "299.99".equals(data.get("value"))
        ));
    }

    @Test
    void updateGift_WithIsFor_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("isFor", "partner");

        when(monicaClient.put(eq("/gifts/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.updateGift(arguments).block();

        // Then - verify isFor is mapped to is_for
        verify(monicaClient).put(eq("/gifts/1"), argThat(data ->
            "partner".equals(data.get("is_for")) &&
            !data.containsKey("isFor")
        ));
    }

    @Test
    void updateGift_WithContactId_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactId", 99L);

        when(monicaClient.put(eq("/gifts/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.updateGift(arguments).block();

        // Then - verify contactId is mapped to contact_id
        verify(monicaClient).put(eq("/gifts/1"), argThat(data ->
            Long.valueOf(99L).equals(data.get("contact_id")) &&
            !data.containsKey("contactId")
        ));
    }

    @Test
    void updateGift_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "42");
        arguments.put("name", "Updated Gift");

        when(monicaClient.put(eq("/gifts/42"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.updateGift(arguments).block();

        // Then
        verify(monicaClient).put(eq("/gifts/42"), any());
    }

    @Test
    void updateGift_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");
        arguments.put("name", "Updated Gift");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            giftService.updateGift(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid gift ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateGift_WithAllFields_MapsAllFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactId", 50L);
        arguments.put("name", "Complete Update");
        arguments.put("comment", "Updated comment");
        arguments.put("url", "https://updated.example.com");
        arguments.put("value", "150.00");
        arguments.put("status", "offered");
        arguments.put("date", "2024-06-15");
        arguments.put("isFor", "child");

        when(monicaClient.put(eq("/gifts/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.updateGift(arguments).block();

        // Then
        verify(monicaClient).put(eq("/gifts/1"), argThat(data ->
            Long.valueOf(50L).equals(data.get("contact_id")) &&
            "Complete Update".equals(data.get("name")) &&
            "Updated comment".equals(data.get("comment")) &&
            "https://updated.example.com".equals(data.get("url")) &&
            "150.00".equals(data.get("value")) &&
            "offered".equals(data.get("status")) &&
            "2024-06-15".equals(data.get("date")) &&
            "child".equals(data.get("is_for")) &&
            !data.containsKey("id")
        ));
    }

    // ========================================================================================
    // DELETE GIFT TESTS
    // ========================================================================================

    @Test
    void deleteGift_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/gifts/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Gift"), eq(1L), eq(true), anyString()
        )).thenReturn("Gift deleted successfully");

        // When
        Map<String, Object> result = giftService.deleteGift(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));

        verify(monicaClient).delete(eq("/gifts/1"));
    }

    @Test
    void deleteGift_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/gifts/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Gift"), eq(99L), eq(true), anyString()
        )).thenReturn("Gift deleted successfully");

        // When
        Map<String, Object> result = giftService.deleteGift(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/gifts/99"));
    }

    @Test
    void deleteGift_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 55);
        Map<String, Object> deleteResponse = createDeleteResponse(55L);

        when(monicaClient.delete(eq("/gifts/55"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Gift"), eq(55L), eq(true), anyString()
        )).thenReturn("Gift deleted successfully");

        // When
        Map<String, Object> result = giftService.deleteGift(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/gifts/55"));
    }

    @Test
    void deleteGift_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("name", "Test");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            giftService.deleteGift(arguments).block();
        });
        assertEquals("Gift ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteGift_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            giftService.deleteGift(arguments).block();
        });
        assertEquals("Gift ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteGift_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            giftService.deleteGift(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid gift ID format"));
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST GIFTS TESTS
    // ========================================================================================

    @Test
    void listGifts_WithPagination_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> gifts = List.of(
            giftBuilder().id(1L).name("Gift A").build(),
            giftBuilder().id(2L).name("Gift B").build()
        );
        Map<String, Object> listResponse = createListResponse(gifts, 2, 20, 50);

        when(monicaClient.get(eq("/gifts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = giftService.listGifts(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/gifts"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listGifts_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> gifts = List.of(
            giftBuilder().id(1L).name("Gift A").build()
        );
        Map<String, Object> listResponse = createListResponse(gifts);

        when(monicaClient.get(eq("/gifts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        giftService.listGifts(arguments).block();

        // Then - verify default pagination values
        verify(monicaClient).get(eq("/gifts"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listGifts_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> gifts = List.of(
            giftBuilder().id(1L).name("Gift 1").build()
        );
        Map<String, Object> listResponse = createListResponse(gifts, 1, 10, 100);

        when(monicaClient.get(eq("/gifts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = giftService.listGifts(arguments).block();

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
    void listGifts_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/gifts"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = giftService.listGifts(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listGifts_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> gifts = List.of(
            giftBuilder().id(1L).name("Gift 1").build()
        );
        Map<String, Object> listResponse = createListResponse(gifts);

        when(monicaClient.get(eq("/gifts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        giftService.listGifts(arguments).block();

        // Then
        verify(monicaClient).get(eq("/gifts"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listGifts_StringPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("page", "3");

        List<Map<String, Object>> gifts = List.of(
            giftBuilder().id(1L).name("Gift 1").build()
        );
        Map<String, Object> listResponse = createListResponse(gifts, 3, 10, 30);

        when(monicaClient.get(eq("/gifts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        giftService.listGifts(arguments).block();

        // Then
        verify(monicaClient).get(eq("/gifts"), argThat(params ->
            "3".equals(params.get("page"))
        ));
    }

    @Test
    void listGifts_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> giftWithSnakeCase = new HashMap<>();
        giftWithSnakeCase.put("id", 1L);
        giftWithSnakeCase.put("name", "Snake Gift");
        giftWithSnakeCase.put("contact_id", 42L);
        giftWithSnakeCase.put("value", "75.00");
        giftWithSnakeCase.put("value_in_base_currency", "75.00");
        giftWithSnakeCase.put("status", "idea");
        giftWithSnakeCase.put("is_for", "spouse");
        giftWithSnakeCase.put("created_at", "2024-01-15T10:00:00Z");
        giftWithSnakeCase.put("updated_at", "2024-01-15T10:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(giftWithSnakeCase));

        when(monicaClient.get(eq("/gifts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = giftService.listGifts(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify snake_case is mapped to camelCase
        assertEquals(42L, data.get(0).get("contactId"));
        assertEquals("75.00", data.get(0).get("valueInBaseCurrency"));
        assertEquals("spouse", data.get(0).get("isFor"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("updatedAt"));
        // These should remain unchanged
        assertEquals("Snake Gift", data.get(0).get("name"));
        assertEquals("75.00", data.get(0).get("value"));
        assertEquals("idea", data.get(0).get("status"));
    }

    @Test
    void listGifts_IntegerPageAndLimit_ConvertsToString() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 5,
            "limit", 50
        );

        List<Map<String, Object>> gifts = List.of(
            giftBuilder().id(1L).name("Gift 1").build()
        );
        Map<String, Object> listResponse = createListResponse(gifts, 5, 50, 100);

        when(monicaClient.get(eq("/gifts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        giftService.listGifts(arguments).block();

        // Then
        verify(monicaClient).get(eq("/gifts"), argThat(params ->
            "5".equals(params.get("page")) &&
            "50".equals(params.get("limit"))
        ));
    }

    @Test
    void listGifts_NoMetaInResponse_HandlesGracefully() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> gifts = List.of(
            giftBuilder().id(1L).name("Gift 1").build()
        );
        // Response without meta
        Map<String, Object> listResponse = new HashMap<>();
        listResponse.put("data", gifts);

        when(monicaClient.get(eq("/gifts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = giftService.listGifts(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertFalse(result.containsKey("meta"));
    }

    @Test
    void listGifts_MultipleGifts_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> gifts = List.of(
            giftBuilder().id(1L).name("Gift A").value("10.00").status("idea").build(),
            giftBuilder().id(2L).name("Gift B").value("20.00").status("offered").build(),
            giftBuilder().id(3L).name("Gift C").value("30.00").status("received").build()
        );
        Map<String, Object> listResponse = createListResponse(gifts, 1, 10, 3);

        when(monicaClient.get(eq("/gifts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = giftService.listGifts(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(3, data.size());

        assertEquals("Gift A", data.get(0).get("name"));
        assertEquals("idea", data.get(0).get("status"));
        assertEquals("Gift B", data.get(1).get("name"));
        assertEquals("offered", data.get(1).get("status"));
        assertEquals("Gift C", data.get(2).get("name"));
        assertEquals("received", data.get(2).get("status"));
    }

    // ========================================================================================
    // STATUS TRACKING TESTS
    // ========================================================================================

    @Test
    void createGift_WithStatusIdea_SetsCorrectStatus() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("name", "Idea Gift");
        arguments.put("status", "idea");

        when(monicaClient.post(eq("/gifts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.createGift(arguments).block();

        // Then
        verify(monicaClient).post(eq("/gifts"), argThat(data ->
            "idea".equals(data.get("status"))
        ));
    }

    @Test
    void createGift_WithStatusOffered_SetsCorrectStatus() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("name", "Offered Gift");
        arguments.put("status", "offered");

        when(monicaClient.post(eq("/gifts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.createGift(arguments).block();

        // Then
        verify(monicaClient).post(eq("/gifts"), argThat(data ->
            "offered".equals(data.get("status"))
        ));
    }

    @Test
    void createGift_WithStatusReceived_SetsCorrectStatus() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("name", "Received Gift");
        arguments.put("status", "received");

        when(monicaClient.post(eq("/gifts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.createGift(arguments).block();

        // Then
        verify(monicaClient).post(eq("/gifts"), argThat(data ->
            "received".equals(data.get("status"))
        ));
    }

    // ========================================================================================
    // MONETARY VALUE HANDLING TESTS
    // ========================================================================================

    @Test
    void createGift_WithDecimalValue_HandlesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("name", "Decimal Value Gift");
        arguments.put("value", "99.99");

        when(monicaClient.post(eq("/gifts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.createGift(arguments).block();

        // Then
        verify(monicaClient).post(eq("/gifts"), argThat(data ->
            "99.99".equals(data.get("value"))
        ));
    }

    @Test
    void createGift_WithIntegerValue_HandlesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("name", "Integer Value Gift");
        arguments.put("value", 100);

        when(monicaClient.post(eq("/gifts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.createGift(arguments).block();

        // Then
        verify(monicaClient).post(eq("/gifts"), argThat(data ->
            Integer.valueOf(100).equals(data.get("value"))
        ));
    }

    @Test
    void createGift_WithZeroValue_HandlesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("name", "Zero Value Gift");
        arguments.put("value", "0.00");

        when(monicaClient.post(eq("/gifts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted gift JSON");

        // When
        giftService.createGift(arguments).block();

        // Then
        verify(monicaClient).post(eq("/gifts"), argThat(data ->
            "0.00".equals(data.get("value"))
        ));
    }

    @Test
    void getGift_WithValueInBaseCurrency_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("name", "Currency Gift");
        apiData.put("value", "100.00");
        apiData.put("value_in_base_currency", "85.50");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/gifts/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = giftService.getGift(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        assertEquals("100.00", data.get("value"));
        assertEquals("85.50", data.get("valueInBaseCurrency"));
    }
}
