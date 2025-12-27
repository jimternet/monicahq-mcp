package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.PhotoFieldMappingConfig;
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
 * Unit tests for PhotoService covering CRUD operations, validation,
 * photo upload handling, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class PhotoServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private PhotoFieldMappingConfig fieldMappingConfig;
    private PhotoService photoService;

    private Map<String, Object> mockPhotoData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        fieldMappingConfig = new PhotoFieldMappingConfig();
        photoService = new PhotoService(monicaClient, contentFormatter, fieldMappingConfig);

        mockPhotoData = photoBuilder()
            .id(1L)
            .contactId(100L)
            .filename("photo.jpg")
            .originalFilename("My Photo.jpg")
            .mimeType("image/jpeg")
            .filesize(2048L)
            .width(1920)
            .height(1080)
            .build();

        mockApiResponse = createSingleEntityResponse(mockPhotoData);
    }

    // ========================================================================================
    // CREATE PHOTO TESTS
    // ========================================================================================

    @Test
    void createPhoto_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "photo.jpg");

        when(monicaClient.post(eq("/photos"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted photo JSON");

        // When
        Map<String, Object> result = photoService.createPhoto(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted photo JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/photos"), argThat(data ->
            data.get("contact_id").equals(100L) &&
            "photo.jpg".equals(data.get("filename"))
        ));
    }

    @Test
    void createPhoto_MissingContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("filename", "photo.jpg");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            photoService.createPhoto(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createPhoto_NullContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", null);
        arguments.put("filename", "photo.jpg");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            photoService.createPhoto(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createPhoto_MissingFilename_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            photoService.createPhoto(arguments).block();
        });
        assertEquals("filename is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createPhoto_NullFilename_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            photoService.createPhoto(arguments).block();
        });
        assertEquals("filename is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createPhoto_EmptyFilename_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            photoService.createPhoto(arguments).block();
        });
        assertEquals("filename is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createPhoto_WhitespaceOnlyFilename_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "   ");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            photoService.createPhoto(arguments).block();
        });
        assertEquals("filename is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createPhoto_StringContactId_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", "100");
        arguments.put("filename", "photo.jpg");

        when(monicaClient.post(eq("/photos"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        photoService.createPhoto(arguments).block();

        // Then
        verify(monicaClient).post(eq("/photos"), argThat(data ->
            "100".equals(data.get("contact_id").toString())
        ));
    }

    @Test
    void createPhoto_IntegerContactId_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100);
        arguments.put("filename", "photo.jpg");

        when(monicaClient.post(eq("/photos"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        photoService.createPhoto(arguments).block();

        // Then
        verify(monicaClient).post(eq("/photos"), argThat(data ->
            Integer.valueOf(100).equals(data.get("contact_id"))
        ));
    }

    @Test
    void createPhoto_WithAllFields_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "photo.jpg");
        arguments.put("originalFilename", "My Photo.jpg");
        arguments.put("mimeType", "image/jpeg");
        arguments.put("filesize", 4096L);
        arguments.put("width", 1920);
        arguments.put("height", 1080);

        when(monicaClient.post(eq("/photos"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        photoService.createPhoto(arguments).block();

        // Then
        verify(monicaClient).post(eq("/photos"), argThat(data ->
            data.get("contact_id").equals(100L) &&
            "photo.jpg".equals(data.get("filename")) &&
            "My Photo.jpg".equals(data.get("original_filename")) &&
            "image/jpeg".equals(data.get("mime_type")) &&
            Long.valueOf(4096L).equals(data.get("filesize")) &&
            Integer.valueOf(1920).equals(data.get("width")) &&
            Integer.valueOf(1080).equals(data.get("height"))
        ));
    }

    @Test
    void createPhoto_MapsOriginalFilename_ToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "photo.jpg");
        arguments.put("originalFilename", "Original Name.jpg");

        when(monicaClient.post(eq("/photos"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        photoService.createPhoto(arguments).block();

        // Then
        verify(monicaClient).post(eq("/photos"), argThat(data ->
            "Original Name.jpg".equals(data.get("original_filename")) &&
            !data.containsKey("originalFilename")
        ));
    }

    @Test
    void createPhoto_MapsMimeType_ToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "photo.jpg");
        arguments.put("mimeType", "image/jpeg");

        when(monicaClient.post(eq("/photos"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        photoService.createPhoto(arguments).block();

        // Then
        verify(monicaClient).post(eq("/photos"), argThat(data ->
            "image/jpeg".equals(data.get("mime_type")) &&
            !data.containsKey("mimeType")
        ));
    }

    // ========================================================================================
    // GET PHOTO TESTS
    // ========================================================================================

    @Test
    void getPhoto_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/photos/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted photo JSON");

        // When
        Map<String, Object> result = photoService.getPhoto(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/photos/1"), any());
    }

    @Test
    void getPhoto_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            photoBuilder().id(42L).filename("test.jpg").build()
        );

        when(monicaClient.get(eq("/photos/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted photo JSON");

        // When
        Map<String, Object> result = photoService.getPhoto(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/photos/42"), any());
    }

    @Test
    void getPhoto_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            photoBuilder().id(123L).filename("test.jpg").build()
        );

        when(monicaClient.get(eq("/photos/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted photo JSON");

        // When
        Map<String, Object> result = photoService.getPhoto(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/photos/123"), any());
    }

    @Test
    void getPhoto_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("filename", "photo.jpg");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            photoService.getPhoto(arguments).block();
        });
        assertEquals("Photo ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getPhoto_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            photoService.getPhoto(arguments).block();
        });
        assertEquals("Photo ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getPhoto_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            photoService.getPhoto(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid photo ID format:"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getPhoto_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("filename", "photo.jpg");
        apiData.put("contact_id", 100L);
        apiData.put("original_filename", "My Photo.jpg");
        apiData.put("mime_type", "image/jpeg");
        apiData.put("filesize", 2048);
        apiData.put("width", 1920);
        apiData.put("height", 1080);
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T11:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/photos/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = photoService.getPhoto(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals(100L, data.get("contactId"));
        assertEquals("My Photo.jpg", data.get("originalFilename"));
        assertEquals("image/jpeg", data.get("mimeType"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T11:00:00Z", data.get("updatedAt"));
        // These fields should pass through unchanged
        assertEquals("photo.jpg", data.get("filename"));
        assertEquals(2048, data.get("filesize"));
        assertEquals(1920, data.get("width"));
        assertEquals(1080, data.get("height"));
    }

    @Test
    void getPhoto_DirectResponse_HandlesCorrectly() {
        // Given - response without data wrapper
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> directApiResponse = new HashMap<>();
        directApiResponse.put("id", 1L);
        directApiResponse.put("filename", "Direct Response Photo.jpg");
        directApiResponse.put("contact_id", 100L);

        when(monicaClient.get(eq("/photos/1"), any())).thenReturn(Mono.just(directApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = photoService.getPhoto(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertEquals("Direct Response Photo.jpg", data.get("filename"));
        assertEquals(100L, data.get("contactId"));
    }

    // ========================================================================================
    // UPDATE PHOTO TESTS
    // ========================================================================================

    @Test
    void updatePhoto_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("originalFilename", "Updated Photo.jpg");

        when(monicaClient.put(eq("/photos/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted photo JSON");

        // When
        Map<String, Object> result = photoService.updatePhoto(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/photos/1"), argThat(data ->
            "Updated Photo.jpg".equals(data.get("original_filename"))
        ));
    }

    @Test
    void updatePhoto_RemovesIdFromUpdateData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 5L);
        arguments.put("originalFilename", "Updated Photo.jpg");

        when(monicaClient.put(eq("/photos/5"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted photo JSON");

        // When
        photoService.updatePhoto(arguments).block();

        // Then - verify that id is NOT included in the request body
        verify(monicaClient).put(eq("/photos/5"), argThat(data ->
            !data.containsKey("id")
        ));
    }

    @Test
    void updatePhoto_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("originalFilename", "Updated Photo.jpg");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            photoService.updatePhoto(arguments).block();
        });
        assertEquals("Photo ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updatePhoto_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "42");
        arguments.put("originalFilename", "Updated Photo.jpg");

        when(monicaClient.put(eq("/photos/42"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted photo JSON");

        // When
        photoService.updatePhoto(arguments).block();

        // Then
        verify(monicaClient).put(eq("/photos/42"), any());
    }

    @Test
    void updatePhoto_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");
        arguments.put("originalFilename", "Updated Photo.jpg");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            photoService.updatePhoto(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid photo ID format:"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updatePhoto_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 99);
        arguments.put("originalFilename", "Updated Photo.jpg");

        when(monicaClient.put(eq("/photos/99"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted photo JSON");

        // When
        photoService.updatePhoto(arguments).block();

        // Then
        verify(monicaClient).put(eq("/photos/99"), any());
    }

    @Test
    void updatePhoto_WithMimeType_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("mimeType", "image/png");

        when(monicaClient.put(eq("/photos/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        photoService.updatePhoto(arguments).block();

        // Then
        verify(monicaClient).put(eq("/photos/1"), argThat(data ->
            "image/png".equals(data.get("mime_type")) &&
            !data.containsKey("mimeType")
        ));
    }

    @Test
    void updatePhoto_WithOriginalFilename_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("originalFilename", "New Original Name.jpg");

        when(monicaClient.put(eq("/photos/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        photoService.updatePhoto(arguments).block();

        // Then
        verify(monicaClient).put(eq("/photos/1"), argThat(data ->
            "New Original Name.jpg".equals(data.get("original_filename")) &&
            !data.containsKey("originalFilename")
        ));
    }

    @Test
    void updatePhoto_WithAllFields_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactId", 200L);
        arguments.put("filename", "updated.jpg");
        arguments.put("originalFilename", "Updated Photo.jpg");
        arguments.put("mimeType", "image/png");
        arguments.put("filesize", 8192L);
        arguments.put("width", 2560);
        arguments.put("height", 1440);

        when(monicaClient.put(eq("/photos/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        photoService.updatePhoto(arguments).block();

        // Then
        verify(monicaClient).put(eq("/photos/1"), argThat(data ->
            data.get("contact_id").equals(200L) &&
            "updated.jpg".equals(data.get("filename")) &&
            "Updated Photo.jpg".equals(data.get("original_filename")) &&
            "image/png".equals(data.get("mime_type")) &&
            Long.valueOf(8192L).equals(data.get("filesize")) &&
            Integer.valueOf(2560).equals(data.get("width")) &&
            Integer.valueOf(1440).equals(data.get("height")) &&
            !data.containsKey("id") &&
            !data.containsKey("contactId") &&
            !data.containsKey("originalFilename") &&
            !data.containsKey("mimeType")
        ));
    }

    // ========================================================================================
    // DELETE PHOTO TESTS
    // ========================================================================================

    @Test
    void deletePhoto_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/photos/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Photo"), eq(1L), eq(true), anyString()
        )).thenReturn("Photo deleted successfully");

        // When
        Map<String, Object> result = photoService.deletePhoto(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertTrue(content.get(0).get("text").toString().contains("deleted successfully"));

        verify(monicaClient).delete(eq("/photos/1"));
    }

    @Test
    void deletePhoto_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/photos/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Photo"), eq(99L), eq(true), anyString()
        )).thenReturn("Photo deleted successfully");

        // When
        Map<String, Object> result = photoService.deletePhoto(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/photos/99"));
    }

    @Test
    void deletePhoto_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 55);
        Map<String, Object> deleteResponse = createDeleteResponse(55L);

        when(monicaClient.delete(eq("/photos/55"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Photo"), eq(55L), eq(true), anyString()
        )).thenReturn("Photo deleted successfully");

        // When
        Map<String, Object> result = photoService.deletePhoto(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/photos/55"));
    }

    @Test
    void deletePhoto_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("filename", "photo.jpg");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            photoService.deletePhoto(arguments).block();
        });
        assertEquals("Photo ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deletePhoto_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            photoService.deletePhoto(arguments).block();
        });
        assertEquals("Photo ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deletePhoto_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            photoService.deletePhoto(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid photo ID format:"));
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST PHOTOS TESTS
    // ========================================================================================

    @Test
    void listPhotos_WithPagination_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> photos = List.of(
            photoBuilder().id(1L).filename("photo1.jpg").build(),
            photoBuilder().id(2L).filename("photo2.jpg").build()
        );
        Map<String, Object> listResponse = createListResponse(photos, 2, 20, 50);

        when(monicaClient.get(eq("/photos"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = photoService.listPhotos(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/photos"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listPhotos_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> photos = List.of(
            photoBuilder().id(1L).filename("photo1.jpg").build()
        );
        Map<String, Object> listResponse = createListResponse(photos);

        when(monicaClient.get(eq("/photos"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        photoService.listPhotos(arguments).block();

        // Then - verify default pagination values
        verify(monicaClient).get(eq("/photos"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listPhotos_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> photos = List.of(
            photoBuilder().id(1L).filename("photo1.jpg").build()
        );
        Map<String, Object> listResponse = createListResponse(photos, 1, 10, 100);

        when(monicaClient.get(eq("/photos"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = photoService.listPhotos(arguments).block();

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
    void listPhotos_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/photos"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = photoService.listPhotos(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listPhotos_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> photos = List.of(
            photoBuilder().id(1L).filename("photo1.jpg").build()
        );
        Map<String, Object> listResponse = createListResponse(photos);

        when(monicaClient.get(eq("/photos"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        photoService.listPhotos(arguments).block();

        // Then
        verify(monicaClient).get(eq("/photos"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listPhotos_StringPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("page", "3");

        List<Map<String, Object>> photos = List.of(
            photoBuilder().id(1L).filename("photo1.jpg").build()
        );
        Map<String, Object> listResponse = createListResponse(photos, 3, 10, 30);

        when(monicaClient.get(eq("/photos"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        photoService.listPhotos(arguments).block();

        // Then
        verify(monicaClient).get(eq("/photos"), argThat(params ->
            "3".equals(params.get("page"))
        ));
    }

    @Test
    void listPhotos_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> photoWithFields = new HashMap<>();
        photoWithFields.put("id", 1L);
        photoWithFields.put("filename", "photo.jpg");
        photoWithFields.put("contact_id", 100L);
        photoWithFields.put("original_filename", "Original Photo.jpg");
        photoWithFields.put("mime_type", "image/jpeg");
        photoWithFields.put("filesize", 2048);
        photoWithFields.put("width", 1920);
        photoWithFields.put("height", 1080);
        photoWithFields.put("created_at", "2024-01-15T10:00:00Z");
        photoWithFields.put("updated_at", "2024-01-15T11:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(photoWithFields));

        when(monicaClient.get(eq("/photos"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = photoService.listPhotos(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify field mapping
        assertEquals(100L, data.get(0).get("contactId"));
        assertEquals("Original Photo.jpg", data.get(0).get("originalFilename"));
        assertEquals("image/jpeg", data.get(0).get("mimeType"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T11:00:00Z", data.get(0).get("updatedAt"));
        assertEquals("photo.jpg", data.get(0).get("filename"));
        assertEquals(2048, data.get(0).get("filesize"));
        assertEquals(1920, data.get(0).get("width"));
        assertEquals(1080, data.get(0).get("height"));
    }

    @Test
    void listPhotos_NoMetaInResponse_HandlesGracefully() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> photos = List.of(
            photoBuilder().id(1L).filename("photo1.jpg").build()
        );
        Map<String, Object> responseWithoutMeta = new HashMap<>();
        responseWithoutMeta.put("data", photos);
        // No meta field

        when(monicaClient.get(eq("/photos"), any())).thenReturn(Mono.just(responseWithoutMeta));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = photoService.listPhotos(arguments).block();

        // Then
        assertNotNull(result);
        assertFalse(result.containsKey("meta"));
    }

    @Test
    void listPhotos_MultiplePhotos_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> photo1 = new HashMap<>();
        photo1.put("id", 1L);
        photo1.put("filename", "photo1.jpg");
        photo1.put("contact_id", 100L);
        photo1.put("mime_type", "image/jpeg");

        Map<String, Object> photo2 = new HashMap<>();
        photo2.put("id", 2L);
        photo2.put("filename", "photo2.png");
        photo2.put("contact_id", 101L);
        photo2.put("mime_type", "image/png");

        Map<String, Object> photo3 = new HashMap<>();
        photo3.put("id", 3L);
        photo3.put("filename", "photo3.gif");
        photo3.put("contact_id", 102L);
        photo3.put("mime_type", "image/gif");

        Map<String, Object> listResponse = createListResponse(List.of(photo1, photo2, photo3), 1, 10, 3);

        when(monicaClient.get(eq("/photos"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = photoService.listPhotos(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(3, data.size());

        assertEquals("photo1.jpg", data.get(0).get("filename"));
        assertEquals(100L, data.get(0).get("contactId"));
        assertEquals("image/jpeg", data.get(0).get("mimeType"));

        assertEquals("photo2.png", data.get(1).get("filename"));
        assertEquals(101L, data.get(1).get("contactId"));
        assertEquals("image/png", data.get(1).get("mimeType"));

        assertEquals("photo3.gif", data.get(2).get("filename"));
        assertEquals(102L, data.get(2).get("contactId"));
        assertEquals("image/gif", data.get(2).get("mimeType"));
    }

    // ========================================================================================
    // PHOTO UPLOAD HANDLING TESTS
    // ========================================================================================

    @Test
    void createPhoto_DifferentImageMimeTypes_MapCorrectly() {
        // Test various image MIME types
        String[] mimeTypes = {
            "image/jpeg",
            "image/png",
            "image/gif",
            "image/webp",
            "image/svg+xml",
            "image/bmp",
            "image/tiff"
        };

        for (String mimeType : mimeTypes) {
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("contactId", 100L);
            arguments.put("filename", "test.img");
            arguments.put("mimeType", mimeType);

            when(monicaClient.post(eq("/photos"), any())).thenReturn(Mono.just(mockApiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

            // When
            Map<String, Object> result = photoService.createPhoto(arguments).block();

            // Then
            assertNotNull(result);
            verify(monicaClient).post(eq("/photos"), argThat(data ->
                mimeType.equals(data.get("mime_type"))
            ));

            reset(monicaClient, contentFormatter);
        }
    }

    @Test
    void createPhoto_WithFilesize_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "large-photo.jpg");
        arguments.put("filesize", 10485760L); // 10 MB

        when(monicaClient.post(eq("/photos"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = photoService.createPhoto(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/photos"), argThat(data ->
            Long.valueOf(10485760L).equals(data.get("filesize"))
        ));
    }

    @Test
    void createPhoto_IntegerFilesize_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "small-photo.jpg");
        arguments.put("filesize", 1024); // Integer filesize

        when(monicaClient.post(eq("/photos"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = photoService.createPhoto(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/photos"), argThat(data ->
            Integer.valueOf(1024).equals(data.get("filesize"))
        ));
    }

    @Test
    void createPhoto_StringFilesize_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "photo.jpg");
        arguments.put("filesize", "2048");

        when(monicaClient.post(eq("/photos"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = photoService.createPhoto(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/photos"), argThat(data ->
            "2048".equals(data.get("filesize").toString())
        ));
    }

    @Test
    void createPhoto_WithDimensions_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "photo.jpg");
        arguments.put("width", 3840);
        arguments.put("height", 2160);

        when(monicaClient.post(eq("/photos"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = photoService.createPhoto(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/photos"), argThat(data ->
            Integer.valueOf(3840).equals(data.get("width")) &&
            Integer.valueOf(2160).equals(data.get("height"))
        ));
    }

    // ========================================================================================
    // EDGE CASE TESTS
    // ========================================================================================

    @Test
    void createPhoto_FilenameWithSpecialCharacters_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "My Photo (2024) - Final Version.jpg");

        when(monicaClient.post(eq("/photos"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = photoService.createPhoto(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/photos"), argThat(data ->
            "My Photo (2024) - Final Version.jpg".equals(data.get("filename"))
        ));
    }

    @Test
    void createPhoto_FilenameWithUnicode_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "Photo_\u4E2D\u6587_\u00E9\u00E8.jpg");

        when(monicaClient.post(eq("/photos"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = photoService.createPhoto(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/photos"), any());
    }

    @Test
    void createPhoto_LongFilename_Succeeds() {
        // Given
        String longFilename = "A".repeat(200) + ".jpg";
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", longFilename);

        when(monicaClient.post(eq("/photos"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = photoService.createPhoto(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/photos"), argThat(data ->
            longFilename.equals(data.get("filename"))
        ));
    }

    @Test
    void getPhoto_LargeId_Succeeds() {
        // Given
        Map<String, Object> arguments = Map.of("id", Long.MAX_VALUE);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            photoBuilder().id(Long.MAX_VALUE).build()
        );

        when(monicaClient.get(eq("/photos/" + Long.MAX_VALUE), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = photoService.getPhoto(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/photos/" + Long.MAX_VALUE), any());
    }

    @Test
    void createPhoto_ZeroFilesize_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "empty.jpg");
        arguments.put("filesize", 0);

        when(monicaClient.post(eq("/photos"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = photoService.createPhoto(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/photos"), argThat(data ->
            Integer.valueOf(0).equals(data.get("filesize"))
        ));
    }

    @Test
    void createPhoto_ZeroDimensions_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "placeholder.jpg");
        arguments.put("width", 0);
        arguments.put("height", 0);

        when(monicaClient.post(eq("/photos"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = photoService.createPhoto(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/photos"), argThat(data ->
            Integer.valueOf(0).equals(data.get("width")) &&
            Integer.valueOf(0).equals(data.get("height"))
        ));
    }

    @Test
    void createPhoto_LargeDimensions_Succeeds() {
        // Given - 8K resolution
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "8k-photo.jpg");
        arguments.put("width", 7680);
        arguments.put("height", 4320);

        when(monicaClient.post(eq("/photos"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = photoService.createPhoto(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/photos"), argThat(data ->
            Integer.valueOf(7680).equals(data.get("width")) &&
            Integer.valueOf(4320).equals(data.get("height"))
        ));
    }

    @Test
    void createPhoto_SquarePhoto_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "square-photo.jpg");
        arguments.put("width", 1000);
        arguments.put("height", 1000);

        when(monicaClient.post(eq("/photos"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = photoService.createPhoto(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/photos"), argThat(data ->
            Integer.valueOf(1000).equals(data.get("width")) &&
            Integer.valueOf(1000).equals(data.get("height"))
        ));
    }

    @Test
    void createPhoto_PortraitOrientation_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "portrait-photo.jpg");
        arguments.put("width", 1080);
        arguments.put("height", 1920);

        when(monicaClient.post(eq("/photos"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = photoService.createPhoto(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/photos"), argThat(data ->
            Integer.valueOf(1080).equals(data.get("width")) &&
            Integer.valueOf(1920).equals(data.get("height"))
        ));
    }
}
