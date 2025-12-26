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
 * Unit tests for DocumentService covering CRUD operations, validation,
 * document upload handling, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class DocumentServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    @InjectMocks
    private DocumentService documentService;

    private Map<String, Object> mockDocumentData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        mockDocumentData = documentBuilder()
            .id(1L)
            .contactId(100L)
            .filename("document.pdf")
            .originalFilename("My Document.pdf")
            .mimeType("application/pdf")
            .size(1024L)
            .build();

        mockApiResponse = createSingleEntityResponse(mockDocumentData);
    }

    // ========================================================================================
    // CREATE DOCUMENT TESTS
    // ========================================================================================

    @Test
    void createDocument_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "document.pdf");

        when(monicaClient.post(eq("/documents"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted document JSON");

        // When
        Map<String, Object> result = documentService.createDocument(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted document JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/documents"), argThat(data ->
            data.get("contact_id").equals(100L) &&
            "document.pdf".equals(data.get("filename"))
        ));
    }

    @Test
    void createDocument_MissingContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("filename", "document.pdf");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            documentService.createDocument(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createDocument_NullContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", null);
        arguments.put("filename", "document.pdf");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            documentService.createDocument(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createDocument_MissingFilename_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            documentService.createDocument(arguments).block();
        });
        assertEquals("filename is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createDocument_NullFilename_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            documentService.createDocument(arguments).block();
        });
        assertEquals("filename is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createDocument_EmptyFilename_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            documentService.createDocument(arguments).block();
        });
        assertEquals("filename is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createDocument_WhitespaceOnlyFilename_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "   ");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            documentService.createDocument(arguments).block();
        });
        assertEquals("filename is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createDocument_StringContactId_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", "100");
        arguments.put("filename", "document.pdf");

        when(monicaClient.post(eq("/documents"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        documentService.createDocument(arguments).block();

        // Then
        verify(monicaClient).post(eq("/documents"), argThat(data ->
            "100".equals(data.get("contact_id").toString())
        ));
    }

    @Test
    void createDocument_IntegerContactId_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100);
        arguments.put("filename", "document.pdf");

        when(monicaClient.post(eq("/documents"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        documentService.createDocument(arguments).block();

        // Then
        verify(monicaClient).post(eq("/documents"), argThat(data ->
            Integer.valueOf(100).equals(data.get("contact_id"))
        ));
    }

    @Test
    void createDocument_WithAllFields_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "document.pdf");
        arguments.put("originalFilename", "My Document.pdf");
        arguments.put("mimeType", "application/pdf");
        arguments.put("size", 2048L);
        arguments.put("description", "Important document");

        when(monicaClient.post(eq("/documents"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        documentService.createDocument(arguments).block();

        // Then
        verify(monicaClient).post(eq("/documents"), argThat(data ->
            data.get("contact_id").equals(100L) &&
            "document.pdf".equals(data.get("filename")) &&
            "My Document.pdf".equals(data.get("original_filename")) &&
            "application/pdf".equals(data.get("mime_type")) &&
            Long.valueOf(2048L).equals(data.get("size")) &&
            "Important document".equals(data.get("description"))
        ));
    }

    @Test
    void createDocument_MapsOriginalFilename_ToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "doc.pdf");
        arguments.put("originalFilename", "Original Name.pdf");

        when(monicaClient.post(eq("/documents"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        documentService.createDocument(arguments).block();

        // Then
        verify(monicaClient).post(eq("/documents"), argThat(data ->
            "Original Name.pdf".equals(data.get("original_filename")) &&
            !data.containsKey("originalFilename")
        ));
    }

    @Test
    void createDocument_MapsMimeType_ToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "doc.pdf");
        arguments.put("mimeType", "application/pdf");

        when(monicaClient.post(eq("/documents"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        documentService.createDocument(arguments).block();

        // Then
        verify(monicaClient).post(eq("/documents"), argThat(data ->
            "application/pdf".equals(data.get("mime_type")) &&
            !data.containsKey("mimeType")
        ));
    }

    // ========================================================================================
    // GET DOCUMENT TESTS
    // ========================================================================================

    @Test
    void getDocument_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/documents/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted document JSON");

        // When
        Map<String, Object> result = documentService.getDocument(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/documents/1"), any());
    }

    @Test
    void getDocument_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            documentBuilder().id(42L).filename("test.pdf").build()
        );

        when(monicaClient.get(eq("/documents/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted document JSON");

        // When
        Map<String, Object> result = documentService.getDocument(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/documents/42"), any());
    }

    @Test
    void getDocument_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            documentBuilder().id(123L).filename("test.pdf").build()
        );

        when(monicaClient.get(eq("/documents/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted document JSON");

        // When
        Map<String, Object> result = documentService.getDocument(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/documents/123"), any());
    }

    @Test
    void getDocument_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("filename", "doc.pdf");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            documentService.getDocument(arguments).block();
        });
        assertEquals("id is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getDocument_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            documentService.getDocument(arguments).block();
        });
        assertEquals("id is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getDocument_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            documentService.getDocument(arguments).block();
        });
        assertEquals("id must be a valid number", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getDocument_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("filename", "doc.pdf");
        apiData.put("contact_id", 100L);
        apiData.put("original_filename", "My Document.pdf");
        apiData.put("mime_type", "application/pdf");
        apiData.put("download_url", "https://example.com/download/doc.pdf");
        apiData.put("size", 2048);
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T11:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/documents/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = documentService.getDocument(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals(100L, data.get("contactId"));
        assertEquals("My Document.pdf", data.get("originalFilename"));
        assertEquals("application/pdf", data.get("mimeType"));
        assertEquals("https://example.com/download/doc.pdf", data.get("downloadUrl"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T11:00:00Z", data.get("updatedAt"));
        // These fields should pass through unchanged
        assertEquals("doc.pdf", data.get("filename"));
        assertEquals(2048, data.get("size"));
    }

    @Test
    void getDocument_DirectResponse_HandlesCorrectly() {
        // Given - response without data wrapper
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> directApiResponse = new HashMap<>();
        directApiResponse.put("id", 1L);
        directApiResponse.put("filename", "Direct Response Doc.pdf");
        directApiResponse.put("contact_id", 100L);

        when(monicaClient.get(eq("/documents/1"), any())).thenReturn(Mono.just(directApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = documentService.getDocument(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertEquals("Direct Response Doc.pdf", data.get("filename"));
        assertEquals(100L, data.get("contactId"));
    }

    // ========================================================================================
    // UPDATE DOCUMENT TESTS
    // ========================================================================================

    @Test
    void updateDocument_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("description", "Updated description");

        when(monicaClient.put(eq("/documents/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted document JSON");

        // When
        Map<String, Object> result = documentService.updateDocument(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/documents/1"), argThat(data ->
            "Updated description".equals(data.get("description"))
        ));
    }

    @Test
    void updateDocument_RemovesIdFromUpdateData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 5L);
        arguments.put("description", "Updated description");

        when(monicaClient.put(eq("/documents/5"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted document JSON");

        // When
        documentService.updateDocument(arguments).block();

        // Then - verify that id is NOT included in the request body
        verify(monicaClient).put(eq("/documents/5"), argThat(data ->
            !data.containsKey("id")
        ));
    }

    @Test
    void updateDocument_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("description", "Updated description");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            documentService.updateDocument(arguments).block();
        });
        assertEquals("id is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateDocument_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "42");
        arguments.put("description", "Updated description");

        when(monicaClient.put(eq("/documents/42"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted document JSON");

        // When
        documentService.updateDocument(arguments).block();

        // Then
        verify(monicaClient).put(eq("/documents/42"), any());
    }

    @Test
    void updateDocument_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");
        arguments.put("description", "Updated description");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            documentService.updateDocument(arguments).block();
        });
        assertEquals("id must be a valid number", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateDocument_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 99);
        arguments.put("description", "Updated description");

        when(monicaClient.put(eq("/documents/99"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted document JSON");

        // When
        documentService.updateDocument(arguments).block();

        // Then
        verify(monicaClient).put(eq("/documents/99"), any());
    }

    @Test
    void updateDocument_WithMimeType_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("mimeType", "text/plain");

        when(monicaClient.put(eq("/documents/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        documentService.updateDocument(arguments).block();

        // Then
        verify(monicaClient).put(eq("/documents/1"), argThat(data ->
            "text/plain".equals(data.get("mime_type")) &&
            !data.containsKey("mimeType")
        ));
    }

    @Test
    void updateDocument_WithOriginalFilename_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("originalFilename", "New Original Name.pdf");

        when(monicaClient.put(eq("/documents/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        documentService.updateDocument(arguments).block();

        // Then
        verify(monicaClient).put(eq("/documents/1"), argThat(data ->
            "New Original Name.pdf".equals(data.get("original_filename")) &&
            !data.containsKey("originalFilename")
        ));
    }

    @Test
    void updateDocument_WithAllFields_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactId", 200L);
        arguments.put("filename", "updated.pdf");
        arguments.put("originalFilename", "Updated Document.pdf");
        arguments.put("mimeType", "application/pdf");
        arguments.put("size", 4096L);
        arguments.put("description", "Updated description");

        when(monicaClient.put(eq("/documents/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        documentService.updateDocument(arguments).block();

        // Then
        verify(monicaClient).put(eq("/documents/1"), argThat(data ->
            data.get("contact_id").equals(200L) &&
            "updated.pdf".equals(data.get("filename")) &&
            "Updated Document.pdf".equals(data.get("original_filename")) &&
            "application/pdf".equals(data.get("mime_type")) &&
            Long.valueOf(4096L).equals(data.get("size")) &&
            "Updated description".equals(data.get("description")) &&
            !data.containsKey("id") &&
            !data.containsKey("contactId") &&
            !data.containsKey("originalFilename") &&
            !data.containsKey("mimeType")
        ));
    }

    // ========================================================================================
    // DELETE DOCUMENT TESTS
    // ========================================================================================

    @Test
    void deleteDocument_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/documents/1"))).thenReturn(Mono.just(deleteResponse));

        // When
        Map<String, Object> result = documentService.deleteDocument(arguments).block();

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

        verify(monicaClient).delete(eq("/documents/1"));
    }

    @Test
    void deleteDocument_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/documents/99"))).thenReturn(Mono.just(deleteResponse));

        // When
        Map<String, Object> result = documentService.deleteDocument(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/documents/99"));
    }

    @Test
    void deleteDocument_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 55);
        Map<String, Object> deleteResponse = createDeleteResponse(55L);

        when(monicaClient.delete(eq("/documents/55"))).thenReturn(Mono.just(deleteResponse));

        // When
        Map<String, Object> result = documentService.deleteDocument(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/documents/55"));
    }

    @Test
    void deleteDocument_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("filename", "doc.pdf");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            documentService.deleteDocument(arguments).block();
        });
        assertEquals("id is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteDocument_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            documentService.deleteDocument(arguments).block();
        });
        assertEquals("id is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteDocument_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            documentService.deleteDocument(arguments).block();
        });
        assertEquals("id must be a valid number", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST DOCUMENTS TESTS
    // ========================================================================================

    @Test
    void listDocuments_WithPagination_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> documents = List.of(
            documentBuilder().id(1L).filename("doc1.pdf").build(),
            documentBuilder().id(2L).filename("doc2.pdf").build()
        );
        Map<String, Object> listResponse = createListResponse(documents, 2, 20, 50);

        when(monicaClient.get(eq("/documents"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = documentService.listDocuments(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/documents"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listDocuments_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> documents = List.of(
            documentBuilder().id(1L).filename("doc1.pdf").build()
        );
        Map<String, Object> listResponse = createListResponse(documents);

        when(monicaClient.get(eq("/documents"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        documentService.listDocuments(arguments).block();

        // Then - verify default pagination values
        verify(monicaClient).get(eq("/documents"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listDocuments_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> documents = List.of(
            documentBuilder().id(1L).filename("doc1.pdf").build()
        );
        Map<String, Object> listResponse = createListResponse(documents, 1, 10, 100);

        when(monicaClient.get(eq("/documents"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = documentService.listDocuments(arguments).block();

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
    void listDocuments_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/documents"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = documentService.listDocuments(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listDocuments_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> documents = List.of(
            documentBuilder().id(1L).filename("doc1.pdf").build()
        );
        Map<String, Object> listResponse = createListResponse(documents);

        when(monicaClient.get(eq("/documents"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        documentService.listDocuments(arguments).block();

        // Then
        verify(monicaClient).get(eq("/documents"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listDocuments_StringPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("page", "3");

        List<Map<String, Object>> documents = List.of(
            documentBuilder().id(1L).filename("doc1.pdf").build()
        );
        Map<String, Object> listResponse = createListResponse(documents, 3, 10, 30);

        when(monicaClient.get(eq("/documents"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        documentService.listDocuments(arguments).block();

        // Then
        verify(monicaClient).get(eq("/documents"), argThat(params ->
            "3".equals(params.get("page"))
        ));
    }

    @Test
    void listDocuments_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> docWithFields = new HashMap<>();
        docWithFields.put("id", 1L);
        docWithFields.put("filename", "doc.pdf");
        docWithFields.put("contact_id", 100L);
        docWithFields.put("original_filename", "Original Doc.pdf");
        docWithFields.put("mime_type", "application/pdf");
        docWithFields.put("download_url", "https://example.com/download/doc.pdf");
        docWithFields.put("size", 2048);
        docWithFields.put("created_at", "2024-01-15T10:00:00Z");
        docWithFields.put("updated_at", "2024-01-15T11:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(docWithFields));

        when(monicaClient.get(eq("/documents"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = documentService.listDocuments(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify field mapping
        assertEquals(100L, data.get(0).get("contactId"));
        assertEquals("Original Doc.pdf", data.get(0).get("originalFilename"));
        assertEquals("application/pdf", data.get(0).get("mimeType"));
        assertEquals("https://example.com/download/doc.pdf", data.get(0).get("downloadUrl"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T11:00:00Z", data.get(0).get("updatedAt"));
        assertEquals("doc.pdf", data.get(0).get("filename"));
        assertEquals(2048, data.get(0).get("size"));
    }

    @Test
    void listDocuments_NoMetaInResponse_HandlesGracefully() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> documents = List.of(
            documentBuilder().id(1L).filename("doc1.pdf").build()
        );
        Map<String, Object> responseWithoutMeta = new HashMap<>();
        responseWithoutMeta.put("data", documents);
        // No meta field

        when(monicaClient.get(eq("/documents"), any())).thenReturn(Mono.just(responseWithoutMeta));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = documentService.listDocuments(arguments).block();

        // Then
        assertNotNull(result);
        assertFalse(result.containsKey("meta"));
    }

    @Test
    void listDocuments_MultipleDocuments_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> doc1 = new HashMap<>();
        doc1.put("id", 1L);
        doc1.put("filename", "doc1.pdf");
        doc1.put("contact_id", 100L);
        doc1.put("mime_type", "application/pdf");

        Map<String, Object> doc2 = new HashMap<>();
        doc2.put("id", 2L);
        doc2.put("filename", "doc2.docx");
        doc2.put("contact_id", 101L);
        doc2.put("mime_type", "application/vnd.openxmlformats-officedocument.wordprocessingml.document");

        Map<String, Object> doc3 = new HashMap<>();
        doc3.put("id", 3L);
        doc3.put("filename", "image.png");
        doc3.put("contact_id", 102L);
        doc3.put("mime_type", "image/png");

        Map<String, Object> listResponse = createListResponse(List.of(doc1, doc2, doc3), 1, 10, 3);

        when(monicaClient.get(eq("/documents"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = documentService.listDocuments(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(3, data.size());

        assertEquals("doc1.pdf", data.get(0).get("filename"));
        assertEquals(100L, data.get(0).get("contactId"));
        assertEquals("application/pdf", data.get(0).get("mimeType"));

        assertEquals("doc2.docx", data.get(1).get("filename"));
        assertEquals(101L, data.get(1).get("contactId"));
        assertEquals("application/vnd.openxmlformats-officedocument.wordprocessingml.document", data.get(1).get("mimeType"));

        assertEquals("image.png", data.get(2).get("filename"));
        assertEquals(102L, data.get(2).get("contactId"));
        assertEquals("image/png", data.get(2).get("mimeType"));
    }

    // ========================================================================================
    // DOCUMENT UPLOAD HANDLING TESTS
    // ========================================================================================

    @Test
    void createDocument_DifferentMimeTypes_MapCorrectly() {
        // Test various MIME types
        String[] mimeTypes = {
            "application/pdf",
            "application/msword",
            "text/plain",
            "image/jpeg",
            "image/png",
            "application/zip",
            "application/json"
        };

        for (String mimeType : mimeTypes) {
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("contactId", 100L);
            arguments.put("filename", "test.file");
            arguments.put("mimeType", mimeType);

            when(monicaClient.post(eq("/documents"), any())).thenReturn(Mono.just(mockApiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

            // When
            Map<String, Object> result = documentService.createDocument(arguments).block();

            // Then
            assertNotNull(result);
            verify(monicaClient).post(eq("/documents"), argThat(data ->
                mimeType.equals(data.get("mime_type"))
            ));

            reset(monicaClient, contentFormatter);
        }
    }

    @Test
    void createDocument_WithSize_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "large-file.pdf");
        arguments.put("size", 10485760L); // 10 MB

        when(monicaClient.post(eq("/documents"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = documentService.createDocument(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/documents"), argThat(data ->
            Long.valueOf(10485760L).equals(data.get("size"))
        ));
    }

    @Test
    void createDocument_IntegerSize_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "small-file.pdf");
        arguments.put("size", 1024); // Integer size

        when(monicaClient.post(eq("/documents"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = documentService.createDocument(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/documents"), argThat(data ->
            Integer.valueOf(1024).equals(data.get("size"))
        ));
    }

    @Test
    void createDocument_StringSize_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "file.pdf");
        arguments.put("size", "2048");

        when(monicaClient.post(eq("/documents"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = documentService.createDocument(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/documents"), argThat(data ->
            "2048".equals(data.get("size").toString())
        ));
    }

    // ========================================================================================
    // EDGE CASE TESTS
    // ========================================================================================

    @Test
    void createDocument_FilenameWithSpecialCharacters_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "My Document (2024) - Final Version.pdf");

        when(monicaClient.post(eq("/documents"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = documentService.createDocument(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/documents"), argThat(data ->
            "My Document (2024) - Final Version.pdf".equals(data.get("filename"))
        ));
    }

    @Test
    void createDocument_FilenameWithUnicode_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "Document_\u4E2D\u6587_\u00E9\u00E8.pdf");

        when(monicaClient.post(eq("/documents"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = documentService.createDocument(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/documents"), any());
    }

    @Test
    void createDocument_LongFilename_Succeeds() {
        // Given
        String longFilename = "A".repeat(200) + ".pdf";
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", longFilename);

        when(monicaClient.post(eq("/documents"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = documentService.createDocument(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/documents"), argThat(data ->
            longFilename.equals(data.get("filename"))
        ));
    }

    @Test
    void getDocument_LargeId_Succeeds() {
        // Given
        Map<String, Object> arguments = Map.of("id", Long.MAX_VALUE);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            documentBuilder().id(Long.MAX_VALUE).build()
        );

        when(monicaClient.get(eq("/documents/" + Long.MAX_VALUE), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = documentService.getDocument(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/documents/" + Long.MAX_VALUE), any());
    }

    @Test
    void createDocument_EmptyDescription_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "doc.pdf");
        arguments.put("description", "");

        when(monicaClient.post(eq("/documents"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = documentService.createDocument(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/documents"), argThat(data ->
            "".equals(data.get("description"))
        ));
    }

    @Test
    void createDocument_LongDescription_Succeeds() {
        // Given
        String longDescription = "D".repeat(1000);
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "doc.pdf");
        arguments.put("description", longDescription);

        when(monicaClient.post(eq("/documents"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = documentService.createDocument(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/documents"), argThat(data ->
            longDescription.equals(data.get("description"))
        ));
    }

    @Test
    void createDocument_ZeroSize_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("filename", "empty.txt");
        arguments.put("size", 0);

        when(monicaClient.post(eq("/documents"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = documentService.createDocument(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/documents"), argThat(data ->
            Integer.valueOf(0).equals(data.get("size"))
        ));
    }

    // ========================================================================================
    // DOWNLOAD URL HANDLING TESTS
    // ========================================================================================

    @Test
    void getDocument_WithDownloadUrl_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("filename", "doc.pdf");
        apiData.put("download_url", "https://storage.example.com/documents/abc123/doc.pdf?token=xyz");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/documents/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = documentService.getDocument(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertEquals("https://storage.example.com/documents/abc123/doc.pdf?token=xyz", data.get("downloadUrl"));
    }

    @Test
    void listDocuments_WithDownloadUrls_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> doc1 = new HashMap<>();
        doc1.put("id", 1L);
        doc1.put("filename", "doc1.pdf");
        doc1.put("download_url", "https://example.com/download/1");

        Map<String, Object> doc2 = new HashMap<>();
        doc2.put("id", 2L);
        doc2.put("filename", "doc2.pdf");
        doc2.put("download_url", "https://example.com/download/2");

        Map<String, Object> listResponse = createListResponse(List.of(doc1, doc2));

        when(monicaClient.get(eq("/documents"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = documentService.listDocuments(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());
        assertEquals("https://example.com/download/1", data.get(0).get("downloadUrl"));
        assertEquals("https://example.com/download/2", data.get(1).get("downloadUrl"));
    }

    // ========================================================================================
    // HELPER METHODS
    // ========================================================================================

    /**
     * Builder for creating mock document data.
     */
    protected DocumentDataBuilder documentBuilder() {
        return new DocumentDataBuilder();
    }

    /**
     * Builder for creating mock document data.
     */
    public static class DocumentDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public DocumentDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("filename", "document.pdf");
            data.put("original_filename", "My Document.pdf");
            data.put("mime_type", "application/pdf");
            data.put("size", 1024);
            data.put("created_at", java.time.LocalDateTime.now().format(DATETIME_FORMATTER));
            data.put("updated_at", java.time.LocalDateTime.now().format(DATETIME_FORMATTER));
        }

        public DocumentDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public DocumentDataBuilder contactId(Long contactId) {
            data.put("contact_id", contactId);
            data.put("contact", Map.of("id", contactId));
            return this;
        }

        public DocumentDataBuilder filename(String filename) {
            data.put("filename", filename);
            return this;
        }

        public DocumentDataBuilder originalFilename(String originalFilename) {
            data.put("original_filename", originalFilename);
            return this;
        }

        public DocumentDataBuilder mimeType(String mimeType) {
            data.put("mime_type", mimeType);
            return this;
        }

        public DocumentDataBuilder size(Long size) {
            data.put("size", size);
            return this;
        }

        public DocumentDataBuilder downloadUrl(String downloadUrl) {
            data.put("download_url", downloadUrl);
            return this;
        }

        public DocumentDataBuilder description(String description) {
            data.put("description", description);
            return this;
        }

        public DocumentDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }
}
