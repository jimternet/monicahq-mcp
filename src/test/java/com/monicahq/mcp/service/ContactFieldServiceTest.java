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
 * Unit tests for ContactFieldService covering custom field CRUD operations,
 * validation, ID parsing, field mapping, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class ContactFieldServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    @InjectMocks
    private ContactFieldService contactFieldService;

    private Map<String, Object> mockContactFieldData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        mockContactFieldData = contactFieldBuilder()
            .id(1L)
            .contactId(100L)
            .fieldData("john@example.com")
            .contactFieldTypeId(1)
            .build();

        mockApiResponse = createSingleEntityResponse(mockContactFieldData);
    }

    // ========================================================================================
    // CREATE CONTACT FIELD TESTS
    // ========================================================================================

    @Test
    void createContactField_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("contactFieldTypeId", 1);
        arguments.put("data", "john@example.com");

        when(monicaClient.post(eq("/contactfields"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted contact field JSON");

        // When
        Map<String, Object> result = contactFieldService.createContactField(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted contact field JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/contactfields"), argThat(data ->
            data.get("contact_id").equals(100L) &&
            data.get("contact_field_type_id").equals(1) &&
            "john@example.com".equals(data.get("data"))
        ));
    }

    @Test
    void createContactField_MissingContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactFieldTypeId", 1);
        arguments.put("data", "john@example.com");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.createContactField(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createContactField_NullContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", null);
        arguments.put("contactFieldTypeId", 1);
        arguments.put("data", "john@example.com");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.createContactField(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createContactField_MissingContactFieldTypeId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("data", "john@example.com");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.createContactField(arguments).block();
        });
        assertEquals("contactFieldTypeId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createContactField_NullContactFieldTypeId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("contactFieldTypeId", null);
        arguments.put("data", "john@example.com");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.createContactField(arguments).block();
        });
        assertEquals("contactFieldTypeId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createContactField_MissingData_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("contactFieldTypeId", 1);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.createContactField(arguments).block();
        });
        assertEquals("data is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createContactField_NullData_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("contactFieldTypeId", 1);
        arguments.put("data", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.createContactField(arguments).block();
        });
        assertEquals("data is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createContactField_EmptyData_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("contactFieldTypeId", 1);
        arguments.put("data", "");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.createContactField(arguments).block();
        });
        assertEquals("data is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createContactField_WhitespaceOnlyData_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("contactFieldTypeId", 1);
        arguments.put("data", "   ");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.createContactField(arguments).block();
        });
        assertEquals("data is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createContactField_EmptyArgs_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.createContactField(arguments).block();
        });
        assertEquals("Contact field creation arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createContactField_NullArgs_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.createContactField(null).block();
        });
        assertEquals("Contact field creation arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createContactField_StringContactId_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", "100");
        arguments.put("contactFieldTypeId", 1);
        arguments.put("data", "john@example.com");

        when(monicaClient.post(eq("/contactfields"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        contactFieldService.createContactField(arguments).block();

        // Then
        verify(monicaClient).post(eq("/contactfields"), argThat(data ->
            "100".equals(data.get("contact_id").toString())
        ));
    }

    @Test
    void createContactField_IntegerContactId_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100);
        arguments.put("contactFieldTypeId", 1);
        arguments.put("data", "john@example.com");

        when(monicaClient.post(eq("/contactfields"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        contactFieldService.createContactField(arguments).block();

        // Then
        verify(monicaClient).post(eq("/contactfields"), any());
    }

    @Test
    void createContactField_FieldMapping_ContactIdMapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("contactFieldTypeId", 2);
        arguments.put("data", "+1-555-1234");

        when(monicaClient.post(eq("/contactfields"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        contactFieldService.createContactField(arguments).block();

        // Then
        verify(monicaClient).post(eq("/contactfields"), argThat(data ->
            data.containsKey("contact_id") &&
            data.containsKey("contact_field_type_id") &&
            !data.containsKey("contactId") &&
            !data.containsKey("contactFieldTypeId")
        ));
    }

    @Test
    void createContactField_PhoneNumber_SuccessfullyCreated() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("contactFieldTypeId", 2); // Phone type
        arguments.put("data", "+1 (555) 123-4567");

        Map<String, Object> phoneFieldData = contactFieldBuilder()
            .id(2L)
            .contactId(100L)
            .fieldData("+1 (555) 123-4567")
            .contactFieldTypeId(2)
            .build();
        Map<String, Object> phoneResponse = createSingleEntityResponse(phoneFieldData);

        when(monicaClient.post(eq("/contactfields"), any())).thenReturn(Mono.just(phoneResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Phone field created");

        // When
        Map<String, Object> result = contactFieldService.createContactField(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
    }

    @Test
    void createContactField_WithLabels_SuccessfullyCreated() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("contactFieldTypeId", 1);
        arguments.put("data", "john@example.com");
        arguments.put("labels", List.of("work", "primary"));

        when(monicaClient.post(eq("/contactfields"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        contactFieldService.createContactField(arguments).block();

        // Then
        verify(monicaClient).post(eq("/contactfields"), argThat(data ->
            data.get("labels") instanceof List &&
            ((List<?>) data.get("labels")).contains("work")
        ));
    }

    @Test
    void createContactField_ResponseMapsFromSnakeCaseToCamelCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("contactFieldTypeId", 1);
        arguments.put("data", "john@example.com");

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("contact_field_type_id", 1);
        apiData.put("data", "john@example.com");
        apiData.put("created_at", "2025-01-01T00:00:00Z");
        apiData.put("updated_at", "2025-01-01T00:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.post(eq("/contactfields"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = contactFieldService.createContactField(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertTrue(data.containsKey("contactFieldTypeId"));
        assertTrue(data.containsKey("createdAt"));
        assertTrue(data.containsKey("updatedAt"));
        assertFalse(data.containsKey("contact_field_type_id"));
        assertFalse(data.containsKey("created_at"));
        assertFalse(data.containsKey("updated_at"));
    }

    // ========================================================================================
    // GET CONTACT FIELD TESTS
    // ========================================================================================

    @Test
    void getContactField_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);

        when(monicaClient.get(eq("/contactfields/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted contact field JSON");

        // When
        Map<String, Object> result = contactFieldService.getContactField(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
    }

    @Test
    void getContactField_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 5);

        when(monicaClient.get(eq("/contactfields/5"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        contactFieldService.getContactField(arguments).block();

        // Then
        verify(monicaClient).get(eq("/contactfields/5"), isNull());
    }

    @Test
    void getContactField_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "99");

        when(monicaClient.get(eq("/contactfields/99"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        contactFieldService.getContactField(arguments).block();

        // Then
        verify(monicaClient).get(eq("/contactfields/99"), isNull());
    }

    @Test
    void getContactField_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.getContactField(arguments).block();
        });
        assertEquals("Contact field ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getContactField_NullArgs_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.getContactField(null).block();
        });
        assertEquals("Contact field ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getContactField_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.getContactField(arguments).block();
        });
        assertEquals("Invalid contact field ID format: invalid", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getContactField_ResponseFieldMapping_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("data", "test@example.com");
        apiData.put("contact_field_type_id", 1);
        apiData.put("created_at", "2025-01-01T10:00:00Z");
        apiData.put("updated_at", "2025-01-01T11:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/contactfields/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = contactFieldService.getContactField(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertEquals("test@example.com", data.get("data"));
        assertEquals(1, data.get("contactFieldTypeId"));
        assertEquals("2025-01-01T10:00:00Z", data.get("createdAt"));
        assertEquals("2025-01-01T11:00:00Z", data.get("updatedAt"));
    }

    @Test
    void getContactField_DirectResponseWithoutDataWrapper_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);

        Map<String, Object> directResponse = new HashMap<>();
        directResponse.put("id", 1L);
        directResponse.put("data", "test@example.com");
        directResponse.put("contact_field_type_id", 1);
        directResponse.put("created_at", "2025-01-01T10:00:00Z");
        directResponse.put("updated_at", "2025-01-01T11:00:00Z");

        when(monicaClient.get(eq("/contactfields/1"), any())).thenReturn(Mono.just(directResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = contactFieldService.getContactField(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertEquals(1L, data.get("id"));
        assertEquals("test@example.com", data.get("data"));
    }

    // ========================================================================================
    // UPDATE CONTACT FIELD TESTS
    // ========================================================================================

    @Test
    void updateContactField_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("data", "newemail@example.com");

        when(monicaClient.put(eq("/contactfields/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted contact field JSON");

        // When
        Map<String, Object> result = contactFieldService.updateContactField(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
    }

    @Test
    void updateContactField_RemovesIdFromBody() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("data", "newemail@example.com");

        when(monicaClient.put(eq("/contactfields/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        contactFieldService.updateContactField(arguments).block();

        // Then
        verify(monicaClient).put(eq("/contactfields/1"), argThat(data ->
            !data.containsKey("id") &&
            "newemail@example.com".equals(data.get("data"))
        ));
    }

    @Test
    void updateContactField_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("data", "newemail@example.com");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.updateContactField(arguments).block();
        });
        assertEquals("Contact field ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateContactField_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "55");
        arguments.put("data", "newemail@example.com");

        when(monicaClient.put(eq("/contactfields/55"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        contactFieldService.updateContactField(arguments).block();

        // Then
        verify(monicaClient).put(eq("/contactfields/55"), any());
    }

    @Test
    void updateContactField_InvalidId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "abc");
        arguments.put("data", "newemail@example.com");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.updateContactField(arguments).block();
        });
        assertEquals("Invalid contact field ID format: abc", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateContactField_FieldMapping_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactFieldTypeId", 2);
        arguments.put("data", "+1-555-9876");

        when(monicaClient.put(eq("/contactfields/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        contactFieldService.updateContactField(arguments).block();

        // Then
        verify(monicaClient).put(eq("/contactfields/1"), argThat(data ->
            data.containsKey("contact_field_type_id") &&
            !data.containsKey("contactFieldTypeId") &&
            "+1-555-9876".equals(data.get("data"))
        ));
    }

    @Test
    void updateContactField_WithContactId_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactId", 200L);
        arguments.put("data", "newemail@example.com");

        when(monicaClient.put(eq("/contactfields/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        contactFieldService.updateContactField(arguments).block();

        // Then
        verify(monicaClient).put(eq("/contactfields/1"), argThat(data ->
            data.containsKey("contact_id") &&
            !data.containsKey("contactId")
        ));
    }

    // ========================================================================================
    // DELETE CONTACT FIELD TESTS
    // ========================================================================================

    @Test
    void deleteContactField_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);

        Map<String, Object> deleteResponse = createDeleteResponse(1L);
        when(monicaClient.delete(eq("/contactfields/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Contact Field"), eq(1L), eq(true), anyString()
        )).thenReturn("Contact field deleted successfully");

        // When
        Map<String, Object> result = contactFieldService.deleteContactField(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Contact field deleted successfully", content.get(0).get("text"));
    }

    @Test
    void deleteContactField_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 99);

        Map<String, Object> deleteResponse = createDeleteResponse(99L);
        when(monicaClient.delete(eq("/contactfields/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Contact Field"), eq(99L), eq(true), anyString()
        )).thenReturn("Contact field deleted successfully");

        // When
        contactFieldService.deleteContactField(arguments).block();

        // Then
        verify(monicaClient).delete(eq("/contactfields/99"));
    }

    @Test
    void deleteContactField_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "55");

        Map<String, Object> deleteResponse = createDeleteResponse(55L);
        when(monicaClient.delete(eq("/contactfields/55"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Contact Field"), eq(55L), eq(true), anyString()
        )).thenReturn("Contact field deleted successfully");

        // When
        contactFieldService.deleteContactField(arguments).block();

        // Then
        verify(monicaClient).delete(eq("/contactfields/55"));
    }

    @Test
    void deleteContactField_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.deleteContactField(arguments).block();
        });
        assertEquals("Contact field ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteContactField_NullArgs_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.deleteContactField(null).block();
        });
        assertEquals("Contact field ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteContactField_InvalidId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.deleteContactField(arguments).block();
        });
        assertEquals("Invalid contact field ID format: invalid", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST CONTACT FIELDS TESTS
    // ========================================================================================

    @Test
    void listContactFields_ValidContactId_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);

        List<Map<String, Object>> fields = List.of(
            contactFieldBuilder().id(1L).fieldData("email@example.com").contactFieldTypeId(1).build(),
            contactFieldBuilder().id(2L).fieldData("+1-555-1234").contactFieldTypeId(2).build()
        );
        Map<String, Object> listResponse = createListResponse(fields, 1, 10, 2);

        when(monicaClient.get(eq("/contacts/100/contactfields"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = contactFieldService.listContactFields(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());
    }

    @Test
    void listContactFields_MissingContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.listContactFields(arguments).block();
        });
        assertTrue(exception.getMessage().contains("contactId is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void listContactFields_StringContactId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", "200");

        Map<String, Object> listResponse = createListResponse(List.of());

        when(monicaClient.get(eq("/contacts/200/contactfields"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        contactFieldService.listContactFields(arguments).block();

        // Then
        verify(monicaClient).get(eq("/contacts/200/contactfields"), any());
    }

    @Test
    void listContactFields_InvalidContactIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", "abc");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            contactFieldService.listContactFields(arguments).block();
        });
        assertEquals("Invalid contact ID format: abc", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void listContactFields_WithPagination_UsesCorrectParams() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("page", 2);
        arguments.put("limit", 25);

        Map<String, Object> listResponse = createListResponse(List.of(), 2, 25, 50);

        when(monicaClient.get(eq("/contacts/100/contactfields"), argThat(params ->
            "2".equals(params.get("page")) &&
            "25".equals(params.get("limit"))
        ))).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        contactFieldService.listContactFields(arguments).block();

        // Then
        verify(monicaClient).get(eq("/contacts/100/contactfields"), argThat(params ->
            "2".equals(params.get("page")) &&
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listContactFields_DefaultPagination_UsesDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);

        Map<String, Object> listResponse = createListResponse(List.of());

        when(monicaClient.get(eq("/contacts/100/contactfields"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ))).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        contactFieldService.listContactFields(arguments).block();

        // Then
        verify(monicaClient).get(eq("/contacts/100/contactfields"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listContactFields_LimitClampsToMax100() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("limit", 200);

        Map<String, Object> listResponse = createListResponse(List.of());

        when(monicaClient.get(eq("/contacts/100/contactfields"), argThat(params ->
            "100".equals(params.get("limit"))
        ))).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        contactFieldService.listContactFields(arguments).block();

        // Then
        verify(monicaClient).get(eq("/contacts/100/contactfields"), argThat(params ->
            "100".equals(params.get("limit"))
        ));
    }

    @Test
    void listContactFields_LimitClampsToMin1() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("limit", 0);

        Map<String, Object> listResponse = createListResponse(List.of());

        when(monicaClient.get(eq("/contacts/100/contactfields"), argThat(params ->
            "1".equals(params.get("limit"))
        ))).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        contactFieldService.listContactFields(arguments).block();

        // Then
        verify(monicaClient).get(eq("/contacts/100/contactfields"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listContactFields_NegativeLimit_ClampsToMin() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("limit", -5);

        Map<String, Object> listResponse = createListResponse(List.of());

        when(monicaClient.get(eq("/contacts/100/contactfields"), argThat(params ->
            "1".equals(params.get("limit"))
        ))).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        contactFieldService.listContactFields(arguments).block();

        // Then
        verify(monicaClient).get(eq("/contacts/100/contactfields"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listContactFields_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);

        Map<String, Object> listResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/contacts/100/contactfields"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Empty list JSON");

        // When
        Map<String, Object> result = contactFieldService.listContactFields(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listContactFields_WithMeta_ReturnsMeta() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);

        List<Map<String, Object>> fields = List.of(
            contactFieldBuilder().id(1L).build()
        );
        Map<String, Object> listResponse = createListResponse(fields, 1, 10, 5);

        when(monicaClient.get(eq("/contacts/100/contactfields"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = contactFieldService.listContactFields(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) result.get("meta");
        assertEquals(1, meta.get("current_page"));
        assertEquals(10, meta.get("per_page"));
        assertEquals(5, meta.get("total"));
    }

    @Test
    void listContactFields_FieldMapping_MapsFromSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);

        Map<String, Object> apiField = new HashMap<>();
        apiField.put("id", 1L);
        apiField.put("data", "test@example.com");
        apiField.put("contact_field_type_id", 1);
        apiField.put("created_at", "2025-01-01T00:00:00Z");
        apiField.put("updated_at", "2025-01-01T00:00:00Z");

        List<Map<String, Object>> fields = List.of(apiField);
        Map<String, Object> listResponse = createListResponse(fields);

        when(monicaClient.get(eq("/contacts/100/contactfields"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = contactFieldService.listContactFields(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        Map<String, Object> field = data.get(0);
        assertEquals(1, field.get("contactFieldTypeId"));
        assertEquals("2025-01-01T00:00:00Z", field.get("createdAt"));
        assertEquals("2025-01-01T00:00:00Z", field.get("updatedAt"));
        assertFalse(field.containsKey("contact_field_type_id"));
        assertFalse(field.containsKey("created_at"));
        assertFalse(field.containsKey("updated_at"));
    }

    @Test
    void listContactFields_StringLimitAndPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("page", "3");
        arguments.put("limit", "15");

        Map<String, Object> listResponse = createListResponse(List.of());

        when(monicaClient.get(eq("/contacts/100/contactfields"), argThat(params ->
            "3".equals(params.get("page")) &&
            "15".equals(params.get("limit"))
        ))).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        contactFieldService.listContactFields(arguments).block();

        // Then
        verify(monicaClient).get(eq("/contacts/100/contactfields"), argThat(params ->
            "3".equals(params.get("page")) &&
            "15".equals(params.get("limit"))
        ));
    }

    @Test
    void listContactFields_NoMetaInResponse_HandlesGracefully() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);

        Map<String, Object> listResponse = new HashMap<>();
        listResponse.put("data", List.of());
        // No meta field

        when(monicaClient.get(eq("/contacts/100/contactfields"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = contactFieldService.listContactFields(arguments).block();

        // Then
        assertNotNull(result);
        assertFalse(result.containsKey("meta"));
    }

    @Test
    void listContactFields_MultipleFields_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);

        Map<String, Object> emailField = new HashMap<>();
        emailField.put("id", 1L);
        emailField.put("data", "email@example.com");
        emailField.put("contact_field_type_id", 1);
        emailField.put("created_at", "2025-01-01T00:00:00Z");
        emailField.put("updated_at", "2025-01-01T00:00:00Z");

        Map<String, Object> phoneField = new HashMap<>();
        phoneField.put("id", 2L);
        phoneField.put("data", "+1-555-1234");
        phoneField.put("contact_field_type_id", 2);
        phoneField.put("created_at", "2025-01-01T01:00:00Z");
        phoneField.put("updated_at", "2025-01-01T01:00:00Z");

        Map<String, Object> twitterField = new HashMap<>();
        twitterField.put("id", 3L);
        twitterField.put("data", "@johndoe");
        twitterField.put("contact_field_type_id", 3);
        twitterField.put("created_at", "2025-01-01T02:00:00Z");
        twitterField.put("updated_at", "2025-01-01T02:00:00Z");

        List<Map<String, Object>> fields = List.of(emailField, phoneField, twitterField);
        Map<String, Object> listResponse = createListResponse(fields, 1, 10, 3);

        when(monicaClient.get(eq("/contacts/100/contactfields"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = contactFieldService.listContactFields(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(3, data.size());

        assertEquals("email@example.com", data.get(0).get("data"));
        assertEquals(1, data.get(0).get("contactFieldTypeId"));

        assertEquals("+1-555-1234", data.get(1).get("data"));
        assertEquals(2, data.get(1).get("contactFieldTypeId"));

        assertEquals("@johndoe", data.get(2).get("data"));
        assertEquals(3, data.get(2).get("contactFieldTypeId"));
    }

    // ========================================================================================
    // EDGE CASES
    // ========================================================================================

    @Test
    void createContactField_SpecialCharactersInData_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("contactFieldTypeId", 1);
        arguments.put("data", "test+special@example.com");

        when(monicaClient.post(eq("/contactfields"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = contactFieldService.createContactField(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/contactfields"), argThat(data ->
            "test+special@example.com".equals(data.get("data"))
        ));
    }

    @Test
    void createContactField_UnicodeData_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("contactFieldTypeId", 1);
        arguments.put("data", "user@example.com");

        when(monicaClient.post(eq("/contactfields"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = contactFieldService.createContactField(arguments).block();

        // Then
        assertNotNull(result);
    }

    @Test
    void createContactField_LongData_Succeeds() {
        // Given
        String longEmail = "verylongemailaddress".repeat(5) + "@example.com";
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("contactFieldTypeId", 1);
        arguments.put("data", longEmail);

        when(monicaClient.post(eq("/contactfields"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = contactFieldService.createContactField(arguments).block();

        // Then
        assertNotNull(result);
    }

    @Test
    void getContactField_LargeId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 9999999999L);

        when(monicaClient.get(eq("/contactfields/9999999999"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        contactFieldService.getContactField(arguments).block();

        // Then
        verify(monicaClient).get(eq("/contactfields/9999999999"), isNull());
    }

    @Test
    void listContactFields_IntegerContactId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100);

        Map<String, Object> listResponse = createListResponse(List.of());

        when(monicaClient.get(eq("/contacts/100/contactfields"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        contactFieldService.listContactFields(arguments).block();

        // Then
        verify(monicaClient).get(eq("/contacts/100/contactfields"), any());
    }
}
