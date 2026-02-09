package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.ReminderFieldMappingConfig;
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
 * Unit tests for ReminderService covering reminder scheduling, frequency validation,
 * date handling, and CRUD operations.
 */
@ExtendWith(MockitoExtension.class)
class ReminderServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private ReminderService reminderService;
    private ReminderFieldMappingConfig fieldMappingConfig;

    private Map<String, Object> mockReminderData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        fieldMappingConfig = new ReminderFieldMappingConfig();
        reminderService = new ReminderService(monicaClient, contentFormatter, fieldMappingConfig);

        mockReminderData = reminderBuilder()
            .id(1L)
            .title("Call mom on birthday")
            .contactId(10L)
            .initialDate("2024-03-15")
            .frequencyType("yearly")
            .build();

        mockApiResponse = createSingleEntityResponse(mockReminderData);
    }

    // ========================================================================================
    // CREATE REMINDER TESTS
    // ========================================================================================

    @Test
    void createReminder_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("title", "Call mom on birthday");
        arguments.put("initialDate", "2024-03-15");

        when(monicaClient.post(eq("/reminders"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted reminder JSON");

        // When
        Map<String, Object> result = reminderService.createReminder(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted reminder JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/reminders"), argThat(data ->
            data.get("contact_id").equals(10L) &&
            "Call mom on birthday".equals(data.get("title")) &&
            "2024-03-15".equals(data.get("initial_date"))
        ));
    }

    @Test
    void createReminder_MissingContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", "Test reminder");
        arguments.put("initialDate", "2024-03-15");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            reminderService.createReminder(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createReminder_NullContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", null);
        arguments.put("title", "Test reminder");
        arguments.put("initialDate", "2024-03-15");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            reminderService.createReminder(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createReminder_MissingTitle_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("initialDate", "2024-03-15");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            reminderService.createReminder(arguments).block();
        });
        assertEquals("title is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createReminder_NullTitle_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("title", null);
        arguments.put("initialDate", "2024-03-15");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            reminderService.createReminder(arguments).block();
        });
        assertEquals("title is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createReminder_EmptyTitle_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("title", "   ");
        arguments.put("initialDate", "2024-03-15");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            reminderService.createReminder(arguments).block();
        });
        assertEquals("title is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createReminder_MissingInitialDate_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("title", "Test reminder");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            reminderService.createReminder(arguments).block();
        });
        assertTrue(exception.getMessage().contains("initialDate is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createReminder_NullInitialDate_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("title", "Test reminder");
        arguments.put("initialDate", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            reminderService.createReminder(arguments).block();
        });
        assertTrue(exception.getMessage().contains("initialDate is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createReminder_EmptyArguments_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            reminderService.createReminder(arguments).block();
        });
        assertEquals("Reminder arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createReminder_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            reminderService.createReminder(null).block();
        });
        assertEquals("Reminder arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createReminder_StringContactId_Succeeds() {
        // Given - String contactId that is parseable to a number
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", "10");
        arguments.put("title", "Test reminder");
        arguments.put("initialDate", "2024-03-15");

        when(monicaClient.post(eq("/reminders"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted reminder JSON");

        // When
        Map<String, Object> result = reminderService.createReminder(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/reminders"), any());
    }

    @Test
    void createReminder_MapsContactIdField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("title", "Test reminder");
        arguments.put("initialDate", "2024-03-15");

        when(monicaClient.post(eq("/reminders"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted reminder JSON");

        // When
        reminderService.createReminder(arguments).block();

        // Then - verify contactId is mapped to contact_id
        verify(monicaClient).post(eq("/reminders"), argThat(data ->
            data.get("contact_id").equals(10L) &&
            !data.containsKey("contactId")
        ));
    }

    @Test
    void createReminder_MapsInitialDateField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("title", "Test reminder");
        arguments.put("initialDate", "2024-03-15");

        when(monicaClient.post(eq("/reminders"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted reminder JSON");

        // When
        reminderService.createReminder(arguments).block();

        // Then - verify initialDate is mapped to initial_date
        verify(monicaClient).post(eq("/reminders"), argThat(data ->
            "2024-03-15".equals(data.get("initial_date")) &&
            !data.containsKey("initialDate")
        ));
    }

    @Test
    void createReminder_MapsNextExpectedDateField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("title", "Test reminder");
        arguments.put("initialDate", "2024-03-15");
        arguments.put("nextExpectedDate", "2025-03-15");

        when(monicaClient.post(eq("/reminders"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted reminder JSON");

        // When
        reminderService.createReminder(arguments).block();

        // Then - verify nextExpectedDate is mapped to next_expected_date
        verify(monicaClient).post(eq("/reminders"), argThat(data ->
            "2025-03-15".equals(data.get("next_expected_date")) &&
            !data.containsKey("nextExpectedDate")
        ));
    }

    @Test
    void createReminder_MapsLastTriggeredField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("title", "Test reminder");
        arguments.put("initialDate", "2024-03-15");
        arguments.put("lastTriggered", "2023-03-15");

        when(monicaClient.post(eq("/reminders"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted reminder JSON");

        // When
        reminderService.createReminder(arguments).block();

        // Then - verify lastTriggered is mapped to last_triggered
        verify(monicaClient).post(eq("/reminders"), argThat(data ->
            "2023-03-15".equals(data.get("last_triggered")) &&
            !data.containsKey("lastTriggered")
        ));
    }

    @Test
    void createReminder_WithFrequencyType_PassesThroughUnchanged() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("title", "Test reminder");
        arguments.put("initialDate", "2024-03-15");
        arguments.put("frequencyType", "yearly");  // Use camelCase as per field mapping config

        when(monicaClient.post(eq("/reminders"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted reminder JSON");

        // When
        reminderService.createReminder(arguments).block();

        // Then - verify frequencyType is mapped to frequency_type for API
        verify(monicaClient).post(eq("/reminders"), argThat(data ->
            "yearly".equals(data.get("frequency_type"))
        ));
    }

    @Test
    void createReminder_WithDescription_PassesThroughUnchanged() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("title", "Test reminder");
        arguments.put("initialDate", "2024-03-15");
        arguments.put("description", "Remember to call her");

        when(monicaClient.post(eq("/reminders"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted reminder JSON");

        // When
        reminderService.createReminder(arguments).block();

        // Then - verify description passes through unchanged
        verify(monicaClient).post(eq("/reminders"), argThat(data ->
            "Remember to call her".equals(data.get("description"))
        ));
    }

    // ========================================================================================
    // GET REMINDER TESTS
    // ========================================================================================

    @Test
    void getReminder_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/reminders/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted reminder JSON");

        // When
        Map<String, Object> result = reminderService.getReminder(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/reminders/1"), any());
    }

    @Test
    void getReminder_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            reminderBuilder().id(42L).title("Test Reminder").build()
        );

        when(monicaClient.get(eq("/reminders/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted reminder JSON");

        // When
        Map<String, Object> result = reminderService.getReminder(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/reminders/42"), any());
    }

    @Test
    void getReminder_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            reminderBuilder().id(123L).title("Test Reminder").build()
        );

        when(monicaClient.get(eq("/reminders/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted reminder JSON");

        // When
        Map<String, Object> result = reminderService.getReminder(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/reminders/123"), any());
    }

    @Test
    void getReminder_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("title", "Test");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            reminderService.getReminder(arguments).block();
        });
        assertEquals("Reminder ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getReminder_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            reminderService.getReminder(null).block();
        });
        assertEquals("Reminder ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getReminder_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            reminderService.getReminder(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid reminder ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getReminder_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("title", "Birthday Reminder");
        apiData.put("contact_id", 5L);
        apiData.put("initial_date", "2024-03-15");
        apiData.put("next_expected_date", "2025-03-15");
        apiData.put("last_triggered", "2024-03-15");
        apiData.put("frequency_type", "yearly");
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T09:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/reminders/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = reminderService.getReminder(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals(5L, data.get("contactId"));
        assertEquals("2025-03-15", data.get("nextExpectedDate"));
        assertEquals("2024-03-15", data.get("lastTriggered"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T09:00:00Z", data.get("updatedAt"));
        assertEquals("yearly", data.get("frequencyType"));  // frequency_type is mapped to frequencyType
        // These should pass through unchanged
        assertEquals("Birthday Reminder", data.get("title"));
    }

    // ========================================================================================
    // UPDATE REMINDER TESTS
    // ========================================================================================

    @Test
    void updateReminder_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("title", "Updated reminder title");
        arguments.put("initialDate", "2024-04-15");

        when(monicaClient.put(eq("/reminders/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted reminder JSON");

        // When
        Map<String, Object> result = reminderService.updateReminder(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/reminders/1"), argThat(data ->
            "Updated reminder title".equals(data.get("title"))
        ));
    }

    @Test
    void updateReminder_RemovesIdFromUpdateData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 5L);
        arguments.put("title", "Updated reminder");

        when(monicaClient.put(eq("/reminders/5"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted reminder JSON");

        // When
        reminderService.updateReminder(arguments).block();

        // Then - verify that id is NOT included in the request body
        verify(monicaClient).put(eq("/reminders/5"), argThat(data ->
            !data.containsKey("id")
        ));
    }

    @Test
    void updateReminder_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("title", "Updated reminder");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            reminderService.updateReminder(arguments).block();
        });
        assertEquals("Reminder ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateReminder_WithContactId_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactId", 20L);

        when(monicaClient.put(eq("/reminders/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted reminder JSON");

        // When
        reminderService.updateReminder(arguments).block();

        // Then - verify contactId is mapped to contact_id
        verify(monicaClient).put(eq("/reminders/1"), argThat(data ->
            Long.valueOf(20L).equals(data.get("contact_id")) &&
            !data.containsKey("contactId")
        ));
    }

    @Test
    void updateReminder_WithInitialDate_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("initialDate", "2024-06-01");

        when(monicaClient.put(eq("/reminders/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted reminder JSON");

        // When
        reminderService.updateReminder(arguments).block();

        // Then - verify initialDate is mapped to initial_date
        verify(monicaClient).put(eq("/reminders/1"), argThat(data ->
            "2024-06-01".equals(data.get("initial_date")) &&
            !data.containsKey("initialDate")
        ));
    }

    @Test
    void updateReminder_WithNextExpectedDate_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("nextExpectedDate", "2025-06-01");

        when(monicaClient.put(eq("/reminders/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted reminder JSON");

        // When
        reminderService.updateReminder(arguments).block();

        // Then - verify nextExpectedDate is mapped to next_expected_date
        verify(monicaClient).put(eq("/reminders/1"), argThat(data ->
            "2025-06-01".equals(data.get("next_expected_date")) &&
            !data.containsKey("nextExpectedDate")
        ));
    }

    @Test
    void updateReminder_WithLastTriggered_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("lastTriggered", "2024-03-15");

        when(monicaClient.put(eq("/reminders/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted reminder JSON");

        // When
        reminderService.updateReminder(arguments).block();

        // Then - verify lastTriggered is mapped to last_triggered
        verify(monicaClient).put(eq("/reminders/1"), argThat(data ->
            "2024-03-15".equals(data.get("last_triggered")) &&
            !data.containsKey("lastTriggered")
        ));
    }

    @Test
    void updateReminder_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "42");
        arguments.put("title", "Updated reminder");

        when(monicaClient.put(eq("/reminders/42"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted reminder JSON");

        // When
        reminderService.updateReminder(arguments).block();

        // Then
        verify(monicaClient).put(eq("/reminders/42"), any());
    }

    @Test
    void updateReminder_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");
        arguments.put("title", "Updated reminder");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            reminderService.updateReminder(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid reminder ID format"));
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // DELETE REMINDER TESTS
    // ========================================================================================

    @Test
    void deleteReminder_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/reminders/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Reminder"), eq(1L), eq(true), anyString()
        )).thenReturn("Reminder deleted successfully");

        // When
        Map<String, Object> result = reminderService.deleteReminder(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));

        verify(monicaClient).delete(eq("/reminders/1"));
    }

    @Test
    void deleteReminder_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/reminders/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Reminder"), eq(99L), eq(true), anyString()
        )).thenReturn("Reminder deleted successfully");

        // When
        Map<String, Object> result = reminderService.deleteReminder(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/reminders/99"));
    }

    @Test
    void deleteReminder_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 55);
        Map<String, Object> deleteResponse = createDeleteResponse(55L);

        when(monicaClient.delete(eq("/reminders/55"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Reminder"), eq(55L), eq(true), anyString()
        )).thenReturn("Reminder deleted successfully");

        // When
        Map<String, Object> result = reminderService.deleteReminder(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/reminders/55"));
    }

    @Test
    void deleteReminder_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("title", "Test");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            reminderService.deleteReminder(arguments).block();
        });
        assertEquals("Reminder ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteReminder_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            reminderService.deleteReminder(null).block();
        });
        assertEquals("Reminder ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteReminder_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            reminderService.deleteReminder(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid reminder ID format"));
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST REMINDERS TESTS
    // ========================================================================================

    @Test
    void listReminders_WithPagination_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> reminders = List.of(
            reminderBuilder().id(1L).title("Reminder 1").build(),
            reminderBuilder().id(2L).title("Reminder 2").build()
        );
        Map<String, Object> listResponse = createListResponse(reminders, 2, 20, 50);

        when(monicaClient.get(eq("/reminders"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = reminderService.listReminders(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/reminders"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listReminders_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> reminders = List.of(
            reminderBuilder().id(1L).title("Reminder 1").build()
        );
        Map<String, Object> listResponse = createListResponse(reminders);

        when(monicaClient.get(eq("/reminders"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        reminderService.listReminders(arguments).block();

        // Then - verify default pagination values
        verify(monicaClient).get(eq("/reminders"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listReminders_WithContactFilter_IncludesQueryParam() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", 5L);

        List<Map<String, Object>> reminders = List.of(
            reminderBuilder().id(1L).title("Reminder for contact").contactId(5L).build()
        );
        Map<String, Object> listResponse = createListResponse(reminders);

        when(monicaClient.get(eq("/reminders"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        reminderService.listReminders(arguments).block();

        // Then - verify contactId is mapped to "contact_id" query param
        verify(monicaClient).get(eq("/reminders"), argThat(params ->
            "5".equals(params.get("contact_id"))
        ));
    }

    @Test
    void listReminders_LimitAboveMaximum_ClampsTo100() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 200);

        List<Map<String, Object>> reminders = List.of(
            reminderBuilder().id(1L).title("Reminder 1").build()
        );
        Map<String, Object> listResponse = createListResponse(reminders);

        when(monicaClient.get(eq("/reminders"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        reminderService.listReminders(arguments).block();

        // Then - verify limit is clamped to 100
        verify(monicaClient).get(eq("/reminders"), argThat(params ->
            "100".equals(params.get("limit"))
        ));
    }

    @Test
    void listReminders_LimitBelowMinimum_ClampsTo1() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 0);

        List<Map<String, Object>> reminders = List.of(
            reminderBuilder().id(1L).title("Reminder 1").build()
        );
        Map<String, Object> listResponse = createListResponse(reminders);

        when(monicaClient.get(eq("/reminders"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        reminderService.listReminders(arguments).block();

        // Then - verify limit is clamped to 1
        verify(monicaClient).get(eq("/reminders"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listReminders_NegativeLimit_ClampsTo1() {
        // Given
        Map<String, Object> arguments = Map.of("limit", -5);

        List<Map<String, Object>> reminders = List.of(
            reminderBuilder().id(1L).title("Reminder 1").build()
        );
        Map<String, Object> listResponse = createListResponse(reminders);

        when(monicaClient.get(eq("/reminders"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        reminderService.listReminders(arguments).block();

        // Then - verify limit is clamped to 1
        verify(monicaClient).get(eq("/reminders"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listReminders_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> reminders = List.of(
            reminderBuilder().id(1L).title("Reminder 1").build()
        );
        Map<String, Object> listResponse = createListResponse(reminders, 1, 10, 100);

        when(monicaClient.get(eq("/reminders"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = reminderService.listReminders(arguments).block();

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
    void listReminders_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/reminders"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = reminderService.listReminders(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listReminders_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> reminders = List.of(
            reminderBuilder().id(1L).title("Reminder 1").build()
        );
        Map<String, Object> listResponse = createListResponse(reminders);

        when(monicaClient.get(eq("/reminders"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        reminderService.listReminders(arguments).block();

        // Then
        verify(monicaClient).get(eq("/reminders"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listReminders_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> reminderWithSnakeCaseFields = new HashMap<>();
        reminderWithSnakeCaseFields.put("id", 1L);
        reminderWithSnakeCaseFields.put("title", "Reminder with contact");
        reminderWithSnakeCaseFields.put("contact_id", 10L);
        reminderWithSnakeCaseFields.put("initial_date", "2024-03-15");
        reminderWithSnakeCaseFields.put("next_expected_date", "2025-03-15");
        reminderWithSnakeCaseFields.put("last_triggered", "2024-03-15");
        reminderWithSnakeCaseFields.put("frequency_type", "yearly");
        reminderWithSnakeCaseFields.put("created_at", "2024-01-15T10:00:00Z");
        reminderWithSnakeCaseFields.put("updated_at", "2024-01-15T10:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(reminderWithSnakeCaseFields));

        when(monicaClient.get(eq("/reminders"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = reminderService.listReminders(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify field mapping from snake_case to camelCase
        assertEquals(10L, data.get(0).get("contactId"));
        assertEquals("2025-03-15", data.get(0).get("nextExpectedDate"));
        assertEquals("2024-03-15", data.get(0).get("lastTriggered"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("updatedAt"));
        assertEquals("yearly", data.get(0).get("frequencyType"));  // frequency_type is mapped to frequencyType
        // These should pass through unchanged
        assertEquals("Reminder with contact", data.get(0).get("title"));
    }

    @Test
    void listReminders_WithStringPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("page", "3");

        List<Map<String, Object>> reminders = List.of(
            reminderBuilder().id(1L).title("Reminder 1").build()
        );
        Map<String, Object> listResponse = createListResponse(reminders, 3, 10, 30);

        when(monicaClient.get(eq("/reminders"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        reminderService.listReminders(arguments).block();

        // Then
        verify(monicaClient).get(eq("/reminders"), argThat(params ->
            "3".equals(params.get("page"))
        ));
    }

    @Test
    void listReminders_WithIntegerContactId_ConvertsToString() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", 42);

        List<Map<String, Object>> reminders = List.of(
            reminderBuilder().id(1L).title("Reminder 1").contactId(42L).build()
        );
        Map<String, Object> listResponse = createListResponse(reminders);

        when(monicaClient.get(eq("/reminders"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        reminderService.listReminders(arguments).block();

        // Then
        verify(monicaClient).get(eq("/reminders"), argThat(params ->
            "42".equals(params.get("contact_id"))
        ));
    }

    @Test
    void listReminders_NullContactId_ExcludesFromQueryParams() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", null);

        List<Map<String, Object>> reminders = List.of(
            reminderBuilder().id(1L).title("Reminder 1").build()
        );
        Map<String, Object> listResponse = createListResponse(reminders);

        when(monicaClient.get(eq("/reminders"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        reminderService.listReminders(arguments).block();

        // Then - verify contactId is not included in query params
        verify(monicaClient).get(eq("/reminders"), argThat(params ->
            !params.containsKey("contact_id")
        ));
    }

    @Test
    void listReminders_WithStringContactId_ConvertsToString() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", "15");

        List<Map<String, Object>> reminders = List.of(
            reminderBuilder().id(1L).title("Reminder 1").contactId(15L).build()
        );
        Map<String, Object> listResponse = createListResponse(reminders);

        when(monicaClient.get(eq("/reminders"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        reminderService.listReminders(arguments).block();

        // Then
        verify(monicaClient).get(eq("/reminders"), argThat(params ->
            "15".equals(params.get("contact_id"))
        ));
    }
}
