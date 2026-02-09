package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.ActivityFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Unit tests for ActivityService covering CRUD operations, validation, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class ActivityServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private ActivityFieldMappingConfig fieldMappingConfig;
    private ActivityService activityService;

    private Map<String, Object> mockActivityData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        fieldMappingConfig = new ActivityFieldMappingConfig();
        activityService = new ActivityService(monicaClient, contentFormatter, fieldMappingConfig);
        mockActivityData = activityBuilder()
            .id(1L)
            .summary("Team meeting")
            .description("Weekly standup meeting")
            .activityTypeId(1)
            .happenedAt("2024-01-15")
            .attendees(List.of(1L, 2L))
            .build();

        mockApiResponse = createSingleEntityResponse(mockActivityData);
    }

    // ========================================================================================
    // CREATE ACTIVITY TESTS
    // ========================================================================================

    @Test
    void createActivity_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("summary", "Team meeting");
        arguments.put("description", "Weekly standup meeting");
        arguments.put("attendees", List.of(Map.of("contactId", 1L), Map.of("contactId", 2L)));
        arguments.put("happenedAt", "2024-01-15");

        when(monicaClient.post(eq("/activities"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity JSON");

        // When
        Map<String, Object> result = activityService.createActivity(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted activity JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/activities"), argThat(data ->
            "Team meeting".equals(data.get("summary")) &&
            data.containsKey("contacts")
        ));
    }

    @Test
    void createActivity_WithIntegerAttendees_ReturnsFormattedResponse() {
        // Given - Attendees can be raw integers (contact IDs)
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("summary", "Lunch meeting");
        arguments.put("attendees", List.of(91, 73));
        arguments.put("happenedAt", "2025-01-15");

        when(monicaClient.post(eq("/activities"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity JSON");

        // When
        Map<String, Object> result = activityService.createActivity(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/activities"), argThat(data -> {
            @SuppressWarnings("unchecked")
            List<Integer> contacts = (List<Integer>) data.get("contacts");
            return contacts != null &&
                   contacts.size() == 2 &&
                   Integer.valueOf(91).equals(contacts.get(0)) &&
                   Integer.valueOf(73).equals(contacts.get(1));
        }));
    }

    @Test
    void createActivity_MissingSummary_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("attendees", List.of(Map.of("contactId", 1L)));

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityService.createActivity(arguments).block();
        });
        assertEquals("summary is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createActivity_EmptySummary_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("summary", "   ");
        arguments.put("attendees", List.of(Map.of("contactId", 1L)));

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityService.createActivity(arguments).block();
        });
        assertEquals("summary is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createActivity_NullSummary_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("summary", null);
        arguments.put("attendees", List.of(Map.of("contactId", 1L)));

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityService.createActivity(arguments).block();
        });
        assertEquals("summary is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createActivity_MissingAttendees_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("summary", "Meeting");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityService.createActivity(arguments).block();
        });
        assertEquals("attendees is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createActivity_NullAttendees_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("summary", "Meeting");
        arguments.put("attendees", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityService.createActivity(arguments).block();
        });
        assertEquals("attendees is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createActivity_EmptyAttendees_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("summary", "Meeting");
        arguments.put("attendees", List.of());

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityService.createActivity(arguments).block();
        });
        assertEquals("attendees cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createActivity_InvalidAttendeesFormat_NotArray_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("summary", "Meeting");
        arguments.put("attendees", "invalid-string");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityService.createActivity(arguments).block();
        });
        assertEquals("attendees must be an array", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createActivity_InvalidAttendeesFormat_MissingContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("summary", "Meeting");
        arguments.put("attendees", List.of(Map.of("name", "John"))); // Missing contactId

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityService.createActivity(arguments).block();
        });
        assertEquals("Invalid attendees format: object must contain 'contactId' field", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createActivity_InvalidAttendeesFormat_EmptyStringName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("summary", "Meeting");
        arguments.put("attendees", List.of("   "));

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityService.createActivity(arguments).block();
        });
        assertEquals("Invalid attendees format: attendee name cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createActivity_EmptyArguments_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityService.createActivity(arguments).block();
        });
        assertEquals("Activity arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createActivity_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityService.createActivity(null).block();
        });
        assertEquals("Activity arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createActivity_WithNumberAttendees_Succeeds() {
        // Given - Numbers are allowed and converted to strings
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("summary", "Meeting");
        arguments.put("attendees", List.of(123, 456));
        arguments.put("happenedAt", "2025-01-15");

        when(monicaClient.post(eq("/activities"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity JSON");

        // When
        Map<String, Object> result = activityService.createActivity(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/activities"), any());
    }

    @Test
    void createActivity_MapsHappenedAtField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("summary", "Meeting");
        arguments.put("attendees", List.of(Map.of("contactId", 1L)));
        arguments.put("happenedAt", "2024-01-20");

        when(monicaClient.post(eq("/activities"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity JSON");

        // When
        activityService.createActivity(arguments).block();

        // Then - verify happenedAt is mapped to happened_at
        verify(monicaClient).post(eq("/activities"), argThat(data ->
            "2024-01-20".equals(data.get("happened_at"))
        ));
    }

    // ========================================================================================
    // GET ACTIVITY TESTS
    // ========================================================================================

    @Test
    void getActivity_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/activities/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity JSON");

        // When
        Map<String, Object> result = activityService.getActivity(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/activities/1"), any());
    }

    @Test
    void getActivity_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            activityBuilder().id(42L).summary("Test Activity").build()
        );

        when(monicaClient.get(eq("/activities/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity JSON");

        // When
        Map<String, Object> result = activityService.getActivity(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/activities/42"), any());
    }

    @Test
    void getActivity_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            activityBuilder().id(123L).summary("Test Activity").build()
        );

        when(monicaClient.get(eq("/activities/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity JSON");

        // When
        Map<String, Object> result = activityService.getActivity(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/activities/123"), any());
    }

    @Test
    void getActivity_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("summary", "Test");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityService.getActivity(arguments).block();
        });
        assertEquals("Activity ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getActivity_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityService.getActivity(null).block();
        });
        assertEquals("Activity ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getActivity_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityService.getActivity(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid activity ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getActivity_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("summary", "Test Activity");
        apiData.put("happened_at", "2024-01-15");
        apiData.put("activity_type_id", 2);
        apiData.put("created_at", "2024-01-14T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T09:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/activities/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = activityService.getActivity(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals("2024-01-15", data.get("happenedAt"));
        assertEquals(2, data.get("activityTypeId"));
        assertEquals("2024-01-14T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T09:00:00Z", data.get("updatedAt"));
    }

    // ========================================================================================
    // UPDATE ACTIVITY TESTS
    // ========================================================================================

    @Test
    void updateActivity_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("summary", "Updated meeting");
        arguments.put("description", "Updated description");

        when(monicaClient.put(eq("/activities/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity JSON");

        // When
        Map<String, Object> result = activityService.updateActivity(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/activities/1"), argThat(data ->
            "Updated meeting".equals(data.get("summary")) &&
            "Updated description".equals(data.get("description"))
        ));
    }

    @Test
    void updateActivity_RemovesIdFromUpdateData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 5L);
        arguments.put("summary", "Updated meeting");

        when(monicaClient.put(eq("/activities/5"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity JSON");

        // When
        activityService.updateActivity(arguments).block();

        // Then - verify that id is NOT included in the request body
        verify(monicaClient).put(eq("/activities/5"), argThat(data ->
            !data.containsKey("id")
        ));
    }

    @Test
    void updateActivity_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("summary", "Updated meeting");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityService.updateActivity(arguments).block();
        });
        assertEquals("Activity ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateActivity_WithHappenedAt_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("happenedAt", "2024-02-01");

        when(monicaClient.put(eq("/activities/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity JSON");

        // When
        activityService.updateActivity(arguments).block();

        // Then - verify happenedAt is mapped to happened_at
        verify(monicaClient).put(eq("/activities/1"), argThat(data ->
            "2024-02-01".equals(data.get("happened_at")) &&
            !data.containsKey("happenedAt")
        ));
    }

    @Test
    void updateActivity_WithAttendees_ExtractsContactIdCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("attendees", List.of(Map.of("contactId", 10L)));

        when(monicaClient.put(eq("/activities/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted activity JSON");

        // When
        activityService.updateActivity(arguments).block();

        // Then - verify contacts array contains just the contact ID (not an object)
        verify(monicaClient).put(eq("/activities/1"), argThat(data -> {
            @SuppressWarnings("unchecked")
            List<?> contacts = (List<?>) data.get("contacts");
            return contacts != null &&
                   contacts.size() == 1 &&
                   Long.valueOf(10L).equals(contacts.get(0));
        }));
    }

    // ========================================================================================
    // DELETE ACTIVITY TESTS
    // ========================================================================================

    @Test
    void deleteActivity_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/activities/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Activity"), eq(1L), eq(true), anyString()
        )).thenReturn("Activity deleted successfully");

        // When
        Map<String, Object> result = activityService.deleteActivity(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));

        verify(monicaClient).delete(eq("/activities/1"));
    }

    @Test
    void deleteActivity_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/activities/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Activity"), eq(99L), eq(true), anyString()
        )).thenReturn("Activity deleted successfully");

        // When
        Map<String, Object> result = activityService.deleteActivity(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/activities/99"));
    }

    @Test
    void deleteActivity_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("summary", "Test");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityService.deleteActivity(arguments).block();
        });
        assertEquals("Activity ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteActivity_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            activityService.deleteActivity(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid activity ID format"));
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST ACTIVITIES TESTS
    // ========================================================================================

    @Test
    void listActivities_WithPagination_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> activities = List.of(
            activityBuilder().id(1L).summary("Activity 1").build(),
            activityBuilder().id(2L).summary("Activity 2").build()
        );
        Map<String, Object> listResponse = createListResponse(activities, 2, 20, 50);

        when(monicaClient.get(eq("/activities"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = activityService.listActivities(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/activities"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listActivities_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> activities = List.of(
            activityBuilder().id(1L).summary("Activity 1").build()
        );
        Map<String, Object> listResponse = createListResponse(activities);

        when(monicaClient.get(eq("/activities"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        activityService.listActivities(arguments).block();

        // Then - verify default pagination values
        verify(monicaClient).get(eq("/activities"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listActivities_WithContactId_IncludesContactsQueryParam() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", 5L);

        List<Map<String, Object>> activities = List.of(
            activityBuilder().id(1L).summary("Activity with contact").build()
        );
        Map<String, Object> listResponse = createListResponse(activities);

        when(monicaClient.get(eq("/activities"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        activityService.listActivities(arguments).block();

        // Then - verify contactId is mapped to "contacts" query param
        verify(monicaClient).get(eq("/activities"), argThat(params ->
            "5".equals(params.get("contacts"))
        ));
    }

    @Test
    void listActivities_LimitAboveMaximum_ClampsTo100() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 200);

        List<Map<String, Object>> activities = List.of(
            activityBuilder().id(1L).summary("Activity 1").build()
        );
        Map<String, Object> listResponse = createListResponse(activities);

        when(monicaClient.get(eq("/activities"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        activityService.listActivities(arguments).block();

        // Then - verify limit is clamped to 100
        verify(monicaClient).get(eq("/activities"), argThat(params ->
            "100".equals(params.get("limit"))
        ));
    }

    @Test
    void listActivities_LimitBelowMinimum_ClampsTo1() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 0);

        List<Map<String, Object>> activities = List.of(
            activityBuilder().id(1L).summary("Activity 1").build()
        );
        Map<String, Object> listResponse = createListResponse(activities);

        when(monicaClient.get(eq("/activities"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        activityService.listActivities(arguments).block();

        // Then - verify limit is clamped to 1
        verify(monicaClient).get(eq("/activities"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listActivities_NegativeLimit_ClampsTo1() {
        // Given
        Map<String, Object> arguments = Map.of("limit", -5);

        List<Map<String, Object>> activities = List.of(
            activityBuilder().id(1L).summary("Activity 1").build()
        );
        Map<String, Object> listResponse = createListResponse(activities);

        when(monicaClient.get(eq("/activities"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        activityService.listActivities(arguments).block();

        // Then - verify limit is clamped to 1
        verify(monicaClient).get(eq("/activities"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listActivities_MapsAttendeesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> activityWithAttendees = new HashMap<>();
        activityWithAttendees.put("id", 1L);
        activityWithAttendees.put("summary", "Meeting");
        activityWithAttendees.put("attendees", List.of(
            Map.of("contact_id", 10L, "name", "John"),
            Map.of("contact_id", 20L, "name", "Jane")
        ));

        Map<String, Object> listResponse = createListResponse(List.of(activityWithAttendees));

        when(monicaClient.get(eq("/activities"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = activityService.listActivities(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> attendees = (List<Map<String, Object>>) data.get(0).get("attendees");
        assertEquals(2, attendees.size());
        // Verify contact_id is mapped to contactId
        assertEquals(10L, attendees.get(0).get("contactId"));
        assertEquals(20L, attendees.get(1).get("contactId"));
    }

    @Test
    void listActivities_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> activities = List.of(
            activityBuilder().id(1L).summary("Activity 1").build()
        );
        Map<String, Object> listResponse = createListResponse(activities, 1, 10, 100);

        when(monicaClient.get(eq("/activities"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = activityService.listActivities(arguments).block();

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
    void listActivities_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/activities"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = activityService.listActivities(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listActivities_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> activities = List.of(
            activityBuilder().id(1L).summary("Activity 1").build()
        );
        Map<String, Object> listResponse = createListResponse(activities);

        when(monicaClient.get(eq("/activities"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        activityService.listActivities(arguments).block();

        // Then
        verify(monicaClient).get(eq("/activities"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }
}
