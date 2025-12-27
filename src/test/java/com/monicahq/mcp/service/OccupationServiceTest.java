package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.OccupationFieldMappingConfig;
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
 * Unit tests for OccupationService covering CRUD operations,
 * company linking, field mapping, validation, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class OccupationServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private OccupationService occupationService;

    private Map<String, Object> mockOccupationData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        OccupationFieldMappingConfig config = new OccupationFieldMappingConfig();
        occupationService = new OccupationService(monicaClient, contentFormatter, config);

        mockOccupationData = occupationBuilder()
            .id(1L)
            .contactId(42L)
            .title("Software Engineer")
            .currentlyWorksHere(true)
            .build();

        mockApiResponse = createSingleEntityResponse(mockOccupationData);
    }

    // ========================================================================================
    // CREATE OCCUPATION TESTS
    // ========================================================================================

    @Test
    void createOccupation_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("title", "Software Engineer");

        when(monicaClient.post(eq("/occupations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        Map<String, Object> result = occupationService.createOccupation(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted occupation JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/occupations"), argThat(data ->
            Long.valueOf(42L).equals(data.get("contact_id")) &&
            "Software Engineer".equals(data.get("title"))
        ));
    }

    @Test
    void createOccupation_MissingContactId_ThrowsException() {
        // Given - has title but no contactId
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", "Developer");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            occupationService.createOccupation(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createOccupation_NullContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", null);
        arguments.put("title", "Developer");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            occupationService.createOccupation(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createOccupation_MissingTitle_ThrowsException() {
        // Given - has contactId but no title
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            occupationService.createOccupation(arguments).block();
        });
        assertEquals("title is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createOccupation_NullTitle_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("title", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            occupationService.createOccupation(arguments).block();
        });
        assertEquals("title is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createOccupation_EmptyTitle_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("title", "");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            occupationService.createOccupation(arguments).block();
        });
        assertEquals("title is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createOccupation_WhitespaceTitle_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("title", "   ");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            occupationService.createOccupation(arguments).block();
        });
        assertEquals("title is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createOccupation_WithCompanyId_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("title", "Developer");
        arguments.put("companyId", 10L);

        when(monicaClient.post(eq("/occupations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.createOccupation(arguments).block();

        // Then - verify companyId is mapped to company_id
        verify(monicaClient).post(eq("/occupations"), argThat(data ->
            Long.valueOf(10L).equals(data.get("company_id")) &&
            !data.containsKey("companyId")
        ));
    }

    @Test
    void createOccupation_WithDescription_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("title", "Developer");
        arguments.put("description", "Full-stack development role");

        when(monicaClient.post(eq("/occupations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.createOccupation(arguments).block();

        // Then - verify description is passed through
        verify(monicaClient).post(eq("/occupations"), argThat(data ->
            "Full-stack development role".equals(data.get("description"))
        ));
    }

    @Test
    void createOccupation_WithSalary_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("title", "Developer");
        arguments.put("salary", "75000");

        when(monicaClient.post(eq("/occupations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.createOccupation(arguments).block();

        // Then - verify salary is passed through
        verify(monicaClient).post(eq("/occupations"), argThat(data ->
            "75000".equals(data.get("salary"))
        ));
    }

    @Test
    void createOccupation_WithSalaryUnit_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("title", "Developer");
        arguments.put("salary", "75000");
        arguments.put("salaryUnit", "year");

        when(monicaClient.post(eq("/occupations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.createOccupation(arguments).block();

        // Then - verify salaryUnit is mapped to salary_unit
        verify(monicaClient).post(eq("/occupations"), argThat(data ->
            "year".equals(data.get("salary_unit")) &&
            !data.containsKey("salaryUnit")
        ));
    }

    @Test
    void createOccupation_WithCurrentlyWorksHere_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("title", "Developer");
        arguments.put("currentlyWorksHere", true);

        when(monicaClient.post(eq("/occupations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.createOccupation(arguments).block();

        // Then - verify currentlyWorksHere is mapped to currently_works_here
        verify(monicaClient).post(eq("/occupations"), argThat(data ->
            Boolean.TRUE.equals(data.get("currently_works_here")) &&
            !data.containsKey("currentlyWorksHere")
        ));
    }

    @Test
    void createOccupation_WithStartDate_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("title", "Developer");
        arguments.put("startDate", "2023-01-15");

        when(monicaClient.post(eq("/occupations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.createOccupation(arguments).block();

        // Then - verify startDate is mapped to start_date
        verify(monicaClient).post(eq("/occupations"), argThat(data ->
            "2023-01-15".equals(data.get("start_date")) &&
            !data.containsKey("startDate")
        ));
    }

    @Test
    void createOccupation_WithEndDate_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("title", "Developer");
        arguments.put("endDate", "2024-06-30");

        when(monicaClient.post(eq("/occupations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.createOccupation(arguments).block();

        // Then - verify endDate is mapped to end_date
        verify(monicaClient).post(eq("/occupations"), argThat(data ->
            "2024-06-30".equals(data.get("end_date")) &&
            !data.containsKey("endDate")
        ));
    }

    @Test
    void createOccupation_WithAllFields_MapsAllFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("companyId", 10L);
        arguments.put("title", "Senior Developer");
        arguments.put("description", "Lead development team");
        arguments.put("salary", "120000");
        arguments.put("salaryUnit", "year");
        arguments.put("currentlyWorksHere", false);
        arguments.put("startDate", "2020-01-01");
        arguments.put("endDate", "2024-12-31");

        when(monicaClient.post(eq("/occupations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.createOccupation(arguments).block();

        // Then
        verify(monicaClient).post(eq("/occupations"), argThat(data ->
            Long.valueOf(42L).equals(data.get("contact_id")) &&
            Long.valueOf(10L).equals(data.get("company_id")) &&
            "Senior Developer".equals(data.get("title")) &&
            "Lead development team".equals(data.get("description")) &&
            "120000".equals(data.get("salary")) &&
            "year".equals(data.get("salary_unit")) &&
            Boolean.FALSE.equals(data.get("currently_works_here")) &&
            "2020-01-01".equals(data.get("start_date")) &&
            "2024-12-31".equals(data.get("end_date"))
        ));
    }

    @Test
    void createOccupation_StringContactId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", "99");
        arguments.put("title", "Developer");

        when(monicaClient.post(eq("/occupations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.createOccupation(arguments).block();

        // Then
        verify(monicaClient).post(eq("/occupations"), argThat(data ->
            "99".equals(data.get("contact_id").toString())
        ));
    }

    @Test
    void createOccupation_IntegerContactId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 123);
        arguments.put("title", "Developer");

        when(monicaClient.post(eq("/occupations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.createOccupation(arguments).block();

        // Then
        verify(monicaClient).post(eq("/occupations"), argThat(data ->
            Integer.valueOf(123).equals(data.get("contact_id"))
        ));
    }

    @Test
    void createOccupation_EmptyArguments_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            occupationService.createOccupation(arguments).block();
        });
        assertTrue(exception.getMessage().contains("cannot be empty"));
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // GET OCCUPATION TESTS
    // ========================================================================================

    @Test
    void getOccupation_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/occupations/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        Map<String, Object> result = occupationService.getOccupation(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/occupations/1"), any());
    }

    @Test
    void getOccupation_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            occupationBuilder().id(42L).title("Manager").build()
        );

        when(monicaClient.get(eq("/occupations/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        Map<String, Object> result = occupationService.getOccupation(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/occupations/42"), any());
    }

    @Test
    void getOccupation_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            occupationBuilder().id(123L).title("Director").build()
        );

        when(monicaClient.get(eq("/occupations/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        Map<String, Object> result = occupationService.getOccupation(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/occupations/123"), any());
    }

    @Test
    void getOccupation_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("title", "Developer");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            occupationService.getOccupation(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Occupation ID is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getOccupation_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            occupationService.getOccupation(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Occupation ID is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getOccupation_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            occupationService.getOccupation(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid occupation ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getOccupation_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("contact_id", 42L);
        apiData.put("company_id", 10L);
        apiData.put("title", "Software Engineer");
        apiData.put("description", "Backend development");
        apiData.put("salary", "80000");
        apiData.put("salary_unit", "year");
        apiData.put("currently_works_here", true);
        apiData.put("start_date", "2022-03-01");
        apiData.put("end_date", null);
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T09:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/occupations/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = occupationService.getOccupation(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals(42L, data.get("contactId"));
        assertEquals(10L, data.get("companyId"));
        assertEquals("year", data.get("salaryUnit"));
        assertEquals(true, data.get("currentlyWorksHere"));
        assertEquals("2022-03-01", data.get("startDate"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T09:00:00Z", data.get("updatedAt"));
        // These should remain unchanged
        assertEquals(1L, data.get("id"));
        assertEquals("Software Engineer", data.get("title"));
        assertEquals("Backend development", data.get("description"));
        assertEquals("80000", data.get("salary"));
    }

    @Test
    void getOccupation_DirectResponseWithoutDataWrapper_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        // API response without "data" wrapper
        Map<String, Object> directResponse = new HashMap<>();
        directResponse.put("id", 1L);
        directResponse.put("contact_id", 55L);
        directResponse.put("title", "Consultant");
        directResponse.put("currently_works_here", false);
        directResponse.put("created_at", "2024-01-20T10:00:00Z");
        directResponse.put("updated_at", "2024-01-20T10:00:00Z");

        when(monicaClient.get(eq("/occupations/1"), any())).thenReturn(Mono.just(directResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = occupationService.getOccupation(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        assertEquals("Consultant", data.get("title"));
        assertEquals(55L, data.get("contactId"));
        assertEquals(false, data.get("currentlyWorksHere"));
    }

    // ========================================================================================
    // UPDATE OCCUPATION TESTS
    // ========================================================================================

    @Test
    void updateOccupation_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("title", "Senior Developer");

        when(monicaClient.put(eq("/occupations/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        Map<String, Object> result = occupationService.updateOccupation(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/occupations/1"), argThat(data ->
            "Senior Developer".equals(data.get("title"))
        ));
    }

    @Test
    void updateOccupation_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("title", "Developer");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            occupationService.updateOccupation(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Occupation ID is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateOccupation_EmptyTitle_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("title", "");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            occupationService.updateOccupation(arguments).block();
        });
        assertEquals("title cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateOccupation_WhitespaceTitle_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("title", "   ");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            occupationService.updateOccupation(arguments).block();
        });
        assertEquals("title cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateOccupation_NullTitle_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("title", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            occupationService.updateOccupation(arguments).block();
        });
        assertEquals("title cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateOccupation_WithCompanyId_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("companyId", 25L);

        when(monicaClient.put(eq("/occupations/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.updateOccupation(arguments).block();

        // Then - verify companyId is mapped to company_id
        verify(monicaClient).put(eq("/occupations/1"), argThat(data ->
            Long.valueOf(25L).equals(data.get("company_id")) &&
            !data.containsKey("companyId")
        ));
    }

    @Test
    void updateOccupation_WithSalaryUnit_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("salaryUnit", "month");

        when(monicaClient.put(eq("/occupations/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.updateOccupation(arguments).block();

        // Then - verify salaryUnit is mapped to salary_unit
        verify(monicaClient).put(eq("/occupations/1"), argThat(data ->
            "month".equals(data.get("salary_unit")) &&
            !data.containsKey("salaryUnit")
        ));
    }

    @Test
    void updateOccupation_WithCurrentlyWorksHere_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("currentlyWorksHere", false);

        when(monicaClient.put(eq("/occupations/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.updateOccupation(arguments).block();

        // Then - verify currentlyWorksHere is mapped to currently_works_here
        verify(monicaClient).put(eq("/occupations/1"), argThat(data ->
            Boolean.FALSE.equals(data.get("currently_works_here")) &&
            !data.containsKey("currentlyWorksHere")
        ));
    }

    @Test
    void updateOccupation_WithStartDate_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("startDate", "2023-06-01");

        when(monicaClient.put(eq("/occupations/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.updateOccupation(arguments).block();

        // Then - verify startDate is mapped to start_date
        verify(monicaClient).put(eq("/occupations/1"), argThat(data ->
            "2023-06-01".equals(data.get("start_date")) &&
            !data.containsKey("startDate")
        ));
    }

    @Test
    void updateOccupation_WithEndDate_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("endDate", "2024-05-31");

        when(monicaClient.put(eq("/occupations/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.updateOccupation(arguments).block();

        // Then - verify endDate is mapped to end_date
        verify(monicaClient).put(eq("/occupations/1"), argThat(data ->
            "2024-05-31".equals(data.get("end_date")) &&
            !data.containsKey("endDate")
        ));
    }

    @Test
    void updateOccupation_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "42");
        arguments.put("description", "Updated description");

        when(monicaClient.put(eq("/occupations/42"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.updateOccupation(arguments).block();

        // Then
        verify(monicaClient).put(eq("/occupations/42"), any());
    }

    @Test
    void updateOccupation_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");
        arguments.put("title", "Developer");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            occupationService.updateOccupation(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid occupation ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateOccupation_WithAllFields_MapsAllFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactId", 50L);
        arguments.put("companyId", 15L);
        arguments.put("title", "Lead Developer");
        arguments.put("description", "Leading the team");
        arguments.put("salary", "150000");
        arguments.put("salaryUnit", "year");
        arguments.put("currentlyWorksHere", true);
        arguments.put("startDate", "2021-01-01");
        arguments.put("endDate", "2025-12-31");

        when(monicaClient.put(eq("/occupations/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.updateOccupation(arguments).block();

        // Then
        verify(monicaClient).put(eq("/occupations/1"), argThat(data ->
            Long.valueOf(50L).equals(data.get("contact_id")) &&
            Long.valueOf(15L).equals(data.get("company_id")) &&
            "Lead Developer".equals(data.get("title")) &&
            "Leading the team".equals(data.get("description")) &&
            "150000".equals(data.get("salary")) &&
            "year".equals(data.get("salary_unit")) &&
            Boolean.TRUE.equals(data.get("currently_works_here")) &&
            "2021-01-01".equals(data.get("start_date")) &&
            "2025-12-31".equals(data.get("end_date"))
        ));
    }

    @Test
    void updateOccupation_WithoutTitle_DoesNotValidate() {
        // Given - updating without title should be allowed
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("description", "New description only");

        when(monicaClient.put(eq("/occupations/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        Map<String, Object> result = occupationService.updateOccupation(arguments).block();

        // Then - should succeed
        assertNotNull(result);
        verify(monicaClient).put(eq("/occupations/1"), argThat(data ->
            "New description only".equals(data.get("description")) &&
            !data.containsKey("title")
        ));
    }

    // ========================================================================================
    // DELETE OCCUPATION TESTS
    // ========================================================================================

    @Test
    void deleteOccupation_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/occupations/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Occupation"), eq(1L), eq(true), anyString()
        )).thenReturn("Occupation with ID 1 has been deleted successfully");

        // When
        Map<String, Object> result = occupationService.deleteOccupation(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertTrue(content.get(0).get("text").toString().contains("deleted successfully"));

        verify(monicaClient).delete(eq("/occupations/1"));
    }

    @Test
    void deleteOccupation_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/occupations/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Occupation"), eq(99L), eq(true), anyString()
        )).thenReturn("Occupation with ID 99 has been deleted successfully");

        // When
        Map<String, Object> result = occupationService.deleteOccupation(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/occupations/99"));
    }

    @Test
    void deleteOccupation_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 55);
        Map<String, Object> deleteResponse = createDeleteResponse(55L);

        when(monicaClient.delete(eq("/occupations/55"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Occupation"), eq(55L), eq(true), anyString()
        )).thenReturn("Occupation with ID 55 has been deleted successfully");

        // When
        Map<String, Object> result = occupationService.deleteOccupation(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/occupations/55"));
    }

    @Test
    void deleteOccupation_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("title", "Developer");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            occupationService.deleteOccupation(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Occupation ID is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteOccupation_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            occupationService.deleteOccupation(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Occupation ID is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteOccupation_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            occupationService.deleteOccupation(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid occupation ID format"));
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST OCCUPATIONS TESTS
    // ========================================================================================

    @Test
    void listOccupations_WithPagination_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> occupations = List.of(
            occupationBuilder().id(1L).title("Developer").build(),
            occupationBuilder().id(2L).title("Designer").build()
        );
        Map<String, Object> listResponse = createListResponse(occupations, 2, 20, 50);

        when(monicaClient.get(eq("/occupations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = occupationService.listOccupations(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/occupations"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listOccupations_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> occupations = List.of(
            occupationBuilder().id(1L).title("Manager").build()
        );
        Map<String, Object> listResponse = createListResponse(occupations);

        when(monicaClient.get(eq("/occupations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        occupationService.listOccupations(arguments).block();

        // Then - verify default pagination values
        verify(monicaClient).get(eq("/occupations"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listOccupations_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> occupations = List.of(
            occupationBuilder().id(1L).title("Developer").build()
        );
        Map<String, Object> listResponse = createListResponse(occupations, 1, 10, 100);

        when(monicaClient.get(eq("/occupations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = occupationService.listOccupations(arguments).block();

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
    void listOccupations_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/occupations"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = occupationService.listOccupations(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listOccupations_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> occupations = List.of(
            occupationBuilder().id(1L).title("Developer").build()
        );
        Map<String, Object> listResponse = createListResponse(occupations);

        when(monicaClient.get(eq("/occupations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        occupationService.listOccupations(arguments).block();

        // Then
        verify(monicaClient).get(eq("/occupations"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listOccupations_StringPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("page", "3");

        List<Map<String, Object>> occupations = List.of(
            occupationBuilder().id(1L).title("Developer").build()
        );
        Map<String, Object> listResponse = createListResponse(occupations, 3, 10, 30);

        when(monicaClient.get(eq("/occupations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        occupationService.listOccupations(arguments).block();

        // Then
        verify(monicaClient).get(eq("/occupations"), argThat(params ->
            "3".equals(params.get("page"))
        ));
    }

    @Test
    void listOccupations_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> occupationWithSnakeCase = new HashMap<>();
        occupationWithSnakeCase.put("id", 1L);
        occupationWithSnakeCase.put("contact_id", 42L);
        occupationWithSnakeCase.put("company_id", 10L);
        occupationWithSnakeCase.put("title", "Software Engineer");
        occupationWithSnakeCase.put("salary_unit", "year");
        occupationWithSnakeCase.put("currently_works_here", true);
        occupationWithSnakeCase.put("start_date", "2022-01-01");
        occupationWithSnakeCase.put("end_date", "2024-12-31");
        occupationWithSnakeCase.put("created_at", "2024-01-15T10:00:00Z");
        occupationWithSnakeCase.put("updated_at", "2024-01-15T10:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(occupationWithSnakeCase));

        when(monicaClient.get(eq("/occupations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = occupationService.listOccupations(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify snake_case is mapped to camelCase
        assertEquals(42L, data.get(0).get("contactId"));
        assertEquals(10L, data.get(0).get("companyId"));
        assertEquals("year", data.get(0).get("salaryUnit"));
        assertEquals(true, data.get(0).get("currentlyWorksHere"));
        assertEquals("2022-01-01", data.get(0).get("startDate"));
        assertEquals("2024-12-31", data.get(0).get("endDate"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("updatedAt"));
        // These should remain unchanged
        assertEquals("Software Engineer", data.get(0).get("title"));
    }

    @Test
    void listOccupations_IntegerPageAndLimit_ConvertsToString() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 5,
            "limit", 50
        );

        List<Map<String, Object>> occupations = List.of(
            occupationBuilder().id(1L).title("Developer").build()
        );
        Map<String, Object> listResponse = createListResponse(occupations, 5, 50, 100);

        when(monicaClient.get(eq("/occupations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        occupationService.listOccupations(arguments).block();

        // Then
        verify(monicaClient).get(eq("/occupations"), argThat(params ->
            "5".equals(params.get("page")) &&
            "50".equals(params.get("limit"))
        ));
    }

    @Test
    void listOccupations_NoMetaInResponse_HandlesGracefully() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> occupations = List.of(
            occupationBuilder().id(1L).title("Developer").build()
        );
        // Response without meta
        Map<String, Object> listResponse = new HashMap<>();
        listResponse.put("data", occupations);

        when(monicaClient.get(eq("/occupations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = occupationService.listOccupations(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertFalse(result.containsKey("meta"));
    }

    @Test
    void listOccupations_MultipleOccupations_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> occupations = List.of(
            occupationBuilder().id(1L).title("Developer").currentlyWorksHere(true).build(),
            occupationBuilder().id(2L).title("Manager").currentlyWorksHere(true).build(),
            occupationBuilder().id(3L).title("Consultant").currentlyWorksHere(false).build()
        );
        Map<String, Object> listResponse = createListResponse(occupations, 1, 10, 3);

        when(monicaClient.get(eq("/occupations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = occupationService.listOccupations(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(3, data.size());

        assertEquals("Developer", data.get(0).get("title"));
        assertEquals(true, data.get(0).get("currentlyWorksHere"));
        assertEquals("Manager", data.get(1).get("title"));
        assertEquals(true, data.get(1).get("currentlyWorksHere"));
        assertEquals("Consultant", data.get(2).get("title"));
        assertEquals(false, data.get(2).get("currentlyWorksHere"));
    }

    // ========================================================================================
    // COMPANY LINKING TESTS
    // ========================================================================================

    @Test
    void createOccupation_WithCompanyLink_SetsCompanyIdCorrectly() {
        // Given - linking occupation to a company
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("title", "Software Engineer");
        arguments.put("companyId", 5L);

        when(monicaClient.post(eq("/occupations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.createOccupation(arguments).block();

        // Then
        verify(monicaClient).post(eq("/occupations"), argThat(data ->
            Long.valueOf(5L).equals(data.get("company_id"))
        ));
    }

    @Test
    void createOccupation_WithIntegerCompanyId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("title", "Developer");
        arguments.put("companyId", 15);

        when(monicaClient.post(eq("/occupations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.createOccupation(arguments).block();

        // Then
        verify(monicaClient).post(eq("/occupations"), argThat(data ->
            Integer.valueOf(15).equals(data.get("company_id"))
        ));
    }

    @Test
    void createOccupation_WithStringCompanyId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("title", "Developer");
        arguments.put("companyId", "25");

        when(monicaClient.post(eq("/occupations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.createOccupation(arguments).block();

        // Then
        verify(monicaClient).post(eq("/occupations"), argThat(data ->
            "25".equals(data.get("company_id").toString())
        ));
    }

    @Test
    void updateOccupation_ChangeCompany_UpdatesCorrectly() {
        // Given - changing company link
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("companyId", 20L);

        when(monicaClient.put(eq("/occupations/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.updateOccupation(arguments).block();

        // Then
        verify(monicaClient).put(eq("/occupations/1"), argThat(data ->
            Long.valueOf(20L).equals(data.get("company_id"))
        ));
    }

    @Test
    void getOccupation_WithCompanyData_MapsCompanyIdCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("contact_id", 42L);
        apiData.put("company_id", 10L);
        apiData.put("title", "Developer");
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T10:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/occupations/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = occupationService.getOccupation(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertEquals(10L, data.get("companyId"));
    }

    // ========================================================================================
    // SALARY HANDLING TESTS
    // ========================================================================================

    @Test
    void createOccupation_WithSalaryAsString_HandlesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("title", "Developer");
        arguments.put("salary", "85000.50");

        when(monicaClient.post(eq("/occupations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.createOccupation(arguments).block();

        // Then
        verify(monicaClient).post(eq("/occupations"), argThat(data ->
            "85000.50".equals(data.get("salary"))
        ));
    }

    @Test
    void createOccupation_WithSalaryAsInteger_HandlesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("title", "Developer");
        arguments.put("salary", 75000);

        when(monicaClient.post(eq("/occupations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.createOccupation(arguments).block();

        // Then
        verify(monicaClient).post(eq("/occupations"), argThat(data ->
            Integer.valueOf(75000).equals(data.get("salary"))
        ));
    }

    @Test
    void createOccupation_WithDifferentSalaryUnits_HandlesCorrectly() {
        // Given - test with "hour" salary unit
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("title", "Contractor");
        arguments.put("salary", "50");
        arguments.put("salaryUnit", "hour");

        when(monicaClient.post(eq("/occupations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.createOccupation(arguments).block();

        // Then
        verify(monicaClient).post(eq("/occupations"), argThat(data ->
            "50".equals(data.get("salary")) &&
            "hour".equals(data.get("salary_unit"))
        ));
    }

    // ========================================================================================
    // DATE HANDLING TESTS
    // ========================================================================================

    @Test
    void createOccupation_WithBothDates_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("title", "Former Developer");
        arguments.put("startDate", "2020-01-15");
        arguments.put("endDate", "2023-06-30");
        arguments.put("currentlyWorksHere", false);

        when(monicaClient.post(eq("/occupations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.createOccupation(arguments).block();

        // Then
        verify(monicaClient).post(eq("/occupations"), argThat(data ->
            "2020-01-15".equals(data.get("start_date")) &&
            "2023-06-30".equals(data.get("end_date")) &&
            Boolean.FALSE.equals(data.get("currently_works_here"))
        ));
    }

    @Test
    void createOccupation_WithStartDateOnly_MapsCorrectly() {
        // Given - currently employed, no end date
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("title", "Current Developer");
        arguments.put("startDate", "2022-03-01");
        arguments.put("currentlyWorksHere", true);

        when(monicaClient.post(eq("/occupations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted occupation JSON");

        // When
        occupationService.createOccupation(arguments).block();

        // Then
        verify(monicaClient).post(eq("/occupations"), argThat(data ->
            "2022-03-01".equals(data.get("start_date")) &&
            !data.containsKey("end_date") &&
            Boolean.TRUE.equals(data.get("currently_works_here"))
        ));
    }
}
