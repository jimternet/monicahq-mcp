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
 * Unit tests for CompanyService covering CRUD operations,
 * field mapping, validation, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class CompanyServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    @InjectMocks
    private CompanyService companyService;

    private Map<String, Object> mockCompanyData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        mockCompanyData = companyBuilder()
            .id(1L)
            .name("Acme Corporation")
            .website("https://acme.com")
            .numberOfEmployees(100)
            .build();

        mockApiResponse = createSingleEntityResponse(mockCompanyData);
    }

    // ========================================================================================
    // CREATE COMPANY TESTS
    // ========================================================================================

    @Test
    void createCompany_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "New Company");
        arguments.put("website", "https://newcompany.com");

        when(monicaClient.post(eq("/companies"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted company JSON");

        // When
        Map<String, Object> result = companyService.createCompany(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted company JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/companies"), argThat(data ->
            "New Company".equals(data.get("name"))
        ));
    }

    @Test
    void createCompany_MissingName_ThrowsException() {
        // Given - has other field but no name
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("website", "https://example.com");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            companyService.createCompany(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createCompany_NullName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            companyService.createCompany(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createCompany_EmptyName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            companyService.createCompany(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createCompany_WhitespaceName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "   ");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            companyService.createCompany(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createCompany_WithNumberOfEmployees_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Tech Corp");
        arguments.put("numberOfEmployees", 250);

        when(monicaClient.post(eq("/companies"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted company JSON");

        // When
        companyService.createCompany(arguments).block();

        // Then - verify numberOfEmployees is mapped to number_of_employees
        verify(monicaClient).post(eq("/companies"), argThat(data ->
            Integer.valueOf(250).equals(data.get("number_of_employees")) &&
            !data.containsKey("numberOfEmployees")
        ));
    }

    @Test
    void createCompany_WithWebsite_IncludesWebsiteField() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Web Corp");
        arguments.put("website", "https://webcorp.io");

        when(monicaClient.post(eq("/companies"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted company JSON");

        // When
        companyService.createCompany(arguments).block();

        // Then
        verify(monicaClient).post(eq("/companies"), argThat(data ->
            "Web Corp".equals(data.get("name")) &&
            "https://webcorp.io".equals(data.get("website"))
        ));
    }

    @Test
    void createCompany_WithAllFields_MapsAllFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Full Corp");
        arguments.put("website", "https://fullcorp.com");
        arguments.put("numberOfEmployees", 500);

        when(monicaClient.post(eq("/companies"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted company JSON");

        // When
        companyService.createCompany(arguments).block();

        // Then
        verify(monicaClient).post(eq("/companies"), argThat(data ->
            "Full Corp".equals(data.get("name")) &&
            "https://fullcorp.com".equals(data.get("website")) &&
            Integer.valueOf(500).equals(data.get("number_of_employees"))
        ));
    }

    // ========================================================================================
    // GET COMPANY TESTS
    // ========================================================================================

    @Test
    void getCompany_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/companies/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted company JSON");

        // When
        Map<String, Object> result = companyService.getCompany(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/companies/1"), any());
    }

    @Test
    void getCompany_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            companyBuilder().id(42L).name("Test Company").build()
        );

        when(monicaClient.get(eq("/companies/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted company JSON");

        // When
        Map<String, Object> result = companyService.getCompany(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/companies/42"), any());
    }

    @Test
    void getCompany_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            companyBuilder().id(123L).name("Test Company").build()
        );

        when(monicaClient.get(eq("/companies/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted company JSON");

        // When
        Map<String, Object> result = companyService.getCompany(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/companies/123"), any());
    }

    @Test
    void getCompany_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("name", "Test");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            companyService.getCompany(arguments).block();
        });
        assertEquals("id is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getCompany_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            companyService.getCompany(arguments).block();
        });
        assertEquals("id is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getCompany_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            companyService.getCompany(arguments).block();
        });
        assertEquals("id must be a valid number", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getCompany_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("name", "API Corp");
        apiData.put("website", "https://apicorp.com");
        apiData.put("number_of_employees", 75);
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T09:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/companies/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = companyService.getCompany(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals(75, data.get("numberOfEmployees"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T09:00:00Z", data.get("updatedAt"));
        // These should remain unchanged
        assertEquals(1L, data.get("id"));
        assertEquals("API Corp", data.get("name"));
        assertEquals("https://apicorp.com", data.get("website"));
    }

    @Test
    void getCompany_DirectResponseWithoutDataWrapper_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        // API response without "data" wrapper
        Map<String, Object> directResponse = new HashMap<>();
        directResponse.put("id", 1L);
        directResponse.put("name", "Direct Corp");
        directResponse.put("number_of_employees", 30);
        directResponse.put("created_at", "2024-01-20T10:00:00Z");
        directResponse.put("updated_at", "2024-01-20T10:00:00Z");

        when(monicaClient.get(eq("/companies/1"), any())).thenReturn(Mono.just(directResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = companyService.getCompany(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        assertEquals("Direct Corp", data.get("name"));
        assertEquals(30, data.get("numberOfEmployees"));
    }

    // ========================================================================================
    // UPDATE COMPANY TESTS
    // ========================================================================================

    @Test
    void updateCompany_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("name", "Updated Company Name");

        when(monicaClient.put(eq("/companies/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted company JSON");

        // When
        Map<String, Object> result = companyService.updateCompany(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/companies/1"), argThat(data ->
            "Updated Company Name".equals(data.get("name"))
        ));
    }

    @Test
    void updateCompany_RemovesIdFromUpdateData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 5L);
        arguments.put("name", "Updated Company");

        when(monicaClient.put(eq("/companies/5"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted company JSON");

        // When
        companyService.updateCompany(arguments).block();

        // Then - verify that id is NOT included in the request body
        verify(monicaClient).put(eq("/companies/5"), argThat(data ->
            !data.containsKey("id")
        ));
    }

    @Test
    void updateCompany_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("name", "Updated Company");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            companyService.updateCompany(arguments).block();
        });
        assertEquals("id is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateCompany_MissingName_ThrowsException() {
        // Given - update requires name
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("website", "https://updated.com");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            companyService.updateCompany(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateCompany_WithNumberOfEmployees_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("name", "Updated Corp");
        arguments.put("numberOfEmployees", 300);

        when(monicaClient.put(eq("/companies/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted company JSON");

        // When
        companyService.updateCompany(arguments).block();

        // Then - verify numberOfEmployees is mapped to number_of_employees
        verify(monicaClient).put(eq("/companies/1"), argThat(data ->
            Integer.valueOf(300).equals(data.get("number_of_employees")) &&
            !data.containsKey("numberOfEmployees")
        ));
    }

    @Test
    void updateCompany_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "42");
        arguments.put("name", "Updated Company");

        when(monicaClient.put(eq("/companies/42"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted company JSON");

        // When
        companyService.updateCompany(arguments).block();

        // Then
        verify(monicaClient).put(eq("/companies/42"), any());
    }

    @Test
    void updateCompany_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");
        arguments.put("name", "Updated Company");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            companyService.updateCompany(arguments).block();
        });
        assertEquals("id must be a valid number", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateCompany_WithAllFields_MapsAllFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("name", "Complete Update Corp");
        arguments.put("website", "https://complete.com");
        arguments.put("numberOfEmployees", 999);

        when(monicaClient.put(eq("/companies/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted company JSON");

        // When
        companyService.updateCompany(arguments).block();

        // Then
        verify(monicaClient).put(eq("/companies/1"), argThat(data ->
            "Complete Update Corp".equals(data.get("name")) &&
            "https://complete.com".equals(data.get("website")) &&
            Integer.valueOf(999).equals(data.get("number_of_employees"))
        ));
    }

    // ========================================================================================
    // DELETE COMPANY TESTS
    // ========================================================================================

    @Test
    void deleteCompany_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/companies/1"))).thenReturn(Mono.just(deleteResponse));

        // When
        Map<String, Object> result = companyService.deleteCompany(arguments).block();

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

        verify(monicaClient).delete(eq("/companies/1"));
    }

    @Test
    void deleteCompany_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/companies/99"))).thenReturn(Mono.just(deleteResponse));

        // When
        Map<String, Object> result = companyService.deleteCompany(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/companies/99"));
    }

    @Test
    void deleteCompany_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 55);
        Map<String, Object> deleteResponse = createDeleteResponse(55L);

        when(monicaClient.delete(eq("/companies/55"))).thenReturn(Mono.just(deleteResponse));

        // When
        Map<String, Object> result = companyService.deleteCompany(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/companies/55"));
    }

    @Test
    void deleteCompany_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("name", "Test");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            companyService.deleteCompany(arguments).block();
        });
        assertEquals("id is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteCompany_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            companyService.deleteCompany(arguments).block();
        });
        assertEquals("id is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteCompany_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            companyService.deleteCompany(arguments).block();
        });
        assertEquals("id must be a valid number", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST COMPANIES TESTS
    // ========================================================================================

    @Test
    void listCompanies_WithPagination_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> companies = List.of(
            companyBuilder().id(1L).name("Company A").build(),
            companyBuilder().id(2L).name("Company B").build()
        );
        Map<String, Object> listResponse = createListResponse(companies, 2, 20, 50);

        when(monicaClient.get(eq("/companies"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = companyService.listCompanies(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/companies"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listCompanies_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> companies = List.of(
            companyBuilder().id(1L).name("Company A").build()
        );
        Map<String, Object> listResponse = createListResponse(companies);

        when(monicaClient.get(eq("/companies"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        companyService.listCompanies(arguments).block();

        // Then - verify default pagination values
        verify(monicaClient).get(eq("/companies"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listCompanies_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> companies = List.of(
            companyBuilder().id(1L).name("Company 1").build()
        );
        Map<String, Object> listResponse = createListResponse(companies, 1, 10, 100);

        when(monicaClient.get(eq("/companies"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = companyService.listCompanies(arguments).block();

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
    void listCompanies_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/companies"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = companyService.listCompanies(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listCompanies_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> companies = List.of(
            companyBuilder().id(1L).name("Company 1").build()
        );
        Map<String, Object> listResponse = createListResponse(companies);

        when(monicaClient.get(eq("/companies"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        companyService.listCompanies(arguments).block();

        // Then
        verify(monicaClient).get(eq("/companies"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listCompanies_StringPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("page", "3");

        List<Map<String, Object>> companies = List.of(
            companyBuilder().id(1L).name("Company 1").build()
        );
        Map<String, Object> listResponse = createListResponse(companies, 3, 10, 30);

        when(monicaClient.get(eq("/companies"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        companyService.listCompanies(arguments).block();

        // Then
        verify(monicaClient).get(eq("/companies"), argThat(params ->
            "3".equals(params.get("page"))
        ));
    }

    @Test
    void listCompanies_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> companyWithSnakeCase = new HashMap<>();
        companyWithSnakeCase.put("id", 1L);
        companyWithSnakeCase.put("name", "Snake Corp");
        companyWithSnakeCase.put("website", "https://snakecorp.com");
        companyWithSnakeCase.put("number_of_employees", 150);
        companyWithSnakeCase.put("created_at", "2024-01-15T10:00:00Z");
        companyWithSnakeCase.put("updated_at", "2024-01-15T10:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(companyWithSnakeCase));

        when(monicaClient.get(eq("/companies"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = companyService.listCompanies(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify snake_case is mapped to camelCase
        assertEquals(150, data.get(0).get("numberOfEmployees"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("updatedAt"));
        // These should remain unchanged
        assertEquals("Snake Corp", data.get(0).get("name"));
        assertEquals("https://snakecorp.com", data.get(0).get("website"));
    }

    @Test
    void listCompanies_IntegerPageAndLimit_ConvertsToString() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 5,
            "limit", 50
        );

        List<Map<String, Object>> companies = List.of(
            companyBuilder().id(1L).name("Company 1").build()
        );
        Map<String, Object> listResponse = createListResponse(companies, 5, 50, 100);

        when(monicaClient.get(eq("/companies"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        companyService.listCompanies(arguments).block();

        // Then
        verify(monicaClient).get(eq("/companies"), argThat(params ->
            "5".equals(params.get("page")) &&
            "50".equals(params.get("limit"))
        ));
    }

    @Test
    void listCompanies_NoMetaInResponse_HandlesGracefully() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> companies = List.of(
            companyBuilder().id(1L).name("Company 1").build()
        );
        // Response without meta
        Map<String, Object> listResponse = new HashMap<>();
        listResponse.put("data", companies);

        when(monicaClient.get(eq("/companies"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = companyService.listCompanies(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertFalse(result.containsKey("meta"));
    }

    @Test
    void listCompanies_MultipleCompanies_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> companies = List.of(
            companyBuilder().id(1L).name("Company A").numberOfEmployees(10).build(),
            companyBuilder().id(2L).name("Company B").numberOfEmployees(20).build(),
            companyBuilder().id(3L).name("Company C").numberOfEmployees(30).build()
        );
        Map<String, Object> listResponse = createListResponse(companies, 1, 10, 3);

        when(monicaClient.get(eq("/companies"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = companyService.listCompanies(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(3, data.size());

        assertEquals("Company A", data.get(0).get("name"));
        assertEquals("Company B", data.get(1).get("name"));
        assertEquals("Company C", data.get(2).get("name"));
    }
}
