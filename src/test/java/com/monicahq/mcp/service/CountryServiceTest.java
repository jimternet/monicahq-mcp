package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.CountryFieldMappingConfig;
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
 * Unit tests for CountryService covering getCountry, listCountries,
 * searchCountries operations, validation, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class CountryServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private CountryService countryService;

    private Map<String, Object> mockCountryData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        CountryFieldMappingConfig fieldMappingConfig = new CountryFieldMappingConfig();
        countryService = new CountryService(monicaClient, contentFormatter, fieldMappingConfig);

        mockCountryData = countryBuilder()
            .id(1L)
            .name("United States")
            .countryCode("US")
            .build();

        mockApiResponse = createSingleEntityResponse(mockCountryData);
    }

    // ========================================================================================
    // GET COUNTRY TESTS
    // ========================================================================================

    @Test
    void getCountry_ValidLongId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);

        when(monicaClient.get(eq("/countries/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted country JSON");

        // When
        Map<String, Object> result = countryService.getCountry(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted country JSON", content.get(0).get("text"));

        verify(monicaClient).get(eq("/countries/1"), isNull());
    }

    @Test
    void getCountry_ValidIntegerId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 42);

        Map<String, Object> countryData = countryBuilder()
            .id(42L)
            .name("Canada")
            .countryCode("CA")
            .build();
        Map<String, Object> apiResponse = createSingleEntityResponse(countryData);

        when(monicaClient.get(eq("/countries/42"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted country JSON");

        // When
        Map<String, Object> result = countryService.getCountry(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/countries/42"), isNull());
    }

    @Test
    void getCountry_ValidStringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "123");

        Map<String, Object> countryData = countryBuilder()
            .id(123L)
            .name("United Kingdom")
            .countryCode("GB")
            .build();
        Map<String, Object> apiResponse = createSingleEntityResponse(countryData);

        when(monicaClient.get(eq("/countries/123"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted country JSON");

        // When
        Map<String, Object> result = countryService.getCountry(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/countries/123"), isNull());
    }

    @Test
    void getCountry_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            countryService.getCountry(arguments).block();
        });
        assertEquals("Country ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getCountry_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            countryService.getCountry(arguments).block();
        });
        assertEquals("Country ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getCountry_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            countryService.getCountry(arguments).block();
        });
        assertTrue(exception.getMessage().startsWith("Invalid country ID format:"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getCountry_DirectResponseWithoutDataWrapper_HandlesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);

        // API returns country directly without data wrapper
        Map<String, Object> directResponse = countryBuilder()
            .id(1L)
            .name("Japan")
            .countryCode("JP")
            .build();

        when(monicaClient.get(eq("/countries/1"), any())).thenReturn(Mono.just(directResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted country JSON");

        // When
        Map<String, Object> result = countryService.getCountry(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        verify(monicaClient).get(eq("/countries/1"), isNull());
    }

    @Test
    void getCountry_ResponseFieldMapping_MapsSnakeCaseToCamelCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);

        Map<String, Object> countryData = new HashMap<>();
        countryData.put("id", 1L);
        countryData.put("name", "United States");
        countryData.put("country_code", "US");
        countryData.put("created_at", "2023-01-01T00:00:00Z");
        countryData.put("updated_at", "2023-01-02T00:00:00Z");
        Map<String, Object> apiResponse = createSingleEntityResponse(countryData);

        when(monicaClient.get(eq("/countries/1"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted country JSON");

        // When
        Map<String, Object> result = countryService.getCountry(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertEquals("United States", data.get("name"));
        assertEquals("US", data.get("countryCode")); // country_code -> countryCode
        assertEquals("2023-01-01T00:00:00Z", data.get("createdAt")); // created_at -> createdAt
        assertEquals("2023-01-02T00:00:00Z", data.get("updatedAt")); // updated_at -> updatedAt
    }

    // ========================================================================================
    // LIST COUNTRIES TESTS
    // ========================================================================================

    @Test
    void listCountries_DefaultPagination_UsesDefaultValues() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> countries = List.of(
            countryBuilder().id(1L).name("United States").countryCode("US").build(),
            countryBuilder().id(2L).name("Canada").countryCode("CA").build()
        );
        Map<String, Object> apiResponse = createListResponse(countries, 1, 50, 2);

        when(monicaClient.get(eq("/countries"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted countries JSON");

        // When
        Map<String, Object> result = countryService.listCountries(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/countries"), argThat(params ->
            "1".equals(params.get("page")) && "50".equals(params.get("limit"))
        ));
    }

    @Test
    void listCountries_CustomPagination_UsesProvidedValues() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("page", 2);
        arguments.put("limit", 25);

        List<Map<String, Object>> countries = List.of(
            countryBuilder().id(3L).name("United Kingdom").countryCode("GB").build()
        );
        Map<String, Object> apiResponse = createListResponse(countries, 2, 25, 26);

        when(monicaClient.get(eq("/countries"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted countries JSON");

        // When
        Map<String, Object> result = countryService.listCountries(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/countries"), argThat(params ->
            "2".equals(params.get("page")) && "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listCountries_StringPagination_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("page", "3");
        arguments.put("limit", "10");

        List<Map<String, Object>> countries = List.of(
            countryBuilder().id(1L).name("Germany").countryCode("DE").build()
        );
        Map<String, Object> apiResponse = createListResponse(countries, 3, 10, 21);

        when(monicaClient.get(eq("/countries"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted countries JSON");

        // When
        Map<String, Object> result = countryService.listCountries(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/countries"), argThat(params ->
            "3".equals(params.get("page")) && "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listCountries_EmptyResult_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> countries = List.of();
        Map<String, Object> apiResponse = createListResponse(countries, 1, 50, 0);

        when(monicaClient.get(eq("/countries"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = countryService.listCountries(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listCountries_IncludesMetadata_WhenPresent() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> countries = List.of(
            countryBuilder().id(1L).name("United States").build(),
            countryBuilder().id(2L).name("Canada").build()
        );
        Map<String, Object> apiResponse = createListResponse(countries, 1, 50, 195);

        when(monicaClient.get(eq("/countries"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted countries JSON");

        // When
        Map<String, Object> result = countryService.listCountries(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("meta"));
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) result.get("meta");
        assertEquals(1, meta.get("current_page"));
        assertEquals(50, meta.get("per_page"));
        assertEquals(195, meta.get("total"));
    }

    @Test
    void listCountries_MultipleItems_MapsAllFields() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> country1 = new HashMap<>();
        country1.put("id", 1L);
        country1.put("name", "United States");
        country1.put("country_code", "US");

        Map<String, Object> country2 = new HashMap<>();
        country2.put("id", 2L);
        country2.put("name", "Canada");
        country2.put("country_code", "CA");

        List<Map<String, Object>> countries = List.of(country1, country2);
        Map<String, Object> apiResponse = createListResponse(countries, 1, 50, 2);

        when(monicaClient.get(eq("/countries"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted countries JSON");

        // When
        Map<String, Object> result = countryService.listCountries(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());
        assertEquals("United States", data.get(0).get("name"));
        assertEquals("US", data.get(0).get("countryCode")); // country_code -> countryCode
        assertEquals("Canada", data.get(1).get("name"));
        assertEquals("CA", data.get(1).get("countryCode"));
    }

    @Test
    void listCountries_ContentFieldFormat_IsCorrect() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> countries = List.of(
            countryBuilder().id(1L).name("United States").build()
        );
        Map<String, Object> apiResponse = createListResponse(countries, 1, 50, 1);

        when(monicaClient.get(eq("/countries"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list content");

        // When
        Map<String, Object> result = countryService.listCountries(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted list content", content.get(0).get("text"));
    }

    // ========================================================================================
    // SEARCH COUNTRIES TESTS
    // ========================================================================================

    @Test
    void searchCountries_WithSearchParam_IncludesInQuery() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("search", "united");

        List<Map<String, Object>> countries = List.of(
            countryBuilder().id(1L).name("United States").countryCode("US").build(),
            countryBuilder().id(2L).name("United Kingdom").countryCode("GB").build()
        );
        Map<String, Object> apiResponse = createListResponse(countries, 1, 50, 2);

        when(monicaClient.get(eq("/countries"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted countries JSON");

        // When
        Map<String, Object> result = countryService.searchCountries(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/countries"), argThat(params ->
            "united".equals(params.get("search")) && "1".equals(params.get("page")) && "50".equals(params.get("limit"))
        ));
    }

    @Test
    void searchCountries_WithPagination_CombinesParams() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("search", "germany");
        arguments.put("page", 1);
        arguments.put("limit", 10);

        List<Map<String, Object>> countries = List.of(
            countryBuilder().id(3L).name("Germany").countryCode("DE").build()
        );
        Map<String, Object> apiResponse = createListResponse(countries, 1, 10, 1);

        when(monicaClient.get(eq("/countries"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted countries JSON");

        // When
        Map<String, Object> result = countryService.searchCountries(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/countries"), argThat(params ->
            "germany".equals(params.get("search")) && "1".equals(params.get("page")) && "10".equals(params.get("limit"))
        ));
    }

    @Test
    void searchCountries_NoSearchParam_UsesDefaultPagination() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> countries = List.of(
            countryBuilder().id(1L).name("United States").build()
        );
        Map<String, Object> apiResponse = createListResponse(countries, 1, 50, 1);

        when(monicaClient.get(eq("/countries"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted countries JSON");

        // When
        Map<String, Object> result = countryService.searchCountries(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/countries"), argThat(params ->
            !params.containsKey("search") && "1".equals(params.get("page")) && "50".equals(params.get("limit"))
        ));
    }

    @Test
    void searchCountries_EmptyResult_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("search", "nonexistent");

        List<Map<String, Object>> countries = List.of();
        Map<String, Object> apiResponse = createListResponse(countries, 1, 50, 0);

        when(monicaClient.get(eq("/countries"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = countryService.searchCountries(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    // ========================================================================================
    // COUNTRY DATA BUILDER
    // ========================================================================================

    /**
     * Builder for creating mock country data.
     */
    protected CountryDataBuilder countryBuilder() {
        return new CountryDataBuilder();
    }

    public static class CountryDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public CountryDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("name", "United States");
            data.put("country_code", "US");
        }

        public CountryDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public CountryDataBuilder name(String name) {
            data.put("name", name);
            return this;
        }

        public CountryDataBuilder countryCode(String countryCode) {
            data.put("country_code", countryCode);
            return this;
        }

        public CountryDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }
}
