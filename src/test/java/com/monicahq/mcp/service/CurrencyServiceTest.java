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
 * Unit tests for CurrencyService covering getCurrency, listCurrencies,
 * searchCurrencies operations, validation, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class CurrencyServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    @InjectMocks
    private CurrencyService currencyService;

    private Map<String, Object> mockCurrencyData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        mockCurrencyData = currencyBuilder()
            .id(1L)
            .code("USD")
            .name("US Dollar")
            .symbol("$")
            .exchangeRate(1.0)
            .build();

        mockApiResponse = createSingleEntityResponse(mockCurrencyData);
    }

    // ========================================================================================
    // GET CURRENCY TESTS
    // ========================================================================================

    @Test
    void getCurrency_ValidLongId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);

        when(monicaClient.get(eq("/currencies/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted currency JSON");

        // When
        Map<String, Object> result = currencyService.getCurrency(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted currency JSON", content.get(0).get("text"));

        verify(monicaClient).get(eq("/currencies/1"), isNull());
    }

    @Test
    void getCurrency_ValidIntegerId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 42);

        Map<String, Object> currencyData = currencyBuilder()
            .id(42L)
            .code("EUR")
            .name("Euro")
            .symbol("€")
            .build();
        Map<String, Object> apiResponse = createSingleEntityResponse(currencyData);

        when(monicaClient.get(eq("/currencies/42"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted currency JSON");

        // When
        Map<String, Object> result = currencyService.getCurrency(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/currencies/42"), isNull());
    }

    @Test
    void getCurrency_ValidStringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "123");

        Map<String, Object> currencyData = currencyBuilder()
            .id(123L)
            .code("GBP")
            .name("British Pound")
            .symbol("£")
            .build();
        Map<String, Object> apiResponse = createSingleEntityResponse(currencyData);

        when(monicaClient.get(eq("/currencies/123"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted currency JSON");

        // When
        Map<String, Object> result = currencyService.getCurrency(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/currencies/123"), isNull());
    }

    @Test
    void getCurrency_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            currencyService.getCurrency(arguments).block();
        });
        assertEquals("id is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getCurrency_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            currencyService.getCurrency(arguments).block();
        });
        assertEquals("id is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getCurrency_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            currencyService.getCurrency(arguments).block();
        });
        assertEquals("id must be a valid number", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getCurrency_DirectResponseWithoutDataWrapper_HandlesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);

        // API returns currency directly without data wrapper
        Map<String, Object> directResponse = currencyBuilder()
            .id(1L)
            .code("JPY")
            .name("Japanese Yen")
            .symbol("¥")
            .build();

        when(monicaClient.get(eq("/currencies/1"), any())).thenReturn(Mono.just(directResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted currency JSON");

        // When
        Map<String, Object> result = currencyService.getCurrency(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        verify(monicaClient).get(eq("/currencies/1"), isNull());
    }

    @Test
    void getCurrency_ResponseFieldMapping_MapsSnakeCaseToCamelCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);

        Map<String, Object> currencyData = new HashMap<>();
        currencyData.put("id", 1L);
        currencyData.put("code", "USD");
        currencyData.put("name", "US Dollar");
        currencyData.put("exchange_rate", 1.0);
        currencyData.put("created_at", "2023-01-01T00:00:00Z");
        currencyData.put("updated_at", "2023-01-02T00:00:00Z");
        Map<String, Object> apiResponse = createSingleEntityResponse(currencyData);

        when(monicaClient.get(eq("/currencies/1"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted currency JSON");

        // When
        Map<String, Object> result = currencyService.getCurrency(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertEquals("USD", data.get("code"));
        assertEquals("US Dollar", data.get("name"));
        assertEquals(1.0, data.get("exchangeRate")); // exchange_rate -> exchangeRate
        assertEquals("2023-01-01T00:00:00Z", data.get("createdAt")); // created_at -> createdAt
        assertEquals("2023-01-02T00:00:00Z", data.get("updatedAt")); // updated_at -> updatedAt
    }

    // ========================================================================================
    // LIST CURRENCIES TESTS
    // ========================================================================================

    @Test
    void listCurrencies_DefaultPagination_UsesDefaultValues() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> currencies = List.of(
            currencyBuilder().id(1L).code("USD").name("US Dollar").build(),
            currencyBuilder().id(2L).code("EUR").name("Euro").build()
        );
        Map<String, Object> apiResponse = createListResponse(currencies, 1, 50, 2);

        when(monicaClient.get(eq("/currencies"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted currencies JSON");

        // When
        Map<String, Object> result = currencyService.listCurrencies(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/currencies"), argThat(params ->
            "1".equals(params.get("page")) && "50".equals(params.get("limit"))
        ));
    }

    @Test
    void listCurrencies_CustomPagination_UsesProvidedValues() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("page", 2);
        arguments.put("limit", 25);

        List<Map<String, Object>> currencies = List.of(
            currencyBuilder().id(3L).code("GBP").name("British Pound").build()
        );
        Map<String, Object> apiResponse = createListResponse(currencies, 2, 25, 26);

        when(monicaClient.get(eq("/currencies"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted currencies JSON");

        // When
        Map<String, Object> result = currencyService.listCurrencies(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/currencies"), argThat(params ->
            "2".equals(params.get("page")) && "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listCurrencies_StringPagination_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("page", "3");
        arguments.put("limit", "10");

        List<Map<String, Object>> currencies = List.of(
            currencyBuilder().id(1L).code("CHF").name("Swiss Franc").build()
        );
        Map<String, Object> apiResponse = createListResponse(currencies, 3, 10, 21);

        when(monicaClient.get(eq("/currencies"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted currencies JSON");

        // When
        Map<String, Object> result = currencyService.listCurrencies(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/currencies"), argThat(params ->
            "3".equals(params.get("page")) && "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listCurrencies_EmptyResult_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> currencies = List.of();
        Map<String, Object> apiResponse = createListResponse(currencies, 1, 50, 0);

        when(monicaClient.get(eq("/currencies"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = currencyService.listCurrencies(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listCurrencies_IncludesMetadata_WhenPresent() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> currencies = List.of(
            currencyBuilder().id(1L).code("USD").build(),
            currencyBuilder().id(2L).code("EUR").build()
        );
        Map<String, Object> apiResponse = createListResponse(currencies, 1, 50, 100);

        when(monicaClient.get(eq("/currencies"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted currencies JSON");

        // When
        Map<String, Object> result = currencyService.listCurrencies(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("meta"));
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) result.get("meta");
        assertEquals(1, meta.get("current_page"));
        assertEquals(50, meta.get("per_page"));
        assertEquals(100, meta.get("total"));
    }

    @Test
    void listCurrencies_MultipleItems_MapsAllFields() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> currency1 = new HashMap<>();
        currency1.put("id", 1L);
        currency1.put("code", "USD");
        currency1.put("name", "US Dollar");
        currency1.put("exchange_rate", 1.0);

        Map<String, Object> currency2 = new HashMap<>();
        currency2.put("id", 2L);
        currency2.put("code", "EUR");
        currency2.put("name", "Euro");
        currency2.put("exchange_rate", 0.85);

        List<Map<String, Object>> currencies = List.of(currency1, currency2);
        Map<String, Object> apiResponse = createListResponse(currencies, 1, 50, 2);

        when(monicaClient.get(eq("/currencies"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted currencies JSON");

        // When
        Map<String, Object> result = currencyService.listCurrencies(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());
        assertEquals("USD", data.get(0).get("code"));
        assertEquals(1.0, data.get(0).get("exchangeRate")); // exchange_rate -> exchangeRate
        assertEquals("EUR", data.get(1).get("code"));
        assertEquals(0.85, data.get(1).get("exchangeRate"));
    }

    @Test
    void listCurrencies_ContentFieldFormat_IsCorrect() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> currencies = List.of(
            currencyBuilder().id(1L).code("USD").build()
        );
        Map<String, Object> apiResponse = createListResponse(currencies, 1, 50, 1);

        when(monicaClient.get(eq("/currencies"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list content");

        // When
        Map<String, Object> result = currencyService.listCurrencies(arguments).block();

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
    // SEARCH CURRENCIES TESTS
    // ========================================================================================

    @Test
    void searchCurrencies_WithSearchParam_IncludesInQuery() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("search", "dollar");

        List<Map<String, Object>> currencies = List.of(
            currencyBuilder().id(1L).code("USD").name("US Dollar").build(),
            currencyBuilder().id(3L).code("AUD").name("Australian Dollar").build()
        );
        Map<String, Object> apiResponse = createListResponse(currencies, 1, 50, 2);

        when(monicaClient.get(eq("/currencies"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted currencies JSON");

        // When
        Map<String, Object> result = currencyService.searchCurrencies(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/currencies"), argThat(params ->
            "dollar".equals(params.get("search")) && "1".equals(params.get("page")) && "50".equals(params.get("limit"))
        ));
    }

    @Test
    void searchCurrencies_WithPagination_CombinesParams() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("search", "euro");
        arguments.put("page", 1);
        arguments.put("limit", 10);

        List<Map<String, Object>> currencies = List.of(
            currencyBuilder().id(2L).code("EUR").name("Euro").build()
        );
        Map<String, Object> apiResponse = createListResponse(currencies, 1, 10, 1);

        when(monicaClient.get(eq("/currencies"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted currencies JSON");

        // When
        Map<String, Object> result = currencyService.searchCurrencies(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/currencies"), argThat(params ->
            "euro".equals(params.get("search")) && "1".equals(params.get("page")) && "10".equals(params.get("limit"))
        ));
    }

    @Test
    void searchCurrencies_NoSearchParam_UsesDefaultPagination() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> currencies = List.of(
            currencyBuilder().id(1L).code("USD").build()
        );
        Map<String, Object> apiResponse = createListResponse(currencies, 1, 50, 1);

        when(monicaClient.get(eq("/currencies"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted currencies JSON");

        // When
        Map<String, Object> result = currencyService.searchCurrencies(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/currencies"), argThat(params ->
            !params.containsKey("search") && "1".equals(params.get("page")) && "50".equals(params.get("limit"))
        ));
    }

    @Test
    void searchCurrencies_EmptyResult_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("search", "nonexistent");

        List<Map<String, Object>> currencies = List.of();
        Map<String, Object> apiResponse = createListResponse(currencies, 1, 50, 0);

        when(monicaClient.get(eq("/currencies"), any())).thenReturn(Mono.just(apiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = currencyService.searchCurrencies(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    // ========================================================================================
    // CURRENCY DATA BUILDER
    // ========================================================================================

    /**
     * Builder for creating mock currency data.
     */
    protected CurrencyDataBuilder currencyBuilder() {
        return new CurrencyDataBuilder();
    }

    public static class CurrencyDataBuilder {
        private final Map<String, Object> data = new HashMap<>();

        public CurrencyDataBuilder() {
            // Set defaults
            data.put("id", 1L);
            data.put("code", "USD");
            data.put("name", "US Dollar");
            data.put("symbol", "$");
            data.put("exchange_rate", 1.0);
        }

        public CurrencyDataBuilder id(Long id) {
            data.put("id", id);
            return this;
        }

        public CurrencyDataBuilder code(String code) {
            data.put("code", code);
            return this;
        }

        public CurrencyDataBuilder name(String name) {
            data.put("name", name);
            return this;
        }

        public CurrencyDataBuilder symbol(String symbol) {
            data.put("symbol", symbol);
            return this;
        }

        public CurrencyDataBuilder exchangeRate(Double exchangeRate) {
            data.put("exchange_rate", exchangeRate);
            return this;
        }

        public CurrencyDataBuilder custom(String key, Object value) {
            data.put(key, value);
            return this;
        }

        public Map<String, Object> build() {
            return new HashMap<>(data);
        }
    }
}
