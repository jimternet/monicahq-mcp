package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.DebtFieldMappingConfig;
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
 * Unit tests for DebtService covering CRUD operations,
 * debt tracking, amount validation, direction handling, field mapping, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class DebtServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private DebtService debtService;

    private Map<String, Object> mockDebtData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        // Create DebtService with real DebtFieldMappingConfig (no dependencies to mock)
        DebtFieldMappingConfig fieldMappingConfig = new DebtFieldMappingConfig();
        debtService = new DebtService(monicaClient, contentFormatter, fieldMappingConfig);

        mockDebtData = debtBuilder()
            .id(1L)
            .contactId(42L)
            .amount("100.00")
            .inDebt("yes")
            .status("inprogress")
            .build();

        mockApiResponse = createSingleEntityResponse(mockDebtData);
    }

    // ========================================================================================
    // CREATE DEBT TESTS
    // ========================================================================================

    @Test
    void createDebt_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("amount", "100.00");
        arguments.put("inDebt", "yes");
        arguments.put("status", "inprogress");

        when(monicaClient.post(eq("/debts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        Map<String, Object> result = debtService.createDebt(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted debt JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/debts"), argThat(data ->
            Long.valueOf(42L).equals(data.get("contact_id")) &&
            "100.00".equals(data.get("amount"))
        ));
    }

    @Test
    void createDebt_MissingContactId_ThrowsException() {
        // Given - has other field but no contactId
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("amount", "50.00");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            debtService.createDebt(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createDebt_NullContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", null);
        arguments.put("amount", "50.00");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            debtService.createDebt(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createDebt_MissingAmount_ThrowsException() {
        // Given - has contactId but no amount
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            debtService.createDebt(arguments).block();
        });
        assertEquals("amount is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createDebt_NullAmount_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("amount", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            debtService.createDebt(arguments).block();
        });
        assertEquals("amount is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createDebt_WithCurrency_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("amount", "250.00");
        arguments.put("currency", "USD");

        when(monicaClient.post(eq("/debts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.createDebt(arguments).block();

        // Then - verify currency is passed through
        verify(monicaClient).post(eq("/debts"), argThat(data ->
            "USD".equals(data.get("currency"))
        ));
    }

    @Test
    void createDebt_WithInDebt_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("amount", "75.00");
        arguments.put("inDebt", "yes");

        when(monicaClient.post(eq("/debts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.createDebt(arguments).block();

        // Then - verify inDebt is mapped to in_debt
        verify(monicaClient).post(eq("/debts"), argThat(data ->
            "yes".equals(data.get("in_debt")) &&
            !data.containsKey("inDebt")
        ));
    }

    @Test
    void createDebt_WithStatus_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("amount", "100.00");
        arguments.put("status", "complete");

        when(monicaClient.post(eq("/debts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.createDebt(arguments).block();

        // Then - verify status is passed through
        verify(monicaClient).post(eq("/debts"), argThat(data ->
            "complete".equals(data.get("status"))
        ));
    }

    @Test
    void createDebt_WithReason_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("amount", "150.00");
        arguments.put("reason", "Borrowed for car repair");

        when(monicaClient.post(eq("/debts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.createDebt(arguments).block();

        // Then - verify reason is passed through
        verify(monicaClient).post(eq("/debts"), argThat(data ->
            "Borrowed for car repair".equals(data.get("reason"))
        ));
    }

    @Test
    void createDebt_WithAllFields_MapsAllFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("amount", "500.00");
        arguments.put("currency", "EUR");
        arguments.put("inDebt", "no");
        arguments.put("status", "inprogress");
        arguments.put("reason", "Loan for home renovation");

        when(monicaClient.post(eq("/debts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.createDebt(arguments).block();

        // Then
        verify(monicaClient).post(eq("/debts"), argThat(data ->
            Long.valueOf(42L).equals(data.get("contact_id")) &&
            "500.00".equals(data.get("amount")) &&
            "EUR".equals(data.get("currency")) &&
            "no".equals(data.get("in_debt")) &&
            "inprogress".equals(data.get("status")) &&
            "Loan for home renovation".equals(data.get("reason"))
        ));
    }

    @Test
    void createDebt_StringContactId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", "99");
        arguments.put("amount", "25.00");

        when(monicaClient.post(eq("/debts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.createDebt(arguments).block();

        // Then
        verify(monicaClient).post(eq("/debts"), argThat(data ->
            "99".equals(data.get("contact_id").toString())
        ));
    }

    @Test
    void createDebt_IntegerContactId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 123);
        arguments.put("amount", "80.00");

        when(monicaClient.post(eq("/debts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.createDebt(arguments).block();

        // Then
        verify(monicaClient).post(eq("/debts"), argThat(data ->
            Integer.valueOf(123).equals(data.get("contact_id"))
        ));
    }

    @Test
    void createDebt_EmptyArguments_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            debtService.createDebt(arguments).block();
        });
        assertEquals("Debt arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createDebt_NullArguments_ThrowsException() {
        // Given
        Map<String, Object> arguments = null;

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            debtService.createDebt(arguments).block();
        });
        assertEquals("Debt arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // GET DEBT TESTS
    // ========================================================================================

    @Test
    void getDebt_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/debts/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        Map<String, Object> result = debtService.getDebt(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/debts/1"), any());
    }

    @Test
    void getDebt_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            debtBuilder().id(42L).amount("75.00").build()
        );

        when(monicaClient.get(eq("/debts/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        Map<String, Object> result = debtService.getDebt(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/debts/42"), any());
    }

    @Test
    void getDebt_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            debtBuilder().id(123L).amount("200.00").build()
        );

        when(monicaClient.get(eq("/debts/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        Map<String, Object> result = debtService.getDebt(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/debts/123"), any());
    }

    @Test
    void getDebt_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("amount", "50.00");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            debtService.getDebt(arguments).block();
        });
        assertEquals("Debt ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getDebt_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            debtService.getDebt(arguments).block();
        });
        assertEquals("Debt ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getDebt_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            debtService.getDebt(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid debt ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getDebt_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("contact_id", 42L);
        apiData.put("amount", "150.00");
        apiData.put("currency", "USD");
        apiData.put("in_debt", "yes");
        apiData.put("status", "inprogress");
        apiData.put("reason", "Test reason");
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T09:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/debts/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = debtService.getDebt(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals(42L, data.get("contactId"));
        assertEquals("yes", data.get("inDebt"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T09:00:00Z", data.get("updatedAt"));
        // These should remain unchanged
        assertEquals(1L, data.get("id"));
        assertEquals("150.00", data.get("amount"));
        assertEquals("USD", data.get("currency"));
        assertEquals("inprogress", data.get("status"));
        assertEquals("Test reason", data.get("reason"));
    }

    @Test
    void getDebt_DirectResponseWithoutDataWrapper_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        // API response without "data" wrapper
        Map<String, Object> directResponse = new HashMap<>();
        directResponse.put("id", 1L);
        directResponse.put("contact_id", 55L);
        directResponse.put("amount", "75.50");
        directResponse.put("in_debt", "no");
        directResponse.put("created_at", "2024-01-20T10:00:00Z");
        directResponse.put("updated_at", "2024-01-20T10:00:00Z");

        when(monicaClient.get(eq("/debts/1"), any())).thenReturn(Mono.just(directResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = debtService.getDebt(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        assertEquals("75.50", data.get("amount"));
        assertEquals(55L, data.get("contactId"));
        assertEquals("no", data.get("inDebt"));
    }

    // ========================================================================================
    // UPDATE DEBT TESTS
    // ========================================================================================

    @Test
    void updateDebt_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("amount", "200.00");

        when(monicaClient.put(eq("/debts/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        Map<String, Object> result = debtService.updateDebt(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/debts/1"), argThat(data ->
            "200.00".equals(data.get("amount"))
        ));
    }

    @Test
    void updateDebt_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("amount", "100.00");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            debtService.updateDebt(arguments).block();
        });
        assertEquals("Debt ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateDebt_WithStatus_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("status", "complete");

        when(monicaClient.put(eq("/debts/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.updateDebt(arguments).block();

        // Then - verify status is passed through
        verify(monicaClient).put(eq("/debts/1"), argThat(data ->
            "complete".equals(data.get("status"))
        ));
    }

    @Test
    void updateDebt_WithAmount_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("amount", "350.00");

        when(monicaClient.put(eq("/debts/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.updateDebt(arguments).block();

        // Then - verify amount is passed through
        verify(monicaClient).put(eq("/debts/1"), argThat(data ->
            "350.00".equals(data.get("amount"))
        ));
    }

    @Test
    void updateDebt_WithInDebt_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("inDebt", "no");

        when(monicaClient.put(eq("/debts/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.updateDebt(arguments).block();

        // Then - verify inDebt is mapped to in_debt
        verify(monicaClient).put(eq("/debts/1"), argThat(data ->
            "no".equals(data.get("in_debt")) &&
            !data.containsKey("inDebt")
        ));
    }

    @Test
    void updateDebt_WithContactId_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactId", 99L);

        when(monicaClient.put(eq("/debts/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.updateDebt(arguments).block();

        // Then - verify contactId is mapped to contact_id
        verify(monicaClient).put(eq("/debts/1"), argThat(data ->
            Long.valueOf(99L).equals(data.get("contact_id")) &&
            !data.containsKey("contactId")
        ));
    }

    @Test
    void updateDebt_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "42");
        arguments.put("amount", "100.00");

        when(monicaClient.put(eq("/debts/42"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.updateDebt(arguments).block();

        // Then
        verify(monicaClient).put(eq("/debts/42"), any());
    }

    @Test
    void updateDebt_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");
        arguments.put("amount", "100.00");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            debtService.updateDebt(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid debt ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateDebt_WithAllFields_MapsAllFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactId", 50L);
        arguments.put("amount", "400.00");
        arguments.put("currency", "GBP");
        arguments.put("inDebt", "yes");
        arguments.put("status", "complete");
        arguments.put("reason", "Updated reason");

        when(monicaClient.put(eq("/debts/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.updateDebt(arguments).block();

        // Then
        verify(monicaClient).put(eq("/debts/1"), argThat(data ->
            Long.valueOf(50L).equals(data.get("contact_id")) &&
            "400.00".equals(data.get("amount")) &&
            "GBP".equals(data.get("currency")) &&
            "yes".equals(data.get("in_debt")) &&
            "complete".equals(data.get("status")) &&
            "Updated reason".equals(data.get("reason"))
        ));
    }

    // ========================================================================================
    // DELETE DEBT TESTS
    // ========================================================================================

    @Test
    void deleteDebt_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/debts/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Debt"), eq(1L), eq(true), anyString()
        )).thenReturn("Debt deleted successfully");

        // When
        Map<String, Object> result = debtService.deleteDebt(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));

        verify(monicaClient).delete(eq("/debts/1"));
    }

    @Test
    void deleteDebt_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/debts/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Debt"), eq(99L), eq(true), anyString()
        )).thenReturn("Debt deleted successfully");

        // When
        Map<String, Object> result = debtService.deleteDebt(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/debts/99"));
    }

    @Test
    void deleteDebt_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 55);
        Map<String, Object> deleteResponse = createDeleteResponse(55L);

        when(monicaClient.delete(eq("/debts/55"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Debt"), eq(55L), eq(true), anyString()
        )).thenReturn("Debt deleted successfully");

        // When
        Map<String, Object> result = debtService.deleteDebt(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/debts/55"));
    }

    @Test
    void deleteDebt_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("amount", "50.00");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            debtService.deleteDebt(arguments).block();
        });
        assertEquals("Debt ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteDebt_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            debtService.deleteDebt(arguments).block();
        });
        assertEquals("Debt ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteDebt_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            debtService.deleteDebt(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid debt ID format"));
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST DEBTS TESTS
    // ========================================================================================

    @Test
    void listDebts_WithPagination_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> debts = List.of(
            debtBuilder().id(1L).amount("100.00").build(),
            debtBuilder().id(2L).amount("200.00").build()
        );
        Map<String, Object> listResponse = createListResponse(debts, 2, 20, 50);

        when(monicaClient.get(eq("/debts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = debtService.listDebts(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/debts"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listDebts_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> debts = List.of(
            debtBuilder().id(1L).amount("50.00").build()
        );
        Map<String, Object> listResponse = createListResponse(debts);

        when(monicaClient.get(eq("/debts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        debtService.listDebts(arguments).block();

        // Then - verify default pagination values
        verify(monicaClient).get(eq("/debts"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listDebts_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> debts = List.of(
            debtBuilder().id(1L).amount("100.00").build()
        );
        Map<String, Object> listResponse = createListResponse(debts, 1, 10, 100);

        when(monicaClient.get(eq("/debts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = debtService.listDebts(arguments).block();

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
    void listDebts_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/debts"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = debtService.listDebts(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listDebts_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> debts = List.of(
            debtBuilder().id(1L).amount("100.00").build()
        );
        Map<String, Object> listResponse = createListResponse(debts);

        when(monicaClient.get(eq("/debts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        debtService.listDebts(arguments).block();

        // Then
        verify(monicaClient).get(eq("/debts"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listDebts_StringPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("page", "3");

        List<Map<String, Object>> debts = List.of(
            debtBuilder().id(1L).amount("100.00").build()
        );
        Map<String, Object> listResponse = createListResponse(debts, 3, 10, 30);

        when(monicaClient.get(eq("/debts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        debtService.listDebts(arguments).block();

        // Then
        verify(monicaClient).get(eq("/debts"), argThat(params ->
            "3".equals(params.get("page"))
        ));
    }

    @Test
    void listDebts_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> debtWithSnakeCase = new HashMap<>();
        debtWithSnakeCase.put("id", 1L);
        debtWithSnakeCase.put("contact_id", 42L);
        debtWithSnakeCase.put("amount", "150.00");
        debtWithSnakeCase.put("currency", "USD");
        debtWithSnakeCase.put("in_debt", "yes");
        debtWithSnakeCase.put("status", "inprogress");
        debtWithSnakeCase.put("created_at", "2024-01-15T10:00:00Z");
        debtWithSnakeCase.put("updated_at", "2024-01-15T10:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(debtWithSnakeCase));

        when(monicaClient.get(eq("/debts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = debtService.listDebts(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify snake_case is mapped to camelCase
        assertEquals(42L, data.get(0).get("contactId"));
        assertEquals("yes", data.get(0).get("inDebt"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("updatedAt"));
        // These should remain unchanged
        assertEquals("150.00", data.get(0).get("amount"));
        assertEquals("USD", data.get(0).get("currency"));
        assertEquals("inprogress", data.get(0).get("status"));
    }

    @Test
    void listDebts_IntegerPageAndLimit_ConvertsToString() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 5,
            "limit", 50
        );

        List<Map<String, Object>> debts = List.of(
            debtBuilder().id(1L).amount("100.00").build()
        );
        Map<String, Object> listResponse = createListResponse(debts, 5, 50, 100);

        when(monicaClient.get(eq("/debts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        debtService.listDebts(arguments).block();

        // Then
        verify(monicaClient).get(eq("/debts"), argThat(params ->
            "5".equals(params.get("page")) &&
            "50".equals(params.get("limit"))
        ));
    }

    @Test
    void listDebts_NoMetaInResponse_HandlesGracefully() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> debts = List.of(
            debtBuilder().id(1L).amount("100.00").build()
        );
        // Response without meta
        Map<String, Object> listResponse = new HashMap<>();
        listResponse.put("data", debts);

        when(monicaClient.get(eq("/debts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = debtService.listDebts(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertFalse(result.containsKey("meta"));
    }

    @Test
    void listDebts_MultipleDebts_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> debts = List.of(
            debtBuilder().id(1L).amount("100.00").inDebt("yes").status("inprogress").build(),
            debtBuilder().id(2L).amount("200.00").inDebt("no").status("complete").build(),
            debtBuilder().id(3L).amount("300.00").inDebt("yes").status("inprogress").build()
        );
        Map<String, Object> listResponse = createListResponse(debts, 1, 10, 3);

        when(monicaClient.get(eq("/debts"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = debtService.listDebts(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(3, data.size());

        assertEquals("100.00", data.get(0).get("amount"));
        assertEquals("yes", data.get(0).get("inDebt"));
        assertEquals("200.00", data.get(1).get("amount"));
        assertEquals("no", data.get(1).get("inDebt"));
        assertEquals("300.00", data.get(2).get("amount"));
        assertEquals("yes", data.get(2).get("inDebt"));
    }

    // ========================================================================================
    // DIRECTION HANDLING TESTS (inDebt field)
    // ========================================================================================

    @Test
    void createDebt_WithInDebtYes_SetsCorrectDirection() {
        // Given - "yes" means the user owes the contact
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("amount", "100.00");
        arguments.put("inDebt", "yes");

        when(monicaClient.post(eq("/debts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.createDebt(arguments).block();

        // Then
        verify(monicaClient).post(eq("/debts"), argThat(data ->
            "yes".equals(data.get("in_debt"))
        ));
    }

    @Test
    void createDebt_WithInDebtNo_SetsCorrectDirection() {
        // Given - "no" means the contact owes the user
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("amount", "100.00");
        arguments.put("inDebt", "no");

        when(monicaClient.post(eq("/debts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.createDebt(arguments).block();

        // Then
        verify(monicaClient).post(eq("/debts"), argThat(data ->
            "no".equals(data.get("in_debt"))
        ));
    }

    @Test
    void updateDebt_ChangeDirection_UpdatesCorrectly() {
        // Given - changing from "yes" to "no"
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("inDebt", "no");

        when(monicaClient.put(eq("/debts/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.updateDebt(arguments).block();

        // Then
        verify(monicaClient).put(eq("/debts/1"), argThat(data ->
            "no".equals(data.get("in_debt"))
        ));
    }

    // ========================================================================================
    // AMOUNT VALIDATION TESTS
    // ========================================================================================

    @Test
    void createDebt_WithDecimalAmount_HandlesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("amount", "99.99");

        when(monicaClient.post(eq("/debts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.createDebt(arguments).block();

        // Then
        verify(monicaClient).post(eq("/debts"), argThat(data ->
            "99.99".equals(data.get("amount"))
        ));
    }

    @Test
    void createDebt_WithIntegerAmount_HandlesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("amount", 100);

        when(monicaClient.post(eq("/debts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.createDebt(arguments).block();

        // Then
        verify(monicaClient).post(eq("/debts"), argThat(data ->
            Integer.valueOf(100).equals(data.get("amount"))
        ));
    }

    @Test
    void createDebt_WithZeroAmount_HandlesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("amount", "0.00");

        when(monicaClient.post(eq("/debts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.createDebt(arguments).block();

        // Then
        verify(monicaClient).post(eq("/debts"), argThat(data ->
            "0.00".equals(data.get("amount"))
        ));
    }

    @Test
    void createDebt_WithLargeAmount_HandlesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("amount", "1000000.00");

        when(monicaClient.post(eq("/debts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.createDebt(arguments).block();

        // Then
        verify(monicaClient).post(eq("/debts"), argThat(data ->
            "1000000.00".equals(data.get("amount"))
        ));
    }

    @Test
    void updateDebt_WithAmount_UpdatesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("amount", "250.50");

        when(monicaClient.put(eq("/debts/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.updateDebt(arguments).block();

        // Then
        verify(monicaClient).put(eq("/debts/1"), argThat(data ->
            "250.50".equals(data.get("amount"))
        ));
    }

    // ========================================================================================
    // STATUS TRACKING TESTS
    // ========================================================================================

    @Test
    void createDebt_WithStatusInprogress_SetsCorrectStatus() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("amount", "100.00");
        arguments.put("status", "inprogress");

        when(monicaClient.post(eq("/debts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.createDebt(arguments).block();

        // Then
        verify(monicaClient).post(eq("/debts"), argThat(data ->
            "inprogress".equals(data.get("status"))
        ));
    }

    @Test
    void createDebt_WithStatusComplete_SetsCorrectStatus() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("amount", "100.00");
        arguments.put("status", "complete");

        when(monicaClient.post(eq("/debts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.createDebt(arguments).block();

        // Then
        verify(monicaClient).post(eq("/debts"), argThat(data ->
            "complete".equals(data.get("status"))
        ));
    }

    @Test
    void updateDebt_ChangeStatus_UpdatesCorrectly() {
        // Given - changing from inprogress to complete
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("status", "complete");

        when(monicaClient.put(eq("/debts/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.updateDebt(arguments).block();

        // Then
        verify(monicaClient).put(eq("/debts/1"), argThat(data ->
            "complete".equals(data.get("status"))
        ));
    }

    // ========================================================================================
    // CURRENCY HANDLING TESTS
    // ========================================================================================

    @Test
    void createDebt_WithVariousCurrencies_HandlesCorrectly() {
        // Given - testing with USD
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("amount", "100.00");
        arguments.put("currency", "USD");

        when(monicaClient.post(eq("/debts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.createDebt(arguments).block();

        // Then
        verify(monicaClient).post(eq("/debts"), argThat(data ->
            "USD".equals(data.get("currency"))
        ));
    }

    @Test
    void createDebt_WithEurCurrency_HandlesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 42L);
        arguments.put("amount", "100.00");
        arguments.put("currency", "EUR");

        when(monicaClient.post(eq("/debts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.createDebt(arguments).block();

        // Then
        verify(monicaClient).post(eq("/debts"), argThat(data ->
            "EUR".equals(data.get("currency"))
        ));
    }

    @Test
    void updateDebt_ChangeCurrency_UpdatesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("currency", "GBP");

        when(monicaClient.put(eq("/debts/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted debt JSON");

        // When
        debtService.updateDebt(arguments).block();

        // Then
        verify(monicaClient).put(eq("/debts/1"), argThat(data ->
            "GBP".equals(data.get("currency"))
        ));
    }
}
