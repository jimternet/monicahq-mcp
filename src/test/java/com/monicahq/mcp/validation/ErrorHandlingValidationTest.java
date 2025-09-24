package com.monicahq.mcp.validation;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.UserService;
import com.monicahq.mcp.service.ComplianceService;
import com.monicahq.mcp.util.ContentFormatter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Mono;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Validation tests for error handling in potentially unavailable APIs.
 */
@ExtendWith(MockitoExtension.class)
class ErrorHandlingValidationTest {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private UserService userService;
    private ComplianceService complianceService;

    @BeforeEach
    void setUp() {
        userService = new UserService(monicaClient, contentFormatter);
        complianceService = new ComplianceService(monicaClient, contentFormatter);
    }

    @Test
    void userOperations_ApiNotAvailable_ShouldThrowAppropriateException() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 10);
        
        // Simulate 404 response from Monica API
        when(monicaClient.get(eq("/users"), any()))
            .thenReturn(Mono.error(new RuntimeException("404 Not Found")));

        // When & Then
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            userService.listUsers(arguments).block();
        });
        
        assertTrue(exception.getMessage().contains("Users API is not available"));
        assertTrue(exception.getMessage().contains("admin-only"));
    }

    @Test
    void complianceOperations_ExperimentalEndpoints_ShouldHandleErrors() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 10);
        
        // Simulate unclear endpoint response
        when(monicaClient.get(eq("/compliance"), any()))
            .thenReturn(Mono.error(new RuntimeException("Endpoint not clearly documented")));

        // When & Then
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            complianceService.listCompliance(arguments).block();
        });
        
        assertTrue(exception.getMessage().contains("Compliance API is not available"));
    }

    @Test
    void userService_CreateOperation_WithValidData_ShouldFormatCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of(
            "firstName", "John",
            "lastName", "Administrator", 
            "email", "admin@example.com"
        );
        
        Map<String, Object> mockResponse = Map.of(
            "data", Map.of(
                "id", 1L,
                "first_name", "John",
                "last_name", "Administrator",
                "email", "admin@example.com"
            )
        );
        
        when(monicaClient.post(eq("/users"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted user JSON");

        // When
        Map<String, Object> result = userService.createUser(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        
        // Verify request body formatting
        verify(monicaClient).post(eq("/users"), argThat(data -> {
            return "John".equals(data.get("first_name")) &&
                   "Administrator".equals(data.get("last_name")) &&
                   "admin@example.com".equals(data.get("email"));
        }));
    }

    @Test
    void complianceService_CreateOperation_WithValidData_ShouldFormatCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of(
            "type", "data_protection",
            "description", "GDPR compliance audit"
        );
        
        Map<String, Object> mockResponse = Map.of(
            "data", Map.of(
                "id", 1L,
                "type", "data_protection",
                "description", "GDPR compliance audit"
            )
        );
        
        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted compliance JSON");

        // When
        Map<String, Object> result = complianceService.createCompliance(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        
        // Verify request body formatting
        verify(monicaClient).post(eq("/compliance"), argThat(data -> {
            return "data_protection".equals(data.get("type")) &&
                   "GDPR compliance audit".equals(data.get("description"));
        }));
    }
}