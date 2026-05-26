package com.monicahq.mcp.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("McpHttpBridgeController Unit Tests")
class McpHttpBridgeControllerTest {

    @Mock
    private McpMessageHandler messageHandler;

    private McpHttpBridgeController controller;

    @BeforeEach
    void setUp() {
        controller = new McpHttpBridgeController(messageHandler, new ObjectMapper());
        ReflectionTestUtils.setField(controller, "authenticationEnabled", false);
    }

    @Test
    @DisplayName("Should return 204 when MCP handler returns null for notification")
    void shouldReturnNoContentWhenHandlerReturnsNull() {
        // Given: valid JSON-RPC notification body that yields null response
        String body = "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}";
        when(messageHandler.handleMessage(any(), isNull())).thenReturn(null);

        // When
        ResponseEntity<Map<String, Object>> response = controller.handleMcpRequest(body, null).block();

        // Then: no NPE and a proper no-content response
        assertNotNull(response);
        assertEquals(HttpStatus.NO_CONTENT, response.getStatusCode());
        assertNull(response.getBody());
    }
}
