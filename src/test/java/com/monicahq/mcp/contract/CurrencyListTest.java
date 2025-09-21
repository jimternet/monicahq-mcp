package com.monicahq.mcp.contract;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.monicahq.mcp.controller.McpMessageHandler;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Contract test for currency_list and currency_search MCP operations.
 * Tests the MCP message handler directly using TestMonicaHqClient.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class CurrencyListTest {

    @Autowired
    private McpMessageHandler messageHandler;
    
    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldListCurrenciesViaMcpProtocol() throws Exception {
        // When: Send MCP tools/call request to list currencies
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "currency_list",
                "arguments", Map.of(
                    "page", 1,
                    "limit", 50
                )
            ),
            "id", 1
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Verify response contains list of currencies
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(1L, response.get("id"));
        assertTrue(response.containsKey("result"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("data"));
        
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> currencies = (List<Map<String, Object>>) result.get("data");
        assertNotNull(currencies);
    }

    @Test
    void shouldSearchCurrenciesByCodeOrName() throws Exception {
        // When: Send MCP tools/call request to search currencies
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "currency_search",
                "arguments", Map.of(
                    "search", "USD",
                    "page", 1,
                    "limit", 50
                )
            ),
            "id", 2
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Verify response structure
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(2L, response.get("id"));
        assertTrue(response.containsKey("result"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("data"));
    }
}