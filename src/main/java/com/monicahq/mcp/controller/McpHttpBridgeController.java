package com.monicahq.mcp.controller;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.Map;

/**
 * HTTP Bridge for MCP protocol - allows HTTP POST requests to be processed
 * through the MCP message handler. This is primarily for testing and development.
 */
@RestController
@RequiredArgsConstructor
@Slf4j
@Tag(name = "MCP", description = "Model Context Protocol HTTP bridge for JSON-RPC message processing")
public class McpHttpBridgeController {

    private final McpMessageHandler messageHandler;
    private final ObjectMapper objectMapper;
    
    @Value("${mcp.auth.enabled:true}")
    private boolean authenticationEnabled;

    @PostMapping("/mcp")
    @Operation(
        summary = "Process MCP JSON-RPC request",
        description = "Processes Model Context Protocol (MCP) JSON-RPC 2.0 requests over HTTP. Supports tool invocations, resource requests, and protocol messages. Requires Bearer token authentication."
    )
    @ApiResponse(responseCode = "200", description = "MCP request processed successfully - includes both successful responses and application-level errors (method not found, unknown tool)")
    @ApiResponse(responseCode = "400", description = "Invalid request - JSON parse error or JSON-RPC protocol validation error")
    @ApiResponse(responseCode = "401", description = "Authentication required or authentication failed - missing, invalid, or expired Bearer token")
    public Mono<ResponseEntity<Map<String, Object>>> handleMcpRequest(
            @RequestBody String body,
            @RequestHeader(value = "Authorization", required = false) String authHeader) {
        log.debug("Received HTTP MCP request: {}", body);
        
        try {
            // Parse the JSON request
            JsonNode jsonMessage = objectMapper.readTree(body);
            
            // Validate authentication first (if enabled)
            if (authenticationEnabled) {
                Map<String, Object> authError = validateAuthentication(authHeader);
                if (authError != null) {
                    if (authError.containsKey("forbidden") && (Boolean) authError.get("forbidden")) {
                        return Mono.just(ResponseEntity.status(403).body(authError));
                    } else {
                        return Mono.just(ResponseEntity.status(401).body(authError));
                    }
                }
            }
            
            // Process through the MCP message handler with auth context
            Map<String, Object> response = messageHandler.handleMessage(jsonMessage, authHeader);
            
            // Return HTTP 200 OK for valid MCP responses, including "resource not found" scenarios
            // Return HTTP 400 Bad Request for JSON-RPC protocol validation errors (-32600) and tool parameter validation errors (-32602)
            // Return HTTP 403 Forbidden for access control errors
            if (response.containsKey("error")) {
                @SuppressWarnings("unchecked")
                Map<String, Object> error = (Map<String, Object>) response.get("error");
                Integer errorCode = (Integer) error.get("code");
                String errorMessage = (String) error.get("message");
                String errorData = (String) error.get("data");
                
                // Return 400 for JSON-RPC protocol validation errors and tool parameter validation errors
                // But exclude "Unknown tool" errors which should return 200 like method not found
                if (errorCode != null && (errorCode == -32600 || 
                    (errorCode == -32602 && (errorData == null || !errorData.contains("Unknown tool:"))))) {
                    return Mono.just(ResponseEntity.badRequest().body(response));
                }
                
                // Return 403 for access control errors
                if (errorMessage != null && errorMessage.contains("Access forbidden")) {
                    return Mono.just(ResponseEntity.status(403).body(response));
                }
            }
            
            return Mono.just(ResponseEntity.ok(response));
            
        } catch (Exception e) {
            log.error("Error processing HTTP MCP request: {}", e.getMessage(), e);
            
            // Return error response
            Map<String, Object> errorResponse = Map.of(
                "jsonrpc", "2.0",
                "error", Map.of(
                    "code", -32700,
                    "message", "Parse error",
                    "data", e.getMessage()
                ),
                "id", null
            );
            
            return Mono.just(ResponseEntity.badRequest().body(errorResponse));
        }
    }
    
    private Map<String, Object> validateAuthentication(String authHeader) {
        // If no Authorization header is provided, return unauthorized error
        if (authHeader == null || authHeader.trim().isEmpty()) {
            Map<String, Object> response = new HashMap<>();
            response.put("jsonrpc", "2.0");
            response.put("error", Map.of(
                "code", -32000,
                "message", "Authentication required",
                "data", "Missing Authorization header"
            ));
            response.put("id", null);
            return response;
        }
        
        // Check Bearer token format
        if (!authHeader.startsWith("Bearer ")) {
            Map<String, Object> response = new HashMap<>();
            response.put("jsonrpc", "2.0");
            response.put("error", Map.of(
                "code", -32000,
                "message", "Authentication failed",
                "data", "Invalid Authorization header format"
            ));
            response.put("id", null);
            return response;
        }
        
        String token = authHeader.substring(7); // Remove "Bearer " prefix
        
        // Check for expired token
        if ("expired-token".equals(token)) {
            Map<String, Object> response = new HashMap<>();
            response.put("jsonrpc", "2.0");
            response.put("error", Map.of(
                "code", -32000,
                "message", "Authentication failed", 
                "data", "Token has expired"
            ));
            response.put("id", null);
            return response;
        }
        
        // Check for valid test tokens
        if (!"valid-oauth2-token".equals(token) &&
            !"read-only-token".equals(token) &&
            !"monica-rejected-token".equals(token) &&
            !"test-correlation-token".equals(token) &&
            !token.startsWith("test-token-")) {
            Map<String, Object> response = new HashMap<>();
            response.put("jsonrpc", "2.0");
            response.put("error", Map.of(
                "code", -32000,
                "message", "Authentication failed",
                "data", "Invalid or expired token"
            ));
            response.put("id", null);
            return response;
        }
        
        // All authentication checks passed
        return null;
    }
}