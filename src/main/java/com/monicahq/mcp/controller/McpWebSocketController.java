package com.monicahq.mcp.controller;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.*;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Component
@RequiredArgsConstructor
@Slf4j
public class McpWebSocketController implements WebSocketHandler {

    private final McpMessageHandler messageHandler;
    private final ObjectMapper objectMapper;
    private final Map<String, WebSocketSession> sessions = new ConcurrentHashMap<>();

    @Override
    public void afterConnectionEstablished(WebSocketSession session) throws Exception {
        String sessionId = session.getId();
        sessions.put(sessionId, session);
        log.info("MCP WebSocket connection established: {} from {}", sessionId, session.getRemoteAddress());
        
        // Send welcome message indicating MCP protocol support
        Map<String, Object> welcome = Map.of(
            "protocol", "MCP",
            "version", "2024-11-05",
            "server", "monicahq-mcp-server",
            "status", "ready"
        );
        
        sendMessage(session, welcome);
    }

    @Override
    public void handleMessage(WebSocketSession session, WebSocketMessage<?> message) throws Exception {
        try {
            String payload = message.getPayload().toString();
            log.debug("Received MCP message from {}: {}", session.getId(), payload);
            
            // Parse JSON-RPC message
            JsonNode jsonMessage = objectMapper.readTree(payload);
            
            // Validate JSON-RPC 2.0 format
            if (!isValidJsonRpc(jsonMessage)) {
                sendErrorResponse(session, null, -32600, "Invalid Request", "Invalid JSON-RPC 2.0 format");
                return;
            }
            
            // Process the MCP message
            Map<String, Object> response = messageHandler.handleMessage(jsonMessage);
            
            // Send response back to client
            sendMessage(session, response);
            
        } catch (JsonProcessingException e) {
            log.error("Failed to parse JSON message from {}: {}", session.getId(), e.getMessage());
            sendErrorResponse(session, null, -32700, "Parse error", "Invalid JSON");
            
        } catch (Exception e) {
            log.error("Error handling message from {}: {}", session.getId(), e.getMessage(), e);
            sendErrorResponse(session, null, -32603, "Internal error", e.getMessage());
        }
    }

    @Override
    public void handleTransportError(WebSocketSession session, Throwable exception) throws Exception {
        String sessionId = session.getId();
        log.error("WebSocket transport error for session {}: {}", sessionId, exception.getMessage(), exception);
        
        // Close session if it's still open
        if (session.isOpen()) {
            session.close(CloseStatus.SERVER_ERROR.withReason("Transport error"));
        }
    }

    @Override
    public void afterConnectionClosed(WebSocketSession session, CloseStatus closeStatus) throws Exception {
        String sessionId = session.getId();
        sessions.remove(sessionId);
        log.info("MCP WebSocket connection closed: {} with status: {}", sessionId, closeStatus);
    }

    @Override
    public boolean supportsPartialMessages() {
        return false;
    }

    private boolean isValidJsonRpc(JsonNode message) {
        // JSON-RPC 2.0 requires "jsonrpc": "2.0"
        JsonNode jsonrpc = message.get("jsonrpc");
        if (jsonrpc == null || !"2.0".equals(jsonrpc.asText())) {
            return false;
        }
        
        // Must have either "method" (request) or "result"/"error" (response)
        boolean hasMethod = message.has("method");
        boolean hasResult = message.has("result");
        boolean hasError = message.has("error");
        
        return hasMethod || hasResult || hasError;
    }

    private void sendMessage(WebSocketSession session, Map<String, Object> message) {
        try {
            if (session.isOpen()) {
                String jsonMessage = objectMapper.writeValueAsString(message);
                session.sendMessage(new TextMessage(jsonMessage));
                log.debug("Sent MCP message to {}: {}", session.getId(), jsonMessage);
            } else {
                log.warn("Attempted to send message to closed session: {}", session.getId());
            }
        } catch (Exception e) {
            log.error("Failed to send message to session {}: {}", session.getId(), e.getMessage(), e);
        }
    }

    private void sendErrorResponse(WebSocketSession session, Object id, int code, String message, String data) {
        Map<String, Object> error = Map.of(
            "code", code,
            "message", message,
            "data", data != null ? data : ""
        );
        
        Map<String, Object> response = Map.of(
            "jsonrpc", "2.0",
            "error", error,
            "id", id != null ? id : null
        );
        
        sendMessage(session, response);
    }

    public int getActiveSessionCount() {
        return sessions.size();
    }

    public void broadcastToAll(Map<String, Object> message) {
        sessions.values().forEach(session -> sendMessage(session, message));
    }
}
