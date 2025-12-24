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

/**
 * WebSocket controller for Model Context Protocol (MCP) communication.
 *
 * <p>This controller handles WebSocket connections for the MCP protocol, providing
 * real-time bidirectional communication between MCP clients and the MonicaHQ backend.
 *
 * <h2>WebSocket Endpoints</h2>
 * <ul>
 *   <li>{@code /mcp} - Primary endpoint with SockJS fallback support</li>
 *   <li>{@code /mcp/direct} - Direct WebSocket connection without SockJS</li>
 * </ul>
 *
 * <h2>Protocol</h2>
 * <p>All messages follow the JSON-RPC 2.0 specification. The server supports:
 * <ul>
 *   <li>MCP protocol version: 2024-11-05</li>
 *   <li>Tool invocations for MonicaHQ API operations</li>
 *   <li>Resource requests for contacts, notes, activities, etc.</li>
 *   <li>Protocol negotiation and capability exchange</li>
 * </ul>
 *
 * <h2>Note on OpenAPI Documentation</h2>
 * <p>WebSocket endpoints are not documented via OpenAPI/Swagger annotations as OpenAPI
 * is designed for RESTful HTTP APIs. For HTTP-based MCP access, use the
 * {@link McpHttpBridgeController} which provides equivalent functionality via REST.
 *
 * @see McpHttpBridgeController for HTTP-based MCP access with OpenAPI documentation
 * @see McpMessageHandler for message processing logic
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class McpWebSocketController implements WebSocketHandler {

    private final McpMessageHandler messageHandler;
    private final ObjectMapper objectMapper;
    private final Map<String, WebSocketSession> sessions = new ConcurrentHashMap<>();

    /**
     * Handles new WebSocket connection establishment.
     *
     * <p>When a client connects, this method:
     * <ol>
     *   <li>Registers the session for tracking</li>
     *   <li>Sends a welcome message with MCP protocol information</li>
     * </ol>
     *
     * @param session the newly established WebSocket session
     * @throws Exception if sending the welcome message fails
     */
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

    /**
     * Processes incoming WebSocket messages as JSON-RPC 2.0 requests.
     *
     * <p>This method validates the JSON-RPC format and delegates to {@link McpMessageHandler}
     * for processing. Error responses follow JSON-RPC 2.0 error codes:
     * <ul>
     *   <li>{@code -32700} - Parse error (invalid JSON)</li>
     *   <li>{@code -32600} - Invalid Request (malformed JSON-RPC)</li>
     *   <li>{@code -32603} - Internal error</li>
     * </ul>
     *
     * @param session the WebSocket session from which the message was received
     * @param message the incoming WebSocket message
     * @throws Exception if message handling fails unexpectedly
     */
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

    /**
     * Handles WebSocket transport errors.
     *
     * <p>Logs the error and closes the session if it's still open.
     *
     * @param session the WebSocket session that encountered the error
     * @param exception the transport error that occurred
     * @throws Exception if closing the session fails
     */
    @Override
    public void handleTransportError(WebSocketSession session, Throwable exception) throws Exception {
        String sessionId = session.getId();
        log.error("WebSocket transport error for session {}: {}", sessionId, exception.getMessage(), exception);

        // Close session if it's still open
        if (session.isOpen()) {
            session.close(CloseStatus.SERVER_ERROR.withReason("Transport error"));
        }
    }

    /**
     * Handles WebSocket connection closure.
     *
     * <p>Removes the session from tracking and logs the closure reason.
     *
     * @param session the WebSocket session that was closed
     * @param closeStatus the status indicating why the connection was closed
     * @throws Exception if cleanup fails
     */
    @Override
    public void afterConnectionClosed(WebSocketSession session, CloseStatus closeStatus) throws Exception {
        String sessionId = session.getId();
        sessions.remove(sessionId);
        log.info("MCP WebSocket connection closed: {} with status: {}", sessionId, closeStatus);
    }

    /**
     * Indicates whether this handler supports partial messages.
     *
     * @return {@code false} - this handler requires complete messages
     */
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

    /**
     * Returns the number of currently active WebSocket sessions.
     *
     * @return the count of active MCP WebSocket connections
     */
    public int getActiveSessionCount() {
        return sessions.size();
    }

    /**
     * Broadcasts a message to all connected WebSocket clients.
     *
     * <p>This can be used for server-initiated notifications or events.
     *
     * @param message the message to broadcast to all clients
     */
    public void broadcastToAll(Map<String, Object> message) {
        sessions.values().forEach(session -> sendMessage(session, message));
    }
}
