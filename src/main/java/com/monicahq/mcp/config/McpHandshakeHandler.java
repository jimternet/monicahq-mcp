package com.monicahq.mcp.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.web.socket.WebSocketHandler;
import org.springframework.web.socket.server.support.DefaultHandshakeHandler;

import java.security.Principal;
import java.util.Map;
import java.util.UUID;

@Slf4j
public class McpHandshakeHandler extends DefaultHandshakeHandler {

    @Override
    protected Principal determineUser(ServerHttpRequest request, 
                                    WebSocketHandler wsHandler, 
                                    Map<String, Object> attributes) {
        
        // Generate a unique session ID for each MCP connection
        String sessionId = UUID.randomUUID().toString();
        
        log.info("MCP WebSocket handshake from {}, assigned session ID: {}", 
            request.getRemoteAddress(), sessionId);
        
        // Store additional connection metadata
        attributes.put("sessionId", sessionId);
        attributes.put("connectedAt", System.currentTimeMillis());
        attributes.put("remoteAddress", request.getRemoteAddress());
        
        // Check for User-Agent or other client identification headers
        String userAgent = request.getHeaders().getFirst("User-Agent");
        if (userAgent != null) {
            attributes.put("userAgent", userAgent);
            log.debug("Client User-Agent: {}", userAgent);
        }
        
        // Check for MCP protocol version in headers
        String mcpVersion = request.getHeaders().getFirst("MCP-Version");
        if (mcpVersion != null) {
            attributes.put("mcpVersion", mcpVersion);
            log.info("Client requests MCP version: {}", mcpVersion);
        }
        
        return new McpSessionPrincipal(sessionId);
    }

    private static class McpSessionPrincipal implements Principal {
        private final String sessionId;

        public McpSessionPrincipal(String sessionId) {
            this.sessionId = sessionId;
        }

        @Override
        public String getName() {
            return sessionId;
        }
        
        public String getSessionId() {
            return sessionId;
        }
    }
}