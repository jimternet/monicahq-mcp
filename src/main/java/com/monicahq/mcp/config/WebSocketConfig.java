package com.monicahq.mcp.config;

import com.monicahq.mcp.controller.McpWebSocketController;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.socket.config.annotation.EnableWebSocket;
import org.springframework.web.socket.config.annotation.WebSocketConfigurer;
import org.springframework.web.socket.config.annotation.WebSocketHandlerRegistry;

@Configuration
@EnableWebSocket
@RequiredArgsConstructor
@Slf4j
public class WebSocketConfig implements WebSocketConfigurer {

    private final McpWebSocketController webSocketController;
    
    @Value("${mcp.websocket.allowed-origins:*}")
    private String allowedOrigins;
    
    @Value("${mcp.websocket.buffer-size:8192}")
    private int bufferSize;

    @Override
    public void registerWebSocketHandlers(WebSocketHandlerRegistry registry) {
        log.info("Registering MCP WebSocket handlers");
        
        registry.addHandler(webSocketController, "/mcp")
            .setAllowedOrigins(allowedOrigins.split(","))
            .setHandshakeHandler(new McpHandshakeHandler())
            .withSockJS()
            .setHeartbeatTime(25000) // 25 seconds heartbeat
            .setDisconnectDelay(30000) // 30 seconds disconnect delay
            .setStreamBytesLimit(bufferSize)
            .setHttpMessageCacheSize(1000);
            
        // Also register without SockJS for direct WebSocket connections
        registry.addHandler(webSocketController, "/mcp/direct")
            .setAllowedOrigins(allowedOrigins.split(","))
            .setHandshakeHandler(new McpHandshakeHandler());
            
        log.info("MCP WebSocket endpoints registered: /mcp (with SockJS) and /mcp/direct");
    }
}
