package com.monicahq.mcp.config;

import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

import java.util.UUID;

@Configuration
@Slf4j
public class LoggingConfig {

    private static final String CORRELATION_ID_HEADER = "X-Correlation-ID";
    private static final String CORRELATION_ID_KEY = "correlationId";
    private static final String SESSION_ID_KEY = "sessionId";
    private static final String REQUEST_ID_KEY = "requestId";

    @Bean
    public WebFilter correlationIdFilter() {
        return new CorrelationIdWebFilter();
    }

    public static void setCorrelationId(String correlationId) {
        MDC.put(CORRELATION_ID_KEY, correlationId);
    }

    public static void setSessionId(String sessionId) {
        MDC.put(SESSION_ID_KEY, sessionId);
    }

    public static void setRequestId(String requestId) {
        MDC.put(REQUEST_ID_KEY, requestId);
    }

    public static String getCorrelationId() {
        return MDC.get(CORRELATION_ID_KEY);
    }

    public static String getSessionId() {
        return MDC.get(SESSION_ID_KEY);
    }

    public static String getRequestId() {
        return MDC.get(REQUEST_ID_KEY);
    }

    public static void clearMDC() {
        MDC.clear();
    }

    public static void addContextToMDC(String correlationId, String sessionId, String requestId) {
        if (correlationId != null) {
            MDC.put(CORRELATION_ID_KEY, correlationId);
        }
        if (sessionId != null) {
            MDC.put(SESSION_ID_KEY, sessionId);
        }
        if (requestId != null) {
            MDC.put(REQUEST_ID_KEY, requestId);
        }
    }

    private static class CorrelationIdWebFilter implements WebFilter {

        @Override
        public Mono<Void> filter(ServerWebExchange exchange, WebFilterChain chain) {
            return Mono.deferContextual(contextView -> {
                
                // Get or generate correlation ID
                String correlationId = exchange.getRequest()
                    .getHeaders()
                    .getFirst(CORRELATION_ID_HEADER);
                    
                final String finalCorrelationId = correlationId == null || correlationId.trim().isEmpty() 
                    ? UUID.randomUUID().toString() 
                    : correlationId;
                
                // Generate request ID for this specific request
                final String finalRequestId = UUID.randomUUID().toString();
                
                // Set correlation ID in response header
                exchange.getResponse().getHeaders().add(CORRELATION_ID_HEADER, finalCorrelationId);
                
                // Set in MDC for logging
                setCorrelationId(finalCorrelationId);
                setRequestId(finalRequestId);
                
                // Store in reactor context for propagation through the reactive chain
                return chain.filter(exchange)
                    .contextWrite(ctx -> ctx
                        .put(CORRELATION_ID_KEY, finalCorrelationId)
                        .put(REQUEST_ID_KEY, finalRequestId)
                    )
                    .doFinally(signalType -> {
                        // Clear MDC after request processing
                        clearMDC();
                    });
            });
        }
    }

    public static class McpLoggingContext {
        
        public static void setMcpContext(String sessionId, String toolName, String messageId) {
            if (sessionId != null) {
                setSessionId(sessionId);
            }
            if (toolName != null) {
                MDC.put("toolName", toolName);
            }
            if (messageId != null) {
                MDC.put("messageId", messageId);
            }
        }
        
        public static void setToolExecutionContext(String toolName, String operation) {
            MDC.put("toolName", toolName);
            MDC.put("operation", operation);
            MDC.put("executionId", UUID.randomUUID().toString());
        }
        
        public static void setServiceContext(String serviceName, String method) {
            MDC.put("serviceName", serviceName);
            MDC.put("serviceMethod", method);
        }
        
        public static void setApiContext(String endpoint, String httpMethod) {
            MDC.put("apiEndpoint", endpoint);
            MDC.put("httpMethod", httpMethod);
        }
        
        public static void clearMcpContext() {
            MDC.remove("toolName");
            MDC.remove("messageId");
            MDC.remove("operation");
            MDC.remove("executionId");
            MDC.remove("serviceName");
            MDC.remove("serviceMethod");
            MDC.remove("apiEndpoint");
            MDC.remove("httpMethod");
        }
    }

    public static class McpMetrics {
        
        public static void logToolExecution(String toolName, long durationMs, boolean success, String error) {
            log.info("MCP tool execution - tool: {}, duration: {}ms, success: {}, error: {}", 
                toolName, durationMs, success, error);
        }
        
        public static void logServiceCall(String serviceName, String method, long durationMs, boolean success) {
            log.info("Service call - service: {}, method: {}, duration: {}ms, success: {}", 
                serviceName, method, durationMs, success);
        }
        
        public static void logApiCall(String endpoint, String httpMethod, long durationMs, int statusCode) {
            log.info("API call - endpoint: {}, method: {}, duration: {}ms, status: {}", 
                endpoint, httpMethod, durationMs, statusCode);
        }
        
        public static void logWebSocketConnection(String sessionId, String event, String details) {
            log.info("WebSocket event - session: {}, event: {}, details: {}", 
                sessionId, event, details);
        }
    }
}