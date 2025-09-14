package com.monicahq.mcp.controller;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.config.LoggingConfig;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

import java.time.Instant;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

@RestController
@RequestMapping("/health")
@RequiredArgsConstructor
@Slf4j
public class HealthController {

    private final MonicaHqClient monicaClient;
    private final McpWebSocketController webSocketController;
    private final McpToolRegistry toolRegistry;
    
    private final AtomicLong requestCount = new AtomicLong(0);
    private final Instant startTime = Instant.now();

    @GetMapping
    public Mono<ResponseEntity<Map<String, Object>>> health() {
        return buildHealthResponse()
            .map(ResponseEntity::ok)
            .doOnError(error -> log.error("Health check failed: {}", error.getMessage()))
            .onErrorReturn(ResponseEntity.status(503).body(Map.of(
                "status", "DOWN",
                "error", "Health check failed"
            )));
    }

    @GetMapping("/live")
    public Mono<ResponseEntity<Map<String, Object>>> liveness() {
        Map<String, Object> response = Map.of(
            "status", "UP",
            "timestamp", Instant.now(),
            "uptime", Instant.now().toEpochMilli() - startTime.toEpochMilli()
        );
        return Mono.just(ResponseEntity.ok(response));
    }

    @GetMapping("/ready")
    public Mono<ResponseEntity<Map<String, Object>>> readiness() {
        return checkMonicaConnectivity()
            .map(monicaHealthy -> {
                Map<String, Object> response = new HashMap<>();
                response.put("timestamp", Instant.now());
                
                if (monicaHealthy) {
                    response.put("status", "UP");
                    return ResponseEntity.ok(response);
                } else {
                    response.put("status", "DOWN");
                    response.put("details", "MonicaHQ API not accessible");
                    return ResponseEntity.status(503).body(response);
                }
            })
            .onErrorResume(error -> {
                Map<String, Object> response = Map.of(
                    "status", "DOWN",
                    "timestamp", Instant.now(),
                    "error", error.getMessage()
                );
                return Mono.just(ResponseEntity.status(503).body(response));
            });
    }

    @GetMapping("/detailed")
    public Mono<ResponseEntity<Map<String, Object>>> detailedHealth() {
        LoggingConfig.McpLoggingContext.setServiceContext("HealthController", "detailedHealth");
        
        return buildDetailedHealthResponse()
            .map(ResponseEntity::ok)
            .doFinally(signalType -> LoggingConfig.McpLoggingContext.clearMcpContext());
    }

    private Mono<Map<String, Object>> buildHealthResponse() {
        return checkMonicaConnectivity()
            .map(monicaHealthy -> {
                Map<String, Object> response = new HashMap<>();
                response.put("status", monicaHealthy ? "UP" : "DOWN");
                response.put("timestamp", Instant.now());
                response.put("components", Map.of(
                    "monicaHQ", monicaHealthy ? "UP" : "DOWN",
                    "webSocket", "UP", // WebSocket is always available
                    "toolRegistry", toolRegistry != null ? "UP" : "DOWN"
                ));
                return response;
            })
            .onErrorReturn(Map.of(
                "status", "DOWN",
                "timestamp", Instant.now(),
                "error", "Health check failed"
            ));
    }

    private Mono<Map<String, Object>> buildDetailedHealthResponse() {
        return checkMonicaConnectivity()
            .map(monicaHealthy -> {
                Map<String, Object> response = new HashMap<>();
                response.put("status", monicaHealthy ? "UP" : "DOWN");
                response.put("timestamp", Instant.now());
                response.put("uptime", Instant.now().toEpochMilli() - startTime.toEpochMilli());
                response.put("requestCount", requestCount.incrementAndGet());
                
                // Component health
                Map<String, Object> components = new HashMap<>();
                components.put("monicaHQ", Map.of(
                    "status", monicaHealthy ? "UP" : "DOWN",
                    "description", "MonicaHQ API connectivity"
                ));
                
                components.put("webSocket", Map.of(
                    "status", "UP",
                    "activeSessions", webSocketController.getActiveSessionCount(),
                    "description", "MCP WebSocket endpoint"
                ));
                
                components.put("toolRegistry", Map.of(
                    "status", toolRegistry != null ? "UP" : "DOWN",
                    "registeredTools", toolRegistry != null ? toolRegistry.getAllTools().size() : 0,
                    "description", "MCP tool registry"
                ));
                
                response.put("components", components);
                
                // System metrics
                Runtime runtime = Runtime.getRuntime();
                Map<String, Object> metrics = new HashMap<>();
                metrics.put("memory", Map.of(
                    "max", runtime.maxMemory(),
                    "total", runtime.totalMemory(),
                    "free", runtime.freeMemory(),
                    "used", runtime.totalMemory() - runtime.freeMemory()
                ));
                metrics.put("processors", runtime.availableProcessors());
                
                response.put("metrics", metrics);
                
                return response;
            })
            .onErrorReturn(Map.of(
                "status", "DOWN",
                "timestamp", Instant.now(),
                "error", "Detailed health check failed"
            ));
    }

    private Mono<Boolean> checkMonicaConnectivity() {
        LoggingConfig.McpLoggingContext.setServiceContext("HealthController", "checkMonicaConnectivity");
        
        return monicaClient.get("/me", null)
            .map(response -> {
                log.debug("MonicaHQ health check successful: {}", response);
                return true;
            })
            .onErrorResume(error -> {
                log.warn("MonicaHQ health check failed: {}", error.getMessage());
                return Mono.just(false);
            })
            .doFinally(signalType -> LoggingConfig.McpLoggingContext.clearMcpContext());
    }

    @GetMapping("/ping")
    public Mono<ResponseEntity<Map<String, Object>>> ping() {
        Map<String, Object> response = Map.of(
            "message", "pong",
            "timestamp", Instant.now(),
            "correlationId", LoggingConfig.getCorrelationId()
        );
        return Mono.just(ResponseEntity.ok(response));
    }
}