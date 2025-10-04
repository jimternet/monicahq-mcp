package com.monicahq.mcp.config;

import com.monicahq.mcp.util.DebugInfoProvider;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.HealthIndicator;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.event.EventListener;
import org.springframework.core.env.Environment;

import java.util.Map;

/**
 * Configuration class for MCP debug features
 * Provides debug endpoints, health checks, and monitoring capabilities
 */
@Configuration
@RequiredArgsConstructor
@Slf4j
public class McpDebugConfiguration {

    private final Environment environment;
    private final DebugInfoProvider debugInfoProvider;
    
    private static final boolean DEBUG_MODE = Boolean.parseBoolean(System.getenv().getOrDefault("MCP_DEBUG", "false"));

    /**
     * Health indicator for MCP server status
     */
    @Bean
    public HealthIndicator mcpHealthIndicator() {
        return () -> {
            try {
                Map<String, Object> healthCheck = debugInfoProvider.getHealthCheck();
                
                boolean isHealthy = (Boolean) healthCheck.getOrDefault("memory_healthy", false) &&
                                  (Boolean) healthCheck.getOrDefault("operation_healthy", false) &&
                                  (Boolean) healthCheck.getOrDefault("environment_healthy", false);
                
                Health.Builder builder = isHealthy ? Health.up() : Health.down();
                
                healthCheck.forEach(builder::withDetail);
                
                return builder.build();
                
            } catch (Exception e) {
                return Health.down()
                    .withDetail("error", e.getMessage())
                    .withDetail("debug_mode", DEBUG_MODE)
                    .build();
            }
        };
    }

    /**
     * Application startup event handler
     */
    @EventListener(ApplicationReadyEvent.class)
    public void onApplicationReady() {
        if (DEBUG_MODE) {
            log.info("[MCP-DEBUG] MonicaHQ MCP Server started in DEBUG mode");
            log.info("[MCP-DEBUG] Debug capabilities enabled");
            
            // Log system information
            Map<String, Object> systemInfo = debugInfoProvider.getSystemInfo();
            log.info("[MCP-DEBUG] System Info: {}", systemInfo);
            
            // Log environment information
            Map<String, Object> envInfo = debugInfoProvider.getEnvironmentInfo();
            log.info("[MCP-DEBUG] Environment: {}", envInfo);
            
            // Record startup metric
            debugInfoProvider.recordMetric("server_startup_time", System.currentTimeMillis());
            debugInfoProvider.recordMetric("debug_mode_enabled", true);
            
        } else {
            log.info("MonicaHQ MCP Server started in production mode");
            debugInfoProvider.recordMetric("debug_mode_enabled", false);
        }
        
        // Record basic server info
        debugInfoProvider.recordMetric("server_version", "0.1.0");
        debugInfoProvider.recordMetric("protocol_version", "2024-11-05");
        
        // Check environment configuration
        boolean apiUrlConfigured = environment.getProperty("MONICA_API_URL") != null || 
                                 System.getenv("MONICA_API_URL") != null;
        boolean apiTokenConfigured = environment.getProperty("MONICA_API_TOKEN") != null || 
                                   System.getenv("MONICA_API_TOKEN") != null;
        
        debugInfoProvider.recordMetric("api_url_configured", apiUrlConfigured);
        debugInfoProvider.recordMetric("api_token_configured", apiTokenConfigured);
        
        if (!apiUrlConfigured || !apiTokenConfigured) {
            log.warn("MonicaHQ API credentials not fully configured. Some operations may fail.");
        }
        
        log.info("MonicaHQ MCP Server ready to accept connections");
    }

    /**
     * Debug endpoint configuration bean
     */
    @Bean
    public McpDebugEndpoint mcpDebugEndpoint() {
        return new McpDebugEndpoint();
    }

    /**
     * Inner class for debug endpoint
     */
    public class McpDebugEndpoint {
        
        /**
         * Get debug information
         */
        public Map<String, Object> getDebugInfo() {
            if (!DEBUG_MODE) {
                return Map.of(
                    "debug_mode", false,
                    "message", "Debug mode is disabled. Set MCP_DEBUG=true to enable debug information."
                );
            }
            
            return debugInfoProvider.getDebugInfo();
        }
        
        /**
         * Get performance snapshot
         */
        public Map<String, Object> getPerformanceSnapshot() {
            return debugInfoProvider.getPerformanceSnapshot();
        }
        
        /**
         * Get health status
         */
        public Map<String, Object> getHealthStatus() {
            return debugInfoProvider.getHealthCheck();
        }
        
        /**
         * Reset debug statistics
         */
        public Map<String, Object> resetStatistics() {
            if (!DEBUG_MODE) {
                return Map.of(
                    "success", false,
                    "message", "Statistics reset is only available in debug mode"
                );
            }
            
            debugInfoProvider.resetStatistics();
            return Map.of(
                "success", true,
                "message", "Debug statistics have been reset",
                "timestamp", System.currentTimeMillis()
            );
        }
    }

    /**
     * Performance monitoring configuration
     */
    @Bean
    public McpPerformanceMonitor mcpPerformanceMonitor() {
        return new McpPerformanceMonitor();
    }

    /**
     * Inner class for performance monitoring
     */
    public class McpPerformanceMonitor {
        
        private static final long MEMORY_WARNING_THRESHOLD = 90; // 90% memory usage
        private static final double ERROR_RATE_WARNING_THRESHOLD = 0.1; // 10% error rate
        
        /**
         * Check if performance is within acceptable limits
         */
        public boolean isPerformanceHealthy() {
            Map<String, Object> snapshot = debugInfoProvider.getPerformanceSnapshot();
            
            // Check memory usage
            Object memoryUsage = snapshot.get("memory_usage_percent");
            if (memoryUsage instanceof Number) {
                double memPercent = ((Number) memoryUsage).doubleValue();
                if (memPercent > MEMORY_WARNING_THRESHOLD) {
                    if (DEBUG_MODE) {
                        log.warn("[MCP-DEBUG] High memory usage detected: {}%", memPercent);
                    }
                    return false;
                }
            }
            
            // Check error rate
            Object errorRate = snapshot.get("recent_success_rate");
            if (errorRate instanceof Number) {
                double successRate = ((Number) errorRate).doubleValue();
                if (successRate < (1.0 - ERROR_RATE_WARNING_THRESHOLD)) {
                    if (DEBUG_MODE) {
                        log.warn("[MCP-DEBUG] High error rate detected: {}% success", successRate * 100);
                    }
                    return false;
                }
            }
            
            return true;
        }
        
        /**
         * Get performance recommendations
         */
        public Map<String, Object> getPerformanceRecommendations() {
            Map<String, Object> snapshot = debugInfoProvider.getPerformanceSnapshot();
            Map<String, Object> recommendations = new java.util.HashMap<>();
            
            // Memory recommendations
            Object memoryUsage = snapshot.get("memory_usage_percent");
            if (memoryUsage instanceof Number) {
                double memPercent = ((Number) memoryUsage).doubleValue();
                if (memPercent > 80) {
                    recommendations.put("memory", 
                        "Consider increasing JVM heap size (-Xmx) or optimizing memory usage");
                } else if (memPercent < 30) {
                    recommendations.put("memory", 
                        "Memory usage is optimal");
                }
            }
            
            // Operations recommendations
            Object totalOps = snapshot.get("total_operations");
            if (totalOps instanceof Number) {
                long ops = ((Number) totalOps).longValue();
                if (ops == 0) {
                    recommendations.put("usage", 
                        "No operations detected. Ensure the server is properly connected to clients.");
                } else if (ops > 10000) {
                    recommendations.put("usage", 
                        "High operation volume detected. Consider enabling connection pooling.");
                }
            }
            
            return recommendations;
        }
    }

    /**
     * Debug configuration summary
     */
    @Bean
    public McpDebugSummary mcpDebugSummary() {
        return new McpDebugSummary();
    }

    /**
     * Inner class for debug configuration summary
     */
    public class McpDebugSummary {
        
        public Map<String, Object> getSummary() {
            Map<String, Object> summary = new java.util.HashMap<>();
            
            summary.put("debug_mode", DEBUG_MODE);
            summary.put("server_version", "0.1.0");
            summary.put("protocol_version", "2024-11-05");
            summary.put("spring_profiles", environment.getActiveProfiles());
            
            // Debug features available
            if (DEBUG_MODE) {
                summary.put("debug_features", Map.of(
                    "detailed_logging", true,
                    "performance_monitoring", true,
                    "health_checks", true,
                    "statistics_tracking", true,
                    "debug_endpoints", true
                ));
            } else {
                summary.put("debug_features", Map.of(
                    "detailed_logging", false,
                    "basic_health_check", true
                ));
            }
            
            // Environment status
            summary.put("environment_status", Map.of(
                "monica_api_url", System.getenv("MONICA_API_URL") != null,
                "monica_api_token", System.getenv("MONICA_API_TOKEN") != null,
                "java_version", System.getProperty("java.version"),
                "working_directory", System.getProperty("user.dir")
            ));
            
            return summary;
        }
    }
}