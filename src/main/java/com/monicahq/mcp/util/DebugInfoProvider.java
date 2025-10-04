package com.monicahq.mcp.util;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.lang.management.ManagementFactory;
import java.lang.management.MemoryMXBean;
import java.lang.management.RuntimeMXBean;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

/**
 * Utility class for providing debug information about the MCP server
 * Collects system metrics, environment details, and operational statistics
 */
@Component
@Slf4j
public class DebugInfoProvider {

    private static final boolean DEBUG_MODE = Boolean.parseBoolean(System.getenv().getOrDefault("MCP_DEBUG", "false"));
    private static final DateTimeFormatter TIMESTAMP_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS");
    
    private final Map<String, Object> serverMetrics = new HashMap<>();
    private final Map<String, Long> operationCounts = new HashMap<>();
    private final Map<String, Long> operationDurations = new HashMap<>();
    private final List<String> recentOperations = new ArrayList<>();
    private volatile long serverStartTime = System.currentTimeMillis();

    /**
     * Gets comprehensive debug information about the server state
     * @return Map containing all debug information
     */
    public Map<String, Object> getDebugInfo() {
        Map<String, Object> debugInfo = new HashMap<>();
        
        debugInfo.put("timestamp", LocalDateTime.now().format(TIMESTAMP_FORMAT));
        debugInfo.put("debug_mode", DEBUG_MODE);
        debugInfo.put("server_uptime_ms", System.currentTimeMillis() - serverStartTime);
        debugInfo.put("system_info", getSystemInfo());
        debugInfo.put("memory_info", getMemoryInfo());
        debugInfo.put("environment_info", getEnvironmentInfo());
        debugInfo.put("operation_statistics", getOperationStatistics());
        debugInfo.put("recent_operations", getRecentOperations());
        debugInfo.put("server_metrics", new HashMap<>(serverMetrics));
        
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Generated debug info with {} categories", debugInfo.size());
        }
        
        return debugInfo;
    }

    /**
     * Gets basic system information
     * @return Map containing system details
     */
    public Map<String, Object> getSystemInfo() {
        Map<String, Object> systemInfo = new HashMap<>();
        
        RuntimeMXBean runtimeBean = ManagementFactory.getRuntimeMXBean();
        
        systemInfo.put("java_version", System.getProperty("java.version"));
        systemInfo.put("java_vendor", System.getProperty("java.vendor"));
        systemInfo.put("os_name", System.getProperty("os.name"));
        systemInfo.put("os_version", System.getProperty("os.version"));
        systemInfo.put("os_arch", System.getProperty("os.arch"));
        systemInfo.put("available_processors", Runtime.getRuntime().availableProcessors());
        systemInfo.put("jvm_uptime_ms", runtimeBean.getUptime());
        systemInfo.put("jvm_start_time", new Date(runtimeBean.getStartTime()));
        
        return systemInfo;
    }

    /**
     * Gets memory usage information
     * @return Map containing memory details
     */
    public Map<String, Object> getMemoryInfo() {
        Map<String, Object> memoryInfo = new HashMap<>();
        
        MemoryMXBean memoryBean = ManagementFactory.getMemoryMXBean();
        Runtime runtime = Runtime.getRuntime();
        
        memoryInfo.put("heap_memory", Map.of(
            "used_mb", memoryBean.getHeapMemoryUsage().getUsed() / 1024 / 1024,
            "max_mb", memoryBean.getHeapMemoryUsage().getMax() / 1024 / 1024,
            "committed_mb", memoryBean.getHeapMemoryUsage().getCommitted() / 1024 / 1024
        ));
        
        memoryInfo.put("non_heap_memory", Map.of(
            "used_mb", memoryBean.getNonHeapMemoryUsage().getUsed() / 1024 / 1024,
            "max_mb", memoryBean.getNonHeapMemoryUsage().getMax() / 1024 / 1024,
            "committed_mb", memoryBean.getNonHeapMemoryUsage().getCommitted() / 1024 / 1024
        ));
        
        memoryInfo.put("runtime_memory", Map.of(
            "total_mb", runtime.totalMemory() / 1024 / 1024,
            "free_mb", runtime.freeMemory() / 1024 / 1024,
            "max_mb", runtime.maxMemory() / 1024 / 1024,
            "used_mb", (runtime.totalMemory() - runtime.freeMemory()) / 1024 / 1024
        ));
        
        return memoryInfo;
    }

    /**
     * Gets environment and configuration information
     * @return Map containing environment details
     */
    public Map<String, Object> getEnvironmentInfo() {
        Map<String, Object> envInfo = new HashMap<>();
        
        // Safe environment variables (no secrets)
        envInfo.put("mcp_debug", System.getenv().getOrDefault("MCP_DEBUG", "false"));
        envInfo.put("monica_api_url_configured", System.getenv("MONICA_API_URL") != null);
        envInfo.put("monica_api_token_configured", System.getenv("MONICA_API_TOKEN") != null);
        
        // Spring profiles
        String profiles = System.getProperty("spring.profiles.active");
        envInfo.put("spring_profiles", profiles != null ? profiles : "default");
        
        // Working directory
        envInfo.put("working_directory", System.getProperty("user.dir"));
        
        // Timezone
        envInfo.put("timezone", TimeZone.getDefault().getID());
        
        return envInfo;
    }

    /**
     * Gets operation statistics and performance metrics
     * @return Map containing operation statistics
     */
    public Map<String, Object> getOperationStatistics() {
        Map<String, Object> stats = new HashMap<>();
        
        synchronized (operationCounts) {
            stats.put("total_operations", operationCounts.values().stream().mapToLong(Long::longValue).sum());
            stats.put("operation_counts", new HashMap<>(operationCounts));
        }
        
        synchronized (operationDurations) {
            Map<String, Double> avgDurations = new HashMap<>();
            operationDurations.forEach((operation, totalDuration) -> {
                long count = operationCounts.getOrDefault(operation, 1L);
                avgDurations.put(operation, totalDuration.doubleValue() / count);
            });
            stats.put("average_durations_ms", avgDurations);
        }
        
        return stats;
    }

    /**
     * Gets list of recent operations for debugging
     * @return List of recent operation strings
     */
    public List<String> getRecentOperations() {
        synchronized (recentOperations) {
            return new ArrayList<>(recentOperations);
        }
    }

    /**
     * Records an operation for statistics and debugging
     * @param operation Operation name
     * @param durationMs Duration in milliseconds
     * @param success Whether the operation succeeded
     */
    public void recordOperation(String operation, long durationMs, boolean success) {
        String timestamp = LocalDateTime.now().format(TIMESTAMP_FORMAT);
        String operationRecord = String.format("%s: %s (%dms) - %s", 
            timestamp, operation, durationMs, success ? "SUCCESS" : "FAILED");
        
        synchronized (operationCounts) {
            operationCounts.merge(operation, 1L, Long::sum);
        }
        
        synchronized (operationDurations) {
            operationDurations.merge(operation, durationMs, Long::sum);
        }
        
        synchronized (recentOperations) {
            recentOperations.add(operationRecord);
            
            // Keep only last 50 operations
            while (recentOperations.size() > 50) {
                recentOperations.remove(0);
            }
        }
        
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Recorded operation: {}", operationRecord);
        }
    }

    /**
     * Records a custom server metric
     * @param key Metric key
     * @param value Metric value
     */
    public void recordMetric(String key, Object value) {
        synchronized (serverMetrics) {
            serverMetrics.put(key, value);
        }
        
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Recorded metric: {} = {}", key, value);
        }
    }

    /**
     * Gets current performance snapshot
     * @return Map containing current performance metrics
     */
    public Map<String, Object> getPerformanceSnapshot() {
        Map<String, Object> snapshot = new HashMap<>();
        
        snapshot.put("timestamp", LocalDateTime.now().format(TIMESTAMP_FORMAT));
        snapshot.put("uptime_ms", System.currentTimeMillis() - serverStartTime);
        
        // Memory snapshot
        Runtime runtime = Runtime.getRuntime();
        long usedMemory = runtime.totalMemory() - runtime.freeMemory();
        long maxMemory = runtime.maxMemory();
        
        snapshot.put("memory_usage_percent", (usedMemory * 100.0) / maxMemory);
        snapshot.put("memory_used_mb", usedMemory / 1024 / 1024);
        
        // Operation snapshot
        synchronized (operationCounts) {
            long totalOps = operationCounts.values().stream().mapToLong(Long::longValue).sum();
            snapshot.put("total_operations", totalOps);
            snapshot.put("operations_per_minute", calculateOperationsPerMinute());
        }
        
        // Recent errors
        synchronized (recentOperations) {
            long recentErrors = recentOperations.stream()
                .filter(op -> op.contains("FAILED"))
                .count();
            snapshot.put("recent_error_count", recentErrors);
            snapshot.put("recent_success_rate", 
                recentOperations.isEmpty() ? 1.0 : 
                (recentOperations.size() - recentErrors) / (double) recentOperations.size());
        }
        
        return snapshot;
    }

    /**
     * Gets detailed debug trace for an operation
     * @param operation Operation name
     * @param parameters Operation parameters
     * @param startTime Operation start time
     * @return Debug trace string
     */
    public String getOperationTrace(String operation, Map<String, Object> parameters, long startTime) {
        StringBuilder trace = new StringBuilder();
        
        trace.append(String.format("[DEBUG-TRACE] Operation: %s\n", operation));
        trace.append(String.format("[DEBUG-TRACE] Start Time: %s\n", 
            LocalDateTime.now().format(TIMESTAMP_FORMAT)));
        trace.append(String.format("[DEBUG-TRACE] Thread: %s\n", Thread.currentThread().getName()));
        
        if (parameters != null && !parameters.isEmpty()) {
            trace.append("[DEBUG-TRACE] Parameters:\n");
            parameters.forEach((key, value) -> 
                trace.append(String.format("  %s: %s\n", key, formatValue(value))));
        }
        
        // System state
        Runtime runtime = Runtime.getRuntime();
        trace.append(String.format("[DEBUG-TRACE] Memory: %d MB used of %d MB max\n",
            (runtime.totalMemory() - runtime.freeMemory()) / 1024 / 1024,
            runtime.maxMemory() / 1024 / 1024));
        
        return trace.toString();
    }

    /**
     * Resets all statistics and metrics
     */
    public void resetStatistics() {
        synchronized (operationCounts) {
            operationCounts.clear();
        }
        
        synchronized (operationDurations) {
            operationDurations.clear();
        }
        
        synchronized (recentOperations) {
            recentOperations.clear();
        }
        
        synchronized (serverMetrics) {
            serverMetrics.clear();
        }
        
        serverStartTime = System.currentTimeMillis();
        
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Statistics reset");
        }
    }

    /**
     * Checks if debug mode is active
     * @return true if debug mode is enabled
     */
    public boolean isDebugMode() {
        return DEBUG_MODE;
    }

    /**
     * Gets a health check summary
     * @return Map containing health check results
     */
    public Map<String, Object> getHealthCheck() {
        Map<String, Object> health = new HashMap<>();
        
        health.put("status", "UP");
        health.put("timestamp", LocalDateTime.now().format(TIMESTAMP_FORMAT));
        health.put("uptime_ms", System.currentTimeMillis() - serverStartTime);
        
        // Memory health
        Runtime runtime = Runtime.getRuntime();
        long usedMemory = runtime.totalMemory() - runtime.freeMemory();
        long maxMemory = runtime.maxMemory();
        double memoryUsagePercent = (usedMemory * 100.0) / maxMemory;
        
        health.put("memory_healthy", memoryUsagePercent < 90);
        health.put("memory_usage_percent", memoryUsagePercent);
        
        // Operation health
        synchronized (recentOperations) {
            if (!recentOperations.isEmpty()) {
                long recentErrors = recentOperations.stream()
                    .filter(op -> op.contains("FAILED"))
                    .count();
                double errorRate = recentErrors / (double) recentOperations.size();
                health.put("operation_healthy", errorRate < 0.1); // Less than 10% error rate
                health.put("error_rate", errorRate);
            } else {
                health.put("operation_healthy", true);
                health.put("error_rate", 0.0);
            }
        }
        
        // Environment health
        health.put("environment_healthy", 
            System.getenv("MONICA_API_URL") != null && 
            System.getenv("MONICA_API_TOKEN") != null);
        
        return health;
    }

    private double calculateOperationsPerMinute() {
        long uptimeMinutes = (System.currentTimeMillis() - serverStartTime) / 60000;
        if (uptimeMinutes == 0) return 0.0;
        
        synchronized (operationCounts) {
            long totalOps = operationCounts.values().stream().mapToLong(Long::longValue).sum();
            return totalOps / (double) uptimeMinutes;
        }
    }

    private String formatValue(Object value) {
        if (value == null) return "null";
        
        if (value instanceof String str) {
            return str.length() > 50 ? 
                String.format("\"%s...\" (length: %d)", str.substring(0, 47), str.length()) :
                String.format("\"%s\"", str);
        }
        
        if (value instanceof List<?> list) {
            return String.format("[%d items]", list.size());
        }
        
        if (value instanceof Map<?, ?> map) {
            return String.format("{%d fields}", map.size());
        }
        
        return value.toString();
    }
}