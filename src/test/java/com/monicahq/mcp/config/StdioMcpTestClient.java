package com.monicahq.mcp.config;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.test.context.TestPropertySource;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

/**
 * Test client for MCP stdio communication.
 * Provides methods to send MCP JSON-RPC messages and receive responses
 * via stdin/stdout communication with McpStdioServer.
 */
@TestPropertySource(properties = {
    "spring.profiles.active=test"
})
public class StdioMcpTestClient {

    private Process mcpProcess;
    private PrintWriter stdinWriter;
    private BufferedReader stdoutReader;
    private BufferedReader stderrReader;
    private final ObjectMapper objectMapper = new ObjectMapper();
    
    @BeforeEach
    public void setup() throws Exception {
        startMcpStdioServer();
    }
    
    @AfterEach  
    public void teardown() throws Exception {
        if (mcpProcess != null) {
            stdinWriter.close();
            mcpProcess.destroyForcibly();
            int timeoutSeconds = System.getenv("CI") != null ? 10 : 5;
            mcpProcess.waitFor(timeoutSeconds, TimeUnit.SECONDS);
        }
    }
    
    private void startMcpStdioServer() throws Exception {
        // Build classpath from current test environment
        String classpath = System.getProperty("java.class.path");
        String javaHome = System.getProperty("java.home");
        
        ProcessBuilder pb = new ProcessBuilder(
            javaHome + "/bin/java",
            "-cp", classpath,
            "-Dspring.profiles.active=test",
            "-Dspring.main.banner-mode=off",
            "-Dlogging.level.root=ERROR",
            "com.monicahq.mcp.McpStdioServer"
        );
        
        pb.environment().put("MCP_STDIO_MODE", "true");
        
        mcpProcess = pb.start();
        
        // Setup streams
        stdinWriter = new PrintWriter(
            new OutputStreamWriter(mcpProcess.getOutputStream(), StandardCharsets.UTF_8), 
            true
        );
        
        stdoutReader = new BufferedReader(
            new InputStreamReader(mcpProcess.getInputStream(), StandardCharsets.UTF_8)
        );
        
        stderrReader = new BufferedReader(
            new InputStreamReader(mcpProcess.getErrorStream(), StandardCharsets.UTF_8)
        );
        
        // Wait for server to be ready (look for ready message on stderr)
        waitForServerReady();
    }
    
    private void waitForServerReady() throws Exception {
        CompletableFuture<Void> readyFuture = CompletableFuture.runAsync(() -> {
            try {
                String line;
                while ((line = stderrReader.readLine()) != null) {
                    if (line.contains("MCP stdio server ready")) {
                        return;
                    }
                }
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });
        
        // Wait up to 30 seconds for server to be ready (CI environments are slower)
        int timeoutSeconds = System.getenv("CI") != null ? 30 : 10;
        readyFuture.get(timeoutSeconds, TimeUnit.SECONDS);
    }
    
    /**
     * Send MCP JSON-RPC request and wait for response
     */
    public JsonNode sendMcpRequest(Map<String, Object> request) throws Exception {
        // Use longer timeout for CI environments
        long timeoutMs = System.getenv("CI") != null ? 15000 : 5000;
        return sendMcpRequest(request, timeoutMs);
    }
    
    /**
     * Send MCP JSON-RPC request with custom timeout
     */
    public JsonNode sendMcpRequest(Map<String, Object> request, long timeoutMs) throws Exception {
        String requestJson = objectMapper.writeValueAsString(request);
        
        // Send request
        stdinWriter.println(requestJson);
        stdinWriter.flush();
        
        // Read response with timeout
        CompletableFuture<String> responseFuture = CompletableFuture.supplyAsync(() -> {
            try {
                return stdoutReader.readLine();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });
        
        String responseJson = responseFuture.get(timeoutMs, TimeUnit.MILLISECONDS);
        
        if (responseJson == null) {
            throw new RuntimeException("No response received from MCP server");
        }
        
        return objectMapper.readTree(responseJson);
    }
    
    /**
     * Send MCP initialize message
     */
    public JsonNode initialize() throws Exception {
        Map<String, Object> initRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "initialize", 
            "params", Map.of(
                "protocolVersion", "2024-11-05",
                "capabilities", Map.of(),
                "clientInfo", Map.of(
                    "name", "junit-test-client",
                    "version", "1.0.0"
                )
            ),
            "id", 1
        );
        
        return sendMcpRequest(initRequest);
    }
    
    /**
     * Send MCP tools/call request  
     */
    public JsonNode callTool(String toolName, Map<String, Object> arguments, int requestId) throws Exception {
        Map<String, Object> toolRequest = Map.of(
            "jsonrpc", "2.0", 
            "method", "tools/call",
            "params", Map.of(
                "name", toolName,
                "arguments", arguments
            ),
            "id", requestId
        );
        
        return sendMcpRequest(toolRequest);
    }
    
    /**
     * Send MCP tools/list request
     */
    public JsonNode listTools(int requestId) throws Exception {
        Map<String, Object> listRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/list", 
            "params", Map.of(),
            "id", requestId
        );
        
        return sendMcpRequest(listRequest);
    }
    
    /**
     * Check if process is still alive
     */
    public boolean isAlive() {
        return mcpProcess != null && mcpProcess.isAlive();
    }
    
    /**
     * Get stderr output (for debugging)
     */
    public String getStderrOutput() throws IOException {
        StringBuilder sb = new StringBuilder();
        if (stderrReader.ready()) {
            String line;
            while ((line = stderrReader.readLine()) != null) {
                sb.append(line).append("\n");
            }
        }
        return sb.toString();
    }
}