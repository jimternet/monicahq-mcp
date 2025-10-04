package com.monicahq.mcp.util;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.boot.test.context.TestComponent;
import org.springframework.test.context.TestPropertySource;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.Predicate;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Comprehensive STDIO test utility class for MCP protocol testing.
 * Provides methods to simulate STDIO communication, validate MCP protocol messages,
 * and support both synchronous and asynchronous message handling.
 * 
 * Features:
 * - JSON-RPC 2.0 message construction and validation
 * - STDIO process management with timeout handling
 * - Asynchronous message handling with CompletableFuture
 * - Error scenario testing utilities
 * - MCP protocol-specific validation helpers
 * - Spring Boot test integration
 * 
 * Usage Example for Integration Testing:
 * <pre>
 * {@code
 * @SpringBootTest
 * @TestPropertySource(properties = {"spring.profiles.active=test"})
 * public class MyMcpIntegrationTest {
 *     private StdioTestHelper stdioHelper;
 *     
 *     @BeforeEach
 *     void setUp() throws Exception {
 *         // Set required environment variables for MonicaHQ client
 *         System.setProperty("MONICA_API_URL", "https://app.monicahq.com/api");
 *         System.setProperty("MONICA_API_TOKEN", "your-test-token");
 *         
 *         stdioHelper = new StdioTestHelper();
 *         stdioHelper.initialize(); // Starts MCP server process
 *     }
 *     
 *     @AfterEach
 *     void tearDown() throws Exception {
 *         stdioHelper.cleanup(); // Stops MCP server process
 *     }
 *     
 *     @Test
 *     void testMcpCommunication() throws Exception {
 *         // Test MCP initialization
 *         JsonNode initResponse = stdioHelper.sendInitialize();
 *         stdioHelper.validateInitializeResponse(initResponse);
 *         
 *         // Test tools listing
 *         JsonNode toolsResponse = stdioHelper.sendListTools();
 *         stdioHelper.validateToolsListResponse(toolsResponse);
 *         stdioHelper.assertToolExists(toolsResponse, "contact_create");
 *         
 *         // Test tool call
 *         Map<String, Object> args = Map.of("firstName", "John", "lastName", "Doe");
 *         JsonNode response = stdioHelper.sendToolCall("contact_create", args);
 *         stdioHelper.validateToolCallResponse(response);
 *         
 *         // Test error scenarios
 *         JsonNode errorResponse = stdioHelper.sendToolCall("invalid_tool", Map.of());
 *         stdioHelper.validateErrorResponse(errorResponse, StdioTestHelper.METHOD_NOT_FOUND);
 *     }
 * }
 * }
 * </pre>
 * 
 * For unit testing of MCP message validation without starting the server,
 * see {@link StdioTestHelperUnitTest} for examples.
 */
@TestComponent
@TestPropertySource(properties = {
    "spring.profiles.active=test"
})
public class StdioTestHelper {

    private final ObjectMapper objectMapper;
    private Process mcpProcess;
    private PrintWriter stdinWriter;
    private BufferedReader stdoutReader;
    private BufferedReader stderrReader;
    private volatile boolean isConnected = false;
    private final ScheduledExecutorService executorService;
    private final Map<Integer, CompletableFuture<JsonNode>> pendingRequests;
    private final List<String> stderrBuffer;
    
    // Default timeouts
    private static final long DEFAULT_TIMEOUT_MS = 5000;
    private static final long SERVER_STARTUP_TIMEOUT_MS = 10000;
    private static final long SHUTDOWN_TIMEOUT_MS = 5000;
    
    // JSON-RPC error codes
    public static final int PARSE_ERROR = -32700;
    public static final int INVALID_REQUEST = -32600;
    public static final int METHOD_NOT_FOUND = -32601;
    public static final int INVALID_PARAMS = -32602;
    public static final int INTERNAL_ERROR = -32603;

    public StdioTestHelper() {
        this.objectMapper = new ObjectMapper();
        this.executorService = Executors.newScheduledThreadPool(4);
        this.pendingRequests = new ConcurrentHashMap<>();
        this.stderrBuffer = Collections.synchronizedList(new ArrayList<>());
    }

    /**
     * Initialize the MCP STDIO server connection
     */
    public void initialize() throws Exception {
        startMcpStdioServer();
        startAsyncResponseHandler();
        
        // Send MCP initialize message
        JsonNode initResponse = sendInitialize();
        validateInitializeResponse(initResponse);
    }

    /**
     * Cleanup resources and shutdown the MCP server
     */
    public void cleanup() throws Exception {
        isConnected = false;
        
        if (stdinWriter != null) {
            stdinWriter.close();
        }
        
        if (mcpProcess != null) {
            mcpProcess.destroyForcibly();
            mcpProcess.waitFor(SHUTDOWN_TIMEOUT_MS, TimeUnit.MILLISECONDS);
        }
        
        executorService.shutdown();
        if (!executorService.awaitTermination(SHUTDOWN_TIMEOUT_MS, TimeUnit.MILLISECONDS)) {
            executorService.shutdownNow();
        }
    }

    /**
     * Send MCP initialize message
     */
    public JsonNode sendInitialize() throws Exception {
        return sendInitialize(Map.of());
    }

    /**
     * Send MCP initialize message with custom capabilities
     */
    public JsonNode sendInitialize(Map<String, Object> capabilities) throws Exception {
        Map<String, Object> initRequest = createMcpRequest(
            "initialize",
            Map.of(
                "protocolVersion", "2024-11-05",
                "capabilities", capabilities,
                "clientInfo", Map.of(
                    "name", "junit-test-client",
                    "version", "1.0.0"
                )
            ),
            1
        );
        
        return sendRequestSynchronous(initRequest, DEFAULT_TIMEOUT_MS);
    }

    /**
     * Send MCP tools/list request
     */
    public JsonNode sendListTools() throws Exception {
        return sendListTools(generateRequestId());
    }

    /**
     * Send MCP tools/list request with specific ID
     */
    public JsonNode sendListTools(int requestId) throws Exception {
        Map<String, Object> listRequest = createMcpRequest("tools/list", Map.of(), requestId);
        return sendRequestSynchronous(listRequest, DEFAULT_TIMEOUT_MS);
    }

    /**
     * Send MCP tools/call request
     */
    public JsonNode sendToolCall(String toolName, Map<String, Object> arguments) throws Exception {
        return sendToolCall(toolName, arguments, generateRequestId());
    }

    /**
     * Send MCP tools/call request with specific ID
     */
    public JsonNode sendToolCall(String toolName, Map<String, Object> arguments, int requestId) throws Exception {
        Map<String, Object> toolRequest = createMcpRequest(
            "tools/call",
            Map.of(
                "name", toolName,
                "arguments", arguments
            ),
            requestId
        );
        
        return sendRequestSynchronous(toolRequest, DEFAULT_TIMEOUT_MS);
    }

    /**
     * Send tool call asynchronously
     */
    public CompletableFuture<JsonNode> sendToolCallAsync(String toolName, Map<String, Object> arguments) {
        return sendToolCallAsync(toolName, arguments, generateRequestId());
    }

    /**
     * Send tool call asynchronously with specific ID
     */
    public CompletableFuture<JsonNode> sendToolCallAsync(String toolName, Map<String, Object> arguments, int requestId) {
        Map<String, Object> toolRequest = createMcpRequest(
            "tools/call",
            Map.of(
                "name", toolName,
                "arguments", arguments
            ),
            requestId
        );
        
        return sendRequestAsynchronous(toolRequest, DEFAULT_TIMEOUT_MS);
    }

    /**
     * Send raw MCP request synchronously
     */
    public JsonNode sendRequestSynchronous(Map<String, Object> request, long timeoutMs) throws Exception {
        CompletableFuture<JsonNode> future = sendRequestAsynchronous(request, timeoutMs);
        return future.get(timeoutMs, TimeUnit.MILLISECONDS);
    }

    /**
     * Send raw MCP request asynchronously
     */
    public CompletableFuture<JsonNode> sendRequestAsynchronous(Map<String, Object> request, long timeoutMs) {
        if (!isConnected) {
            return CompletableFuture.failedFuture(new IllegalStateException("MCP server not connected"));
        }

        try {
            String requestJson = objectMapper.writeValueAsString(request);
            Integer requestId = (Integer) request.get("id");
            
            CompletableFuture<JsonNode> future = new CompletableFuture<>();
            if (requestId != null) {
                pendingRequests.put(requestId, future);
                
                // Set timeout
                executorService.schedule(() -> {
                    if (pendingRequests.remove(requestId) != null) {
                        future.completeExceptionally(new TimeoutException("Request timed out after " + timeoutMs + "ms"));
                    }
                }, timeoutMs, TimeUnit.MILLISECONDS);
            }
            
            // Send request
            stdinWriter.println(requestJson);
            stdinWriter.flush();
            
            return future;
        } catch (Exception e) {
            return CompletableFuture.failedFuture(e);
        }
    }

    /**
     * Send invalid JSON to test error handling
     */
    public void sendInvalidJson() throws Exception {
        if (!isConnected) {
            throw new IllegalStateException("MCP server not connected");
        }
        
        stdinWriter.println("{ invalid json }");
        stdinWriter.flush();
    }

    /**
     * Send malformed MCP request to test error handling
     */
    public JsonNode sendMalformedRequest() throws Exception {
        Map<String, Object> malformedRequest = Map.of(
            "jsonrpc", "1.0", // Wrong version
            "method", "invalid_method",
            "id", generateRequestId()
        );
        
        return sendRequestSynchronous(malformedRequest, DEFAULT_TIMEOUT_MS);
    }

    /**
     * Create standard MCP JSON-RPC request
     */
    public Map<String, Object> createMcpRequest(String method, Map<String, Object> params, int id) {
        return Map.of(
            "jsonrpc", "2.0",
            "method", method,
            "params", params,
            "id", id
        );
    }

    /**
     * Create MCP notification (no response expected)
     */
    public Map<String, Object> createMcpNotification(String method, Map<String, Object> params) {
        return Map.of(
            "jsonrpc", "2.0",
            "method", method,
            "params", params
        );
    }

    // Validation Methods

    /**
     * Validate MCP initialize response
     */
    public void validateInitializeResponse(JsonNode response) {
        validateJsonRpcResponse(response);
        assertTrue(response.has("result"), "Initialize response should have result field");
        
        JsonNode result = response.get("result");
        assertTrue(result.has("protocolVersion"), "Initialize result should have protocolVersion");
        assertTrue(result.has("capabilities"), "Initialize result should have capabilities");
        assertTrue(result.has("serverInfo"), "Initialize result should have serverInfo");
        
        JsonNode serverInfo = result.get("serverInfo");
        assertTrue(serverInfo.has("name"), "ServerInfo should have name");
        assertTrue(serverInfo.has("version"), "ServerInfo should have version");
    }

    /**
     * Validate tools/list response
     */
    public void validateToolsListResponse(JsonNode response) {
        validateJsonRpcResponse(response);
        assertTrue(response.has("result"), "Tools list response should have result field");
        
        JsonNode result = response.get("result");
        assertTrue(result.has("tools"), "Tools list result should have tools array");
        assertTrue(result.get("tools").isArray(), "Tools should be an array");
    }

    /**
     * Validate tools/call response
     */
    public void validateToolCallResponse(JsonNode response) {
        validateJsonRpcResponse(response);
        assertTrue(response.has("result"), "Tool call response should have result field");
        
        JsonNode result = response.get("result");
        assertTrue(result.has("content"), "Tool call result should have content");
    }

    /**
     * Validate JSON-RPC response structure
     */
    public void validateJsonRpcResponse(JsonNode response) {
        assertNotNull(response, "Response should not be null");
        assertTrue(response.has("jsonrpc"), "Response should have jsonrpc field");
        assertEquals("2.0", response.get("jsonrpc").asText(), "Response should use JSON-RPC 2.0");
        assertTrue(response.has("id"), "Response should have id field");
        
        // Should have either result or error, but not both
        boolean hasResult = response.has("result");
        boolean hasError = response.has("error");
        assertTrue(hasResult || hasError, "Response should have either result or error");
        assertFalse(hasResult && hasError, "Response should not have both result and error");
    }

    /**
     * Validate JSON-RPC error response
     */
    public void validateErrorResponse(JsonNode response, int expectedCode) {
        validateJsonRpcResponse(response);
        assertTrue(response.has("error"), "Error response should have error field");
        
        JsonNode error = response.get("error");
        assertTrue(error.has("code"), "Error should have code field");
        assertTrue(error.has("message"), "Error should have message field");
        assertEquals(expectedCode, error.get("code").asInt(), "Error code should match expected");
    }

    /**
     * Assert that tool exists in tools list
     */
    public void assertToolExists(JsonNode toolsListResponse, String toolName) {
        validateToolsListResponse(toolsListResponse);
        
        JsonNode tools = toolsListResponse.get("result").get("tools");
        boolean found = false;
        for (JsonNode tool : tools) {
            if (toolName.equals(tool.get("name").asText())) {
                found = true;
                break;
            }
        }
        assertTrue(found, "Tool '" + toolName + "' should exist in tools list");
    }

    /**
     * Assert that tool has required properties
     */
    public void assertToolProperties(JsonNode toolsListResponse, String toolName, String... requiredProperties) {
        JsonNode tool = findTool(toolsListResponse, toolName);
        assertNotNull(tool, "Tool '" + toolName + "' not found");
        
        for (String property : requiredProperties) {
            assertTrue(tool.has(property), "Tool '" + toolName + "' should have property '" + property + "'");
        }
    }

    /**
     * Find tool in tools list response
     */
    public JsonNode findTool(JsonNode toolsListResponse, String toolName) {
        validateToolsListResponse(toolsListResponse);
        
        JsonNode tools = toolsListResponse.get("result").get("tools");
        for (JsonNode tool : tools) {
            if (toolName.equals(tool.get("name").asText())) {
                return tool;
            }
        }
        return null;
    }

    /**
     * Wait for stderr output matching predicate
     */
    public boolean waitForStderrOutput(Predicate<String> matcher, long timeoutMs) {
        long startTime = System.currentTimeMillis();
        while (System.currentTimeMillis() - startTime < timeoutMs) {
            synchronized (stderrBuffer) {
                for (String line : stderrBuffer) {
                    if (matcher.test(line)) {
                        return true;
                    }
                }
            }
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                return false;
            }
        }
        return false;
    }

    /**
     * Get all stderr output
     */
    public List<String> getStderrOutput() {
        synchronized (stderrBuffer) {
            return new ArrayList<>(stderrBuffer);
        }
    }

    /**
     * Check if MCP server process is alive
     */
    public boolean isServerAlive() {
        return mcpProcess != null && mcpProcess.isAlive();
    }

    /**
     * Generate unique request ID
     */
    public int generateRequestId() {
        return (int) (System.currentTimeMillis() % Integer.MAX_VALUE);
    }

    // Private helper methods

    private void startMcpStdioServer() throws Exception {
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
        
        // Start stderr monitoring
        startStderrMonitoring();
        
        // Wait for server to be ready
        boolean ready = waitForStderrOutput(
            line -> line.contains("Starting MCP Server in stdio mode"), 
            SERVER_STARTUP_TIMEOUT_MS
        );
        
        if (!ready) {
            throw new RuntimeException("MCP server failed to start within " + SERVER_STARTUP_TIMEOUT_MS + "ms");
        }
        
        isConnected = true;
    }

    private void startAsyncResponseHandler() {
        executorService.submit(() -> {
            try {
                String line;
                while (isConnected && (line = stdoutReader.readLine()) != null) {
                    try {
                        JsonNode response = objectMapper.readTree(line);
                        if (response.has("id")) {
                            Integer requestId = response.get("id").asInt();
                            CompletableFuture<JsonNode> future = pendingRequests.remove(requestId);
                            if (future != null) {
                                future.complete(response);
                            }
                        }
                    } catch (Exception e) {
                        // Log error but continue processing
                        System.err.println("Error processing response: " + e.getMessage());
                    }
                }
            } catch (IOException e) {
                if (isConnected) {
                    System.err.println("Response handler error: " + e.getMessage());
                }
            }
        });
    }

    private void startStderrMonitoring() {
        executorService.submit(() -> {
            try {
                String line;
                while (isConnected && (line = stderrReader.readLine()) != null) {
                    synchronized (stderrBuffer) {
                        stderrBuffer.add(line);
                    }
                }
            } catch (IOException e) {
                if (isConnected) {
                    System.err.println("Stderr monitoring error: " + e.getMessage());
                }
            }
        });
    }
}