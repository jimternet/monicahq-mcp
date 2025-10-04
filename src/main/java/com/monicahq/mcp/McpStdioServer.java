package com.monicahq.mcp;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.monicahq.mcp.controller.McpMessageHandler;
import org.springframework.boot.SpringApplication;
import org.springframework.context.ConfigurableApplicationContext;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.util.Map;

/**
 * MCP Server for Claude Desktop - uses stdin/stdout for communication
 * This is the entry point when running as an MCP server for Claude Desktop.
 */
public class McpStdioServer {
    
    private static final boolean DEBUG_MODE = Boolean.parseBoolean(System.getenv().getOrDefault("MCP_DEBUG", "false"));

    public static void main(String[] args) {
        runStdioServer();
    }
    
    private static void runStdioServer() {
        // Use debug logging configuration if debug mode is enabled
        if (DEBUG_MODE) {
            System.setProperty("logging.config", "classpath:logback-debug.xml");
            System.setProperty("logging.level.com.monicahq.mcp", "DEBUG");
        } else {
            System.setProperty("logging.config", "classpath:logback-stdio.xml");
            System.setProperty("logging.level.com.monicahq.mcp", "ERROR"); // Only errors
        }
        
        // Start Spring Boot context without web server
        System.setProperty("spring.main.web-application-type", "none");
        System.setProperty("server.port", "-1"); // Disable web server
        System.setProperty("spring.main.banner-mode", "off"); // Disable Spring Boot banner
        System.setProperty("logging.level.root", "WARN"); // Minimal logging
        
        // Print initial message to stderr only
        if (DEBUG_MODE) {
            System.err.println("[MCP-DEBUG] Starting MCP Server in stdio mode with DEBUG logging enabled");
            System.err.println("[MCP-DEBUG] Debug mode activated via MCP_DEBUG=true environment variable");
        } else {
            System.err.println("Starting MCP Server in stdio mode for Claude Desktop");
        }
        
        try (ConfigurableApplicationContext context = SpringApplication.run(MonicaHqMcpApplication.class)) {
            McpMessageHandler messageHandler = context.getBean(McpMessageHandler.class);
            ObjectMapper objectMapper = context.getBean(ObjectMapper.class);
            
            runMessageLoop(messageHandler, objectMapper);
        } catch (Exception e) {
            System.err.println("Error in MCP stdio server: " + e.getMessage());
            System.exit(1);
        }
    }
    
    private static void runMessageLoop(McpMessageHandler messageHandler, ObjectMapper objectMapper) {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8));
             PrintWriter writer = new PrintWriter(new OutputStreamWriter(System.out, StandardCharsets.UTF_8), true)) {
            
            if (DEBUG_MODE) {
                System.err.println("[MCP-DEBUG] STDIO server ready, waiting for messages...");
            }
            
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) {
                    continue;
                }
                
                try {
                    if (DEBUG_MODE) {
                        System.err.println("[MCP-DEBUG] RECEIVED: " + line);
                    }
                    
                    // Parse JSON message
                    JsonNode message = objectMapper.readTree(line);
                    
                    // Process message through handler
                    Map<String, Object> response = messageHandler.handleMessage(message, null);
                    
                    // Send response - ONLY JSON to stdout (skip null responses for notifications)
                    if (response != null) {
                        String responseJson = objectMapper.writeValueAsString(response);
                        if (DEBUG_MODE) {
                            System.err.println("[MCP-DEBUG] SENDING: " + responseJson);
                        }
                        writer.println(responseJson);
                        writer.flush();
                    } else if (DEBUG_MODE) {
                        System.err.println("[MCP-DEBUG] No response (notification message)");
                    }
                    
                } catch (Exception e) {
                    if (DEBUG_MODE) {
                        System.err.println("[MCP-DEBUG] ERROR processing message: " + line);
                        System.err.println("[MCP-DEBUG] Exception: " + e.getClass().getSimpleName() + ": " + e.getMessage());
                        e.printStackTrace(System.err);
                    } else {
                        System.err.println("Error processing message: " + line + " - " + e.getMessage());
                    }
                    
                    // Send error response
                    Map<String, Object> errorResponse = Map.of(
                        "jsonrpc", "2.0",
                        "error", Map.of(
                            "code", -32700,
                            "message", "Parse error",
                            "data", e.getMessage()
                        ),
                        "id", null
                    );
                    
                    String errorJson = objectMapper.writeValueAsString(errorResponse);
                    if (DEBUG_MODE) {
                        System.err.println("[MCP-DEBUG] SENDING ERROR: " + errorJson);
                    }
                    writer.println(errorJson);
                    writer.flush();
                }
            }
            
            if (DEBUG_MODE) {
                System.err.println("[MCP-DEBUG] MCP stdio server shutting down");
            } else {
                System.err.println("MCP stdio server shutting down");
            }
            
        } catch (Exception e) {
            if (DEBUG_MODE) {
                System.err.println("[MCP-DEBUG] Error in message loop: " + e.getMessage());
                e.printStackTrace(System.err);
            } else {
                System.err.println("Error in message loop: " + e.getMessage());
            }
        }
    }
}