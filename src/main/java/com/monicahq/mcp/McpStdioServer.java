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

    public static void main(String[] args) {
        runStdioServer();
    }
    
    private static void runStdioServer() {
        // Use STDIO-specific logging configuration that sends ALL output to stderr
        System.setProperty("logging.config", "classpath:logback-stdio.xml");
        
        // Start Spring Boot context without web server
        System.setProperty("spring.main.web-application-type", "none");
        System.setProperty("server.port", "-1"); // Disable web server
        System.setProperty("spring.main.banner-mode", "off"); // Disable Spring Boot banner
        System.setProperty("logging.level.root", "WARN"); // Minimal logging
        System.setProperty("logging.level.com.monicahq.mcp", "ERROR"); // Only errors
        
        // Print initial message to stderr only
        System.err.println("Starting MCP Server in stdio mode for Claude Desktop");
        
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
            
            // Server is ready - no logging during normal operation
            
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) {
                    continue;
                }
                
                try {
                    // Parse JSON message
                    JsonNode message = objectMapper.readTree(line);
                    
                    // Process message through handler
                    Map<String, Object> response = messageHandler.handleMessage(message, null);
                    
                    // Send response - ONLY JSON to stdout (skip null responses for notifications)
                    if (response != null) {
                        String responseJson = objectMapper.writeValueAsString(response);
                        writer.println(responseJson);
                        writer.flush();
                    }
                    
                } catch (Exception e) {
                    System.err.println("Error processing message: " + line + " - " + e.getMessage());
                    
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
                    writer.println(errorJson);
                    writer.flush();
                }
            }
            
            System.err.println("MCP stdio server shutting down");
            
        } catch (Exception e) {
            System.err.println("Error in message loop: " + e.getMessage());
        }
    }
}