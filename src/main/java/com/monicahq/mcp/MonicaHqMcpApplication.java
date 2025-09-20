package com.monicahq.mcp;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class MonicaHqMcpApplication {

    public static void main(String[] args) {
        // Check if we should run in stdio mode (for Claude Desktop)
        boolean stdioMode = (args.length > 0 && "--stdio".equals(args[0])) || 
                           System.getenv("MCP_STDIO_MODE") != null;
        
        if (stdioMode) {
            McpStdioServer.main(args);
        } else {
            SpringApplication.run(MonicaHqMcpApplication.class, args);
        }
    }
}