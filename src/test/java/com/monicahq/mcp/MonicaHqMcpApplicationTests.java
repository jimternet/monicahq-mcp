package com.monicahq.mcp;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

@SpringBootTest
@ActiveProfiles("test")
class MonicaHqMcpApplicationTests {

    @Test
    void contextLoads() {
        // This test just verifies that the Spring context loads successfully
        // with all our configurations and dependencies
    }
}