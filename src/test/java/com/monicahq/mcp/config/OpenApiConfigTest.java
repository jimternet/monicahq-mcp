package com.monicahq.mcp.config;

import org.junit.jupiter.api.Test;
import org.springdoc.core.properties.SpringDocConfigProperties;
import org.springdoc.core.properties.SwaggerUiConfigProperties;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests to verify that springdoc-openapi configuration loads correctly.
 * Validates API documentation settings from application.yml.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@ActiveProfiles("test")
@TestPropertySource(properties = {
    "spring.main.web-application-type=reactive"
})
public class OpenApiConfigTest {

    @Autowired
    private SpringDocConfigProperties springDocConfigProperties;

    @Autowired(required = false)
    private SwaggerUiConfigProperties swaggerUiConfigProperties;

    @Test
    void contextLoads() {
        // Verify springdoc beans are loaded
        assertNotNull(springDocConfigProperties, "SpringDocConfigProperties should be loaded");
        assertNotNull(swaggerUiConfigProperties, "SwaggerUiConfigProperties should be loaded");
    }

    @Test
    void apiDocsConfigurationIsCorrect() {
        // Verify API docs path is configured
        SpringDocConfigProperties.ApiDocs apiDocs = springDocConfigProperties.getApiDocs();
        assertNotNull(apiDocs, "API docs configuration should be present");
        assertEquals("/v3/api-docs", apiDocs.getPath(), "API docs path should be /v3/api-docs");
        assertTrue(apiDocs.isEnabled(), "API docs should be enabled");
    }

    @Test
    void swaggerUiConfigurationIsCorrect() {
        // Verify Swagger UI path is configured
        String swaggerUiPath = swaggerUiConfigProperties.getPath();
        assertNotNull(swaggerUiPath, "Swagger UI path should be configured");
        assertEquals("/swagger-ui.html", swaggerUiPath, "Swagger UI path should be /swagger-ui.html");
        assertTrue(swaggerUiConfigProperties.isEnabled(), "Swagger UI should be enabled");
    }

    @Test
    void swaggerUiSortingConfigurationIsCorrect() {
        // Verify sorting options are configured per application.yml
        String operationsSorter = swaggerUiConfigProperties.getOperationsSorter();
        String tagsSorter = swaggerUiConfigProperties.getTagsSorter();

        assertEquals("alpha", operationsSorter, "Operations should be sorted alphabetically");
        assertEquals("alpha", tagsSorter, "Tags should be sorted alphabetically");
    }
}
