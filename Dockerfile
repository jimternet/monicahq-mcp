# Multi-stage Dockerfile for MonicaHQ MCP Server
# Uses OpenJDK 21 Alpine for optimal size and security

# Build stage - use JDK image and Gradle wrapper for version consistency
FROM eclipse-temurin:21-jdk-alpine AS build
WORKDIR /app

# Copy Gradle wrapper and build files for dependency caching
COPY gradlew ./
COPY gradle/ ./gradle/
COPY build.gradle settings.gradle ./
RUN chmod +x ./gradlew

# Download dependencies (cached layer if no dependency changes)
RUN ./gradlew dependencies --no-daemon

# Copy source code
COPY src/ ./src/

# Build the application
RUN ./gradlew clean bootJar --no-daemon

# Runtime stage
FROM eclipse-temurin:21-jre-alpine
WORKDIR /app

# Create non-root user for security (Alpine Linux)
RUN addgroup -S mcp && adduser -S mcp -G mcp

# Install curl for health checks
RUN apk add --no-cache curl

# Copy the JAR file from build stage
COPY --from=build /app/build/libs/*.jar app.jar

# Change ownership to non-root user
RUN chown -R mcp:mcp /app
USER mcp

# Expose the application port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8080/actuator/health || exit 1

# Environment variables with defaults
ENV SPRING_PROFILES_ACTIVE=docker
ENV JAVA_OPTS="-Xmx512m -Xms256m -XX:+UseG1GC -XX:MaxGCPauseMillis=100"

# Run the application
# Default to stdio mode for Claude Desktop, or web server mode with --web argument
ENTRYPOINT ["sh", "-c", "if [ \"$1\" = \"--web\" ]; then java $JAVA_OPTS -jar app.jar; else MCP_STDIO_MODE=true java $JAVA_OPTS -jar app.jar --stdio; fi", "--"]