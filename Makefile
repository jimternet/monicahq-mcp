# MonicaHQ MCP Server Development Makefile
.PHONY: help dev-up dev-down dev-validate dev-test dev-logs dev-shell build test clean

help: ## Show this help message
	@echo 'Usage: make [target]'
	@echo ''
	@echo 'Targets:'
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  %-15s %s\n", $$1, $$2}' $(MAKEFILE_LIST)

# Development Environment
dev-up: ## Start complete development stack (Monica + MCP Server + Database)
	@echo "🚀 Starting MonicaHQ MCP Development Stack..."
	@if [ -z "$$MONICA_API_TOKEN" ]; then \
		echo "⚠️  Warning: MONICA_API_TOKEN not set. Use: export MONICA_API_TOKEN=your-token"; \
	fi
	docker-compose -f docker-compose.dev.yml up -d
	@echo "✅ Development stack started!"
	@echo "📱 Monica CRM: http://localhost:8081"
	@echo "🔌 MCP Server: http://localhost:8080"
	@echo "🏥 Health Check: http://localhost:8080/actuator/health"

dev-down: ## Stop and cleanup development stack
	@echo "🛑 Stopping development stack..."
	docker-compose -f docker-compose.dev.yml down -v
	@echo "✅ Development stack stopped and cleaned up!"

dev-validate: ## Run validation suite in development environment
	@echo "🧪 Running constitutional compliance validation..."
	docker-compose -f docker-compose.dev.yml --profile validation up -d validation-service
	docker-compose -f docker-compose.dev.yml exec validation-service ./validation/constitutional/validate-constitution.sh
	@echo "✅ Validation complete!"

dev-test: ## Run comprehensive test suite in development environment
	@echo "🧪 Running comprehensive 7-phase testing suite..."
	docker-compose -f docker-compose.dev.yml --profile validation up -d validation-service
	docker-compose -f docker-compose.dev.yml exec validation-service ./validation/integration/test-mcp-complete.sh
	@echo "✅ Testing complete!"

dev-logs: ## Show logs from development stack
	docker-compose -f docker-compose.dev.yml logs -f

dev-shell: ## Get shell access to MCP server container
	docker-compose -f docker-compose.dev.yml exec monicahq-mcp /bin/bash

dev-monica-shell: ## Get shell access to Monica container
	docker-compose -f docker-compose.dev.yml exec monica /bin/bash

# Build and Test
build: ## Build the application
	@echo "🔨 Building MonicaHQ MCP Server..."
	./gradlew build
	@echo "✅ Build complete!"

test: ## Run all tests locally
	@echo "🧪 Running all tests..."
	./gradlew test
	@echo "✅ All tests passed!"

test-constitutional: ## Run constitutional compliance validation locally
	@echo "🏛️ Running constitutional compliance validation..."
	./validation/constitutional/validate-constitution.sh
	@echo "✅ Constitutional compliance verified!"

# Docker Operations
docker-build: ## Build Docker image
	@echo "🐳 Building Docker image..."
	./gradlew build
	docker build -t monicahq-mcp .
	@echo "✅ Docker image built!"

docker-run-stdio: ## Run Docker container in STDIO mode
	@if [ -z "$$MONICA_API_URL" ] || [ -z "$$MONICA_API_TOKEN" ]; then \
		echo "❌ Error: Set MONICA_API_URL and MONICA_API_TOKEN environment variables"; \
		exit 1; \
	fi
	docker run --rm -i -e MONICA_API_URL -e MONICA_API_TOKEN monicahq-mcp

docker-run-web: ## Run Docker container in Web Server mode
	@if [ -z "$$MONICA_API_URL" ] || [ -z "$$MONICA_API_TOKEN" ]; then \
		echo "❌ Error: Set MONICA_API_URL and MONICA_API_TOKEN environment variables"; \
		exit 1; \
	fi
	docker run --rm -p 8080:8080 -e MONICA_API_URL -e MONICA_API_TOKEN monicahq-mcp --web

# Development Validation
validate-all: ## Run all validation checks (constitutional + tests + Claude Desktop)
	@echo "🧪 Running complete validation suite..."
	@echo "Phase 1: Constitutional compliance..."
	./validation/constitutional/validate-constitution.sh
	@echo "Phase 2: Test suite..."
	./gradlew test
	@echo "Phase 3: Claude Desktop integration..."
	./validation/integration/test-claude-desktop.sh
	@echo "✅ All validation checks passed!"

validate-stdout: ## Check STDOUT cleanliness for MCP protocol
	@echo "🔍 Validating STDOUT cleanliness..."
	./tests/integration/test_stdout_cleanliness.sh
	@echo "✅ STDOUT validation passed!"

# Cleanup
clean: ## Clean build artifacts and containers
	@echo "🧹 Cleaning up..."
	./gradlew clean
	docker system prune -f
	@echo "✅ Cleanup complete!"

clean-all: ## Complete cleanup including volumes
	@echo "🧹 Complete cleanup (including data volumes)..."
	make dev-down
	./gradlew clean
	docker system prune -af --volumes
	@echo "✅ Complete cleanup finished!"

# Claude Desktop Integration
claude-config-jar: ## Generate Claude Desktop config for JAR mode
	@echo "📝 Generating Claude Desktop configuration for JAR mode..."
	@echo "Add this to your Claude Desktop config:"
	@echo ""
	@cat claude_desktop_config.json
	@echo ""
	@echo "Config file location:"
	@echo "  macOS: ~/Library/Application\\ Support/Claude/claude_desktop_config.json"
	@echo "  Linux: ~/.config/claude-desktop/claude_desktop_config.json"

claude-config-docker: ## Generate Claude Desktop config for Docker mode
	@echo "📝 Generating Claude Desktop configuration for Docker mode..."
	@echo "Add this to your Claude Desktop config:"
	@echo ""
	@cat claude_desktop_config_docker.json
	@echo ""
	@echo "Config file location:"
	@echo "  macOS: ~/Library/Application\\ Support/Claude/claude_desktop_config.json"
	@echo "  Linux: ~/.config/claude-desktop/claude_desktop_config.json"

# Development Status
status: ## Show development environment status
	@echo "📊 MonicaHQ MCP Server Development Status"
	@echo "========================================"
	@echo ""
	@echo "🔧 Environment Variables:"
	@echo "  MONICA_API_URL: $${MONICA_API_URL:-❌ Not set}"
	@echo "  MONICA_API_TOKEN: $${MONICA_API_TOKEN:+✅ Set}$${MONICA_API_TOKEN:-❌ Not set}"
	@echo ""
	@echo "🐳 Docker Services:"
	@docker-compose -f docker-compose.dev.yml ps 2>/dev/null || echo "  ❌ Development stack not running (use 'make dev-up')"
	@echo ""
	@echo "🏗️ Build Status:"
	@if [ -f "build/libs/monicahqmcp-0.1.0.jar" ]; then \
		echo "  ✅ JAR file exists: build/libs/monicahqmcp-0.1.0.jar"; \
	else \
		echo "  ❌ JAR file missing (use 'make build')"; \
	fi
	@echo ""
	@echo "📚 Quick Commands:"
	@echo "  make dev-up       - Start development environment"
	@echo "  make dev-validate - Run validation suite"
	@echo "  make test         - Run all tests"
	@echo "  make help         - Show all available commands"