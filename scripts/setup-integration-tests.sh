#!/bin/bash

# Setup script for Docker-based integration testing with real Monica instance
# This script sets up a complete isolated testing environment

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_ROOT"

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${BLUE}================================================${NC}"
echo -e "${BLUE}MonicaHQ MCP Server - Integration Test Setup${NC}"
echo -e "${BLUE}================================================${NC}"
echo ""

# Step 1: Build the MCP server JAR
echo -e "${BLUE}[1/6] Building MCP Server JAR...${NC}"
./gradlew build -x test
echo -e "${GREEN}✅ JAR built successfully${NC}"
echo ""

# Step 2: Generate Monica APP_KEY
echo -e "${BLUE}[2/6] Generating Monica APP_KEY...${NC}"
APP_KEY=$(openssl rand -base64 32)
echo -e "${GREEN}✅ APP_KEY generated: base64:$APP_KEY${NC}"
echo ""

# Step 3: Create .env.test file
echo -e "${BLUE}[3/6] Creating .env.test configuration...${NC}"
cat > .env.test <<EOF
# Monica Configuration
APP_KEY=base64:$APP_KEY
ADMIN_EMAIL=test@example.com
ADMIN_PASSWORD=test_password_123

# Monica API Token (will be generated after Monica starts)
# You'll need to generate this manually via Monica UI or API
MONICA_TEST_API_TOKEN=

# Database
DB_HOST=monica-db
DB_DATABASE=monica
DB_USERNAME=monica_user
DB_PASSWORD=monica_pass
EOF
echo -e "${GREEN}✅ Configuration file created: .env.test${NC}"
echo ""

# Step 4: Start Monica and database
echo -e "${BLUE}[4/6] Starting Monica instance and database...${NC}"
echo -e "${YELLOW}This may take 1-2 minutes for initial setup...${NC}"
docker-compose -f docker-compose.test.yml up -d monica-db monica
echo -e "${GREEN}✅ Monica is starting...${NC}"
echo ""

# Step 5: Wait for Monica to be healthy
echo -e "${BLUE}[5/6] Waiting for Monica to be ready...${NC}"
MAX_WAIT=120
ELAPSED=0
while [ $ELAPSED -lt $MAX_WAIT ]; do
    if docker-compose -f docker-compose.test.yml ps | grep -q "monica.*healthy"; then
        echo -e "${GREEN}✅ Monica is healthy and ready!${NC}"
        break
    fi
    echo -n "."
    sleep 5
    ELAPSED=$((ELAPSED + 5))
done

if [ $ELAPSED -ge $MAX_WAIT ]; then
    echo -e "${RED}❌ Monica did not become healthy within $MAX_WAIT seconds${NC}"
    echo -e "${YELLOW}Check logs: docker-compose -f docker-compose.test.yml logs monica${NC}"
    exit 1
fi
echo ""

# Step 6: Instructions for generating API token
echo -e "${BLUE}[6/6] Next Steps: Generate Monica API Token${NC}"
echo ""
echo -e "${YELLOW}Monica is now running at: http://localhost:8081${NC}"
echo ""
echo "To complete setup and run integration tests:"
echo "1. Visit http://localhost:8081"
echo "2. Login with: test@example.com / test_password_123"
echo "3. Navigate to Settings → API → Personal Access Tokens"
echo "4. Create a new token with full permissions"
echo "5. Copy the token and add it to .env.test:"
echo "   MONICA_TEST_API_TOKEN=your-token-here"
echo ""
echo "Then run integration tests with:"
echo -e "${GREEN}  docker-compose -f docker-compose.test.yml --profile test up integration-tests${NC}"
echo ""
echo "Or run validation scripts manually:"
echo -e "${GREEN}  export MONICA_API_URL=http://localhost:8081/api${NC}"
echo -e "${GREEN}  export MONICA_API_TOKEN=your-token-here${NC}"
echo -e "${GREEN}  ./validation/crud/validate-contact-crud.sh${NC}"
echo ""
echo -e "${BLUE}================================================${NC}"
echo -e "${BLUE}Setup Complete!${NC}"
echo -e "${BLUE}================================================${NC}"
