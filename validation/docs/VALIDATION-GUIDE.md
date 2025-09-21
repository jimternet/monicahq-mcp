# MonicaHQ MCP Server - Validation Guide

This guide explains how to validate all MCP tools using a local Monica instance to ensure we don't impact your real Monica data.

## Prerequisites

- Docker and Docker Compose installed
- curl and jq (optional but recommended)
- At least 2GB of available RAM

## Quick Start

### 1. Set up the development environment:

```bash
./validate-dev-setup.sh
```

This will:
- Build the MCP server
- Start a local Monica instance (http://localhost:8081)
- Start the MCP server pointing to the local Monica (http://localhost:8080)
- Start MCP Inspector for testing (http://localhost:3000)

### 2. Create a Monica API token:

1. Open http://localhost:8081 in your browser
2. Create a new account (use any email/password)
3. Go to Settings > API
4. Create a new API token
5. Copy the token

### 3. Validate the tools:

```bash
MONICA_API_TOKEN=your-token-here ./validate-tools.sh
```

This will test:
- Discovery tools (gender_list, contact_field_type_list)
- Contact creation with discovered gender IDs
- Contact field creation with discovered field type IDs

## What Gets Tested

### Discovery Tools (Constitutional Principle VII)

1. **gender_list**: Lists all available genders from Monica
   - Validates response format
   - Ensures escaped JSON content
   - Verifies gender options are returned

2. **contact_field_type_list**: Lists all available contact field types
   - Validates response format
   - Ensures escaped JSON content
   - Verifies field types (email, phone, etc.) are returned

### Integration Testing

1. **Dynamic Gender Usage**: Uses discovered gender IDs instead of hardcoded values
2. **Dynamic Field Type Usage**: Uses discovered field type IDs for contact fields
3. **Full CRUD Cycle**: Create → Read → Update → Delete operations

## Expected Results

✅ **Success Indicators:**
- All discovery tools return valid JSON data
- Contact creation works with discovered gender IDs
- Contact field creation works with discovered field type IDs
- No hardcoded limitations encountered

❌ **Failure Indicators:**
- Discovery tools return empty or invalid data
- Operations fail due to missing IDs
- Hardcoded values are required instead of discovered ones

## Working Practice

This validation setup ensures that:

1. **No Real Data Impact**: All testing happens against a fresh local Monica instance
2. **Constitutional Compliance**: Validates Principle VII (API Discovery and Completeness)
3. **User Experience**: Confirms tools work without guesswork about valid IDs
4. **Regression Testing**: Ensures existing functionality isn't broken

## Cleanup

To stop the development environment:

```bash
docker-compose -f docker-compose.dev.yml down
```

To remove all data and start fresh:

```bash
docker-compose -f docker-compose.dev.yml down -v
```

## Troubleshooting

### Monica takes a long time to start
- First startup can take 5-10 minutes as it sets up the database
- Check logs: `docker-compose -f docker-compose.dev.yml logs monica`

### MCP server can't connect to Monica
- Ensure Monica is fully ready (check http://localhost:8081/api)
- Check MCP server logs: `docker-compose -f docker-compose.dev.yml logs monicahq-mcp`

### API token issues
- Make sure you created the token in the LOCAL Monica instance (localhost:8081)
- Don't use tokens from your real Monica instance

## Directory Structure

```
├── docker-compose.dev.yml          # Development environment setup
├── validate-dev-setup.sh           # Environment setup script
├── validate-tools.sh               # Tool validation script
├── mcp-inspector-dev-config.json   # MCP Inspector configuration
└── VALIDATION-GUIDE.md             # This guide
```

This validation setup provides a safe, repeatable way to test all MCP tools without affecting real Monica data, ensuring Constitutional compliance and user experience quality.