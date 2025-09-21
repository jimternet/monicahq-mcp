#!/bin/bash

# Load environment variables from .env file
# This script should be sourced by other scripts: source ./load-env.sh

if [ -f .env ]; then
    # Load .env variables
    export $(grep -v '^#' .env | grep -v '^$' | xargs)
    echo "✅ Environment variables loaded from .env"
else
    echo "⚠️  Warning: .env file not found. Please copy .env.template to .env and configure your settings."
    echo "   Monica API token is required for validation scripts to work."
    exit 1
fi

# Validate required variables
if [ -z "$MONICA_API_TOKEN" ] || [ "$MONICA_API_TOKEN" = "your_monica_jwt_token_here" ]; then
    echo "❌ Error: MONICA_API_TOKEN not set in .env file"
    echo "   Please set your Monica API token in .env file"
    exit 1
fi

if [ -z "$MONICA_API_URL" ]; then
    echo "❌ Error: MONICA_API_URL not set in .env file"
    exit 1
fi

echo "🔑 Using Monica API: $MONICA_API_URL"
echo "🎫 Token configured: ${MONICA_API_TOKEN:0:20}..."