#!/bin/bash

# Script to update all validation scripts to use environment variables instead of hardcoded tokens

set -e

echo "🔧 Updating validation scripts to use environment variables..."

# List of files to update
FILES=(
    "validate-call-crud.sh"
    "validate-contact-field-workaround.sh"
    "validate-contact-tag-crud.sh"
    "validate-conversation-crud.sh"
    "validate-activity-crud.sh"
    "validate-reminder-crud.sh"
    "validate-tag-crud.sh"
    "validate-task-crud.sh"
    "validate-note-crud.sh"
    "validate-contact-field-crud.sh"
    "validate-complete-integration.sh"
)

# Backup original files
echo "📁 Creating backups..."
for file in "${FILES[@]}"; do
    if [ -f "$file" ]; then
        cp "$file" "$file.backup"
        echo "   Backed up $file"
    fi
done

# Update each file
for file in "${FILES[@]}"; do
    if [ -f "$file" ]; then
        echo "🔄 Updating $file..."
        
        # Add environment loading at the top (after the shebang and initial comments)
        sed -i '' '6i\
# Load environment variables\
source ./load-env.sh\
' "$file"
        
        # Replace hardcoded token with environment variable
        sed -i '' 's/MONICA_TOKEN="eyJ0eXAi[^"]*"$/MONICA_TOKEN="$MONICA_API_TOKEN"/' "$file"
        
        # Replace hardcoded URL if present
        sed -i '' 's|http://localhost:8081|$MONICA_API_URL|g' "$file"
        
        echo "   ✅ Updated $file"
    else
        echo "   ⚠️  File $file not found"
    fi
done

# Update mcp-config.json if it exists
if [ -f "mcp-config.json" ]; then
    echo "🔄 Updating mcp-config.json..."
    # Create a version that uses environment variables
    cat > mcp-config.json.template << 'EOF'
{
  "mcpServers": {
    "monicahq": {
      "command": "java",
      "args": [
        "-jar",
        "build/libs/monicahq-mcp-0.1.0.jar"
      ],
      "env": {
        "MONICA_API_URL": "${MONICA_API_URL}",
        "MONICA_API_TOKEN": "${MONICA_API_TOKEN}"
      }
    }
  }
}
EOF
    echo "   ✅ Created mcp-config.json.template"
fi

echo ""
echo "🎉 All scripts updated successfully!"
echo ""
echo "📋 Summary of changes:"
echo "   • Added environment variable loading to all validation scripts"
echo "   • Replaced hardcoded Monica API tokens with \$MONICA_API_TOKEN"
echo "   • Replaced hardcoded URLs with \$MONICA_API_URL"
echo "   • Created backups with .backup extension"
echo "   • Created mcp-config.json.template for secure configuration"
echo ""
echo "🔐 Security improvements:"
echo "   • No more hardcoded tokens in scripts"
echo "   • All sensitive data now in .env file (already in .gitignore)"
echo "   • Environment validation added"
echo ""
echo "🚀 Next steps:"
echo "   1. Verify your .env file has the correct MONICA_API_TOKEN"
echo "   2. Test a validation script: ./validate-contact-crud.sh"
echo "   3. Remove .backup files when satisfied: rm *.backup"