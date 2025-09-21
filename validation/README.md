# 🧪 MonicaHQ MCP Server - Validation Suite

This directory contains the comprehensive validation framework for the MonicaHQ MCP Server, organized for clarity and maintainability.

## 📁 Directory Structure

```
validation/
├── README.md                    # This documentation
├── crud/                        # CRUD validation scripts (8 entity types)
├── constitutional/              # Constitutional governance validation
├── integration/                 # MCP protocol integration tests
├── setup/                       # Environment setup and utility scripts
└── docs/                        # Validation reports and certificates
```

## 🎯 Quick Start

### Prerequisites
1. Ensure Monica instance is running on localhost:8081
2. Set up environment variables:
   ```bash
   cp ../.env.template ../.env
   # Edit .env with your Monica API credentials
   ```

### Run Complete Validation
```bash
# From project root directory
cd validation

# Run all CRUD validations
./run-all-crud-tests.sh

# Run constitutional compliance
./constitutional/validate-constitution.sh

# Run integration tests
./integration/test-mcp-complete.sh
```

## 📋 Validation Categories

### 🔧 CRUD Validation (`crud/`)
Complete lifecycle testing for all entity types:

| Script | Entity Type | Operations Tested | Status |
|--------|-------------|------------------|---------|
| `validate-contact-crud.sh` | Contact | Create, Read, Update, Delete, List | ✅ 10/10 |
| `validate-contact-field-crud.sh` | Contact Field | Create (via inline), Read, Update, Delete, List | ✅ 9/9 |
| `validate-note-crud.sh` | Note | Create, Read, Update, Delete, List | ✅ 10/10 |
| `validate-task-crud.sh` | Task | Create, Read, Update, Delete, List | ✅ 10/10 |
| `validate-tag-crud.sh` | Tag | Create, Read, Update, Delete, List | ✅ 9/9 |
| `validate-reminder-crud.sh` | Reminder | Create, Read, Update, Delete, List | ✅ 10/10 |
| `validate-activity-crud.sh` | Activity | Create, Read, Update, Delete, List | ✅ 10/10 |
| `validate-call-crud.sh` | Call | Create, Read, Update (alt), Delete, List | ✅ 10/10 |

**Additional CRUD Scripts:**
- `validate-contact-crud-simple.sh` - Simplified contact validation
- `validate-contact-crud-stdio.sh` - STDIO protocol validation
- `validate-contact-crud-mcp-inspector.sh` - MCP inspector validation
- `validate-contact-tag-crud.sh` - Contact-tag relationship operations
- `validate-conversation-crud.sh` - Conversation entity (future implementation)

### 🏛️ Constitutional Validation (`constitutional/`)
Governance framework compliance testing:

| Script | Purpose | Validates |
|--------|---------|-----------|
| `validate-constitution.sh` | Constitutional compliance | All 5 core principles |
| `validate-complete-integration.sh` | End-to-end validation | Discovery tools + API integration |

### 🔗 Integration Testing (`integration/`)
MCP protocol and system integration:

| Script | Purpose | Protocol |
|--------|---------|----------|
| `test-mcp-complete.sh` | Comprehensive MCP testing | STDIO + JSON-RPC 2.0 |
| `test-mcp-stdio.sh` | STDIO communication | Direct pipe communication |
| `test-mcp-stdio-simple.sh` | Simple STDIO test | Basic request/response |
| `test-claude-desktop.sh` | Claude Desktop integration | Desktop compatibility |
| `test-mcp-operations.sh` | MCP operations testing | Tool invocation |
| `validate-dev-setup.sh` | Development environment | Docker + Monica setup |

### ⚙️ Setup & Utilities (`setup/`)
Environment configuration and helper scripts:

| Script | Purpose | Usage |
|--------|---------|-------|
| `load-env.sh` | Environment variable loading | `source ../setup/load-env.sh` |
| `setup-mcp-tools.sh` | MCP tools configuration | Initial setup |
| `update-scripts-for-env.sh` | Mass script updates | Security improvements |

### 📊 Documentation (`docs/`)
Validation reports and compliance certificates:

- `COMPREHENSIVE-CRUD-VALIDATION-SUMMARY.md` - Complete validation overview
- `CONSTITUTIONAL-PRINCIPLE-VII-CERTIFICATE.md` - Compliance certificate
- Additional validation reports

## 🚀 Usage Examples

### Run Single Entity Validation
```bash
cd validation/crud
./validate-contact-crud.sh
```

### Run Constitutional Compliance Check
```bash
cd validation/constitutional
./validate-constitution.sh
```

### Run Integration Tests
```bash
cd validation/integration
./test-mcp-complete.sh
```

### Environment Setup
```bash
cd validation/setup
./setup-mcp-tools.sh
```

## 🔒 Security Features

### Environment Variable Management
All scripts use secure environment variable loading:
- No hardcoded JWT tokens in any script
- `.env` file for sensitive configuration
- Template provided for easy setup

### Validation Security
- All validation scripts include cleanup procedures
- Test data is automatically removed after validation
- No production data modification

## 📈 Validation Coverage

### Success Metrics
- **Entity Coverage**: 8/8 major entity types (100%)
- **Test Success Rate**: 78/78 tests passed (100%)
- **Constitutional Compliance**: 5/5 principles verified
- **Security Audit**: 0 hardcoded credentials found

### Constitutional Principle VII Compliance
✅ **API Discovery Capabilities**: Dynamic ID resolution  
✅ **Complete Monica API Coverage**: 54 MCP tools  
✅ **Dynamic Functionality**: No hardcoded limitations  
✅ **User Experience Excellence**: Discovery-driven workflows  

## 🛠️ Troubleshooting

### Common Issues
1. **Monica API Connection**: Ensure Monica is running on localhost:8081
2. **Environment Variables**: Check `.env` file configuration
3. **Permission Issues**: Ensure scripts have execute permissions
4. **Docker Setup**: Verify Monica container is healthy

### Debug Mode
Add `set -x` to any script for detailed execution logging:
```bash
set -x  # Add to script for debug output
```

## 📝 Adding New Validations

### For New Entity Types
1. Create new script in `crud/` following naming pattern: `validate-{entity}-crud.sh`
2. Use existing scripts as templates
3. Include environment loading: `source ../setup/load-env.sh`
4. Follow standard CRUD pattern: Create → Read → Update → Delete → List
5. Update this README with new entity information

### For New Test Types
1. Add to appropriate category folder
2. Update documentation
3. Ensure security compliance (no hardcoded credentials)

## 🎊 Production Readiness

The validation suite confirms:
- ✅ **100% CRUD Coverage**: All major entities validated
- ✅ **Security Compliance**: No credential exposure
- ✅ **Constitutional Compliance**: All governance principles verified
- ✅ **Claude Desktop Ready**: STDIO protocol integration complete

---

*This validation framework ensures the MonicaHQ MCP Server meets all quality, security, and functionality requirements for production deployment.*