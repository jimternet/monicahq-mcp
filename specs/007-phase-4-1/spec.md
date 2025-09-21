# Specification: Phase 4.1 - Core Missing Entities

## <¯ Objective
Implement the 4 most critical missing Monica API entities to advance from 70% to 85% API coverage, adding 18 new MCP operations that enable essential contact management workflows.

## =Ê Scope Definition

### Current State
- **API Coverage**: 70% (68 operations across 16 entity types)
- **Missing Critical Functionality**: Activity categorization, physical addresses, contact grouping, professional information

### Target State  
- **API Coverage**: 85% (86 operations across 20 entity types)
- **New Operations**: 18 MCP operations across 4 essential entity types
- **Impact**: Enable complete contact lifecycle management workflows

## <× Entity Implementation Plan

### 1. Activity Types (4 operations)
**Business Value**: Proper categorization and organization of contact activities

#### Entity Definition
```typescript
ActivityType {
  id: number
  name: string                    // "Meeting", "Call", "Email", etc.
  category_id?: number           // Link to ActivityTypeCategory
  description?: string           // Detailed description
  icon?: string                  // Visual representation
  created_at: string
  updated_at: string
}
```

#### MCP Operations
- `activity_type_create`: Create new activity type
- `activity_type_get`: Retrieve activity type by ID
- `activity_type_update`: Update existing activity type  
- `activity_type_delete`: Remove activity type
- `activity_type_list`: List all activity types with pagination

#### Integration Points
- **Activity Service**: Enhanced activity creation with proper typing
- **ActivityTypeCategory**: Hierarchical organization support
- **User Workflows**: Activity creation with type selection

### 2. Activity Type Categories (4 operations)  
**Business Value**: Hierarchical organization of activity types

#### Entity Definition
```typescript
ActivityTypeCategory {
  id: number
  name: string                   // "Work", "Personal", "Health", etc.
  parent_id?: number            // Support for nested categories
  description?: string
  sort_order?: number
  created_at: string
  updated_at: string
}
```

#### MCP Operations
- `activity_type_category_create`: Create new category
- `activity_type_category_get`: Retrieve category by ID
- `activity_type_category_update`: Update existing category
- `activity_type_category_delete`: Remove category
- `activity_type_category_list`: List all categories with hierarchy

#### Integration Points
- **ActivityType**: Parent-child relationship management
- **Hierarchical Display**: Tree structure for organization
- **User Experience**: Nested category selection

### 3. Addresses (5 operations)
**Business Value**: Physical location tracking for contacts

#### Entity Definition
```typescript
Address {
  id: number
  contact_id: number            // Required link to contact
  name?: string                 // "Home", "Work", "Billing", etc.
  street?: string              // Street address
  city?: string                // City name
  province?: string            // State/Province
  postal_code?: string         // ZIP/Postal code
  country_id?: number          // Link to Country reference
  latitude?: number            // GPS coordinates
  longitude?: number           // GPS coordinates
  created_at: string
  updated_at: string
}
```

#### MCP Operations
- `address_create`: Add new address to contact
- `address_get`: Retrieve address by ID
- `address_update`: Update existing address
- `address_delete`: Remove address
- `address_list`: List addresses (by contact or all)

#### Integration Points
- **Contact**: One-to-many relationship with contacts
- **Country**: Reference data for international addresses
- **Geographic Services**: Latitude/longitude support
- **User Workflows**: Address management during contact creation/editing

### 4. Groups (5 operations)
**Business Value**: Contact organization and segmentation

#### Entity Definition
```typescript
Group {
  id: number
  name: string                  // "Family", "Colleagues", "Friends", etc.
  description?: string          // Group purpose/description
  contact_count?: number        // Calculated field
  created_at: string
  updated_at: string
}
```

#### MCP Operations
- `group_create`: Create new contact group
- `group_get`: Retrieve group by ID with contact count
- `group_update`: Update group information
- `group_delete`: Remove group (handle contact relationships)
- `group_list`: List all groups with contact counts

#### Integration Points
- **Contact**: Many-to-many relationship via ContactGroup
- **ContactGroup**: Junction table for group membership
- **Bulk Operations**: Mass contact operations by group
- **Filtering**: Contact lists filtered by group membership

### 5. Occupations (4 operations)
**Business Value**: Professional information and career tracking

#### Entity Definition
```typescript
Occupation {
  id: number
  contact_id: number            // Required link to contact
  title: string                 // Job title
  company?: string              // Company name
  description?: string          // Role description
  salary?: number               // Optional salary information
  start_date?: string          // Employment start date
  end_date?: string            // Employment end date (if ended)
  created_at: string
  updated_at: string
}
```

#### MCP Operations
- `occupation_create`: Add occupation to contact
- `occupation_get`: Retrieve occupation by ID
- `occupation_update`: Update occupation information
- `occupation_delete`: Remove occupation
- `occupation_list`: List occupations (by contact or all)

#### Integration Points
- **Contact**: One-to-many relationship (career history)
- **Company**: Future integration with company entities
- **Professional Networks**: Career progression tracking
- **Timeline**: Chronological career history

## >ê Testing Strategy

### Test-Driven Development Approach
Each entity follows the established TDD pattern:

1. **Contract Tests First**: Write failing MCP protocol tests
2. **Implementation**: Build minimal code to pass tests
3. **Integration Tests**: End-to-end workflow validation
4. **Mock Integration**: Update TestMonicaHqClient with entity stubs

### Test Coverage Requirements
- **Contract Tests**: 5 tests per entity (20 total)
- **Integration Tests**: 2 cross-entity workflow tests
- **Performance Tests**: Response time validation
- **Security Tests**: Authentication and authorization validation

### Quality Gates
- **100% Test Coverage**: All new code must have tests
- **Constitutional Compliance**: All 6 principles maintained
- **Performance**: <500ms average response time
- **Security**: Proper authentication for all operations

## <Û Constitutional Compliance

### Principle I: MCP Protocol First
- All operations follow JSON-RPC 2.0 over STDIO
- Proper schema definitions for Claude Desktop integration
- Consistent error handling and response formats

### Principle II: Test-Driven Development  
- Tests written before implementation
- 100% test coverage maintained (173 ’ 195+ tests)
- Contract and integration test validation

### Principle III: Spring Boot Architecture Excellence
- Reactive WebFlux patterns for all external API calls
- Proper service layer organization
- MonicaHqClient integration following established patterns

### Principle IV: Production-Ready Deployment
- Docker deployment configuration updated
- Claude Desktop integration verified
- Performance and reliability standards maintained

### Principle V: Type Safety & Code Generation
- MapStruct-style DTOs with validation annotations
- Lombok for code generation where appropriate
- Type-safe conversions between API and domain formats

### Principle VI: Complete Monica API Data Access
- Raw API data preservation in content responses
- ContentFormatter integration for field visibility
- No data loss during API-to-MCP transformations

## <¯ Success Criteria

### Quantitative Metrics
- **Operations**: 68 ’ 86 (18 new operations)
- **Entity Types**: 16 ’ 20 (4 new types)
- **API Coverage**: 70% ’ 85% 
- **Test Coverage**: 173 ’ 195+ tests (100% pass rate)

### Qualitative Metrics
- **User Experience**: Complete contact management workflows enabled
- **Data Integrity**: No loss of Monica API field information
- **Performance**: No degradation in response times
- **Security**: Proper authentication and authorization
- **Maintainability**: Code follows established patterns and conventions

## =Ë Implementation Priority

### Week 1A: Foundation (Days 1-2)
- Activity Types and Categories (8 operations)
- Hierarchical organization system
- Enhanced activity creation workflows

### Week 1B: Contact Enhancement (Days 3-4)  
- Addresses and Groups (10 operations)
- Physical location and organization features
- Contact lifecycle completion

### Week 1C: Professional Information (Day 5)
- Occupations (4 operations) 
- Career tracking and professional networks
- Integration testing and validation

## =€ Expected Impact

Upon completion of Phase 4.1:
- **Essential Workflows**: Complete contact management capability
- **User Satisfaction**: No critical functionality gaps
- **Foundation**: Solid base for subsequent phases
- **Quality**: Maintained constitutional compliance and test coverage
- **Performance**: Optimized system with 20 entity types

This phase establishes the essential foundation for comprehensive Monica CRM access, enabling users to manage the complete contact relationship lifecycle through Claude Desktop.