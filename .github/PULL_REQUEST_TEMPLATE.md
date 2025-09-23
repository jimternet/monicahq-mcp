## Summary
<!-- Provide a brief description of the changes in this PR -->

## Type of Change
<!-- Check all that apply -->
- [ ] 🐛 Bug fix (non-breaking change which fixes an issue)
- [ ] ✨ New feature (non-breaking change which adds functionality)
- [ ] 💥 Breaking change (fix or feature that would cause existing functionality to not work as expected)
- [ ] 📚 Documentation update
- [ ] 🧹 Code refactoring
- [ ] 🧪 Test improvement
- [ ] 🔧 Configuration change

## Related Issues
<!-- Link to related issues using #issue-number -->
Fixes #
Related to #

## Constitutional Compliance Checklist
<!-- All items must be checked for PR approval -->
- [ ] **Principle I: MCP Protocol First** - Changes maintain JSON-RPC 2.0 over STDIO compliance
- [ ] **Principle II: TDD** - Tests written/updated before implementation (100% coverage maintained)
- [ ] **Principle III: Spring Boot Excellence** - WebFlux patterns followed for I/O operations
- [ ] **Principle IV: Production Ready** - Docker and Claude Desktop compatibility verified
- [ ] **Principle V: Type Safety** - MapStruct/Lombok patterns used appropriately
- [ ] **Principle VI: Complete API Access** - All Monica API fields visible in MCP responses

## Testing
<!-- Describe the tests you ran and their results -->
- [ ] All existing tests pass (`./gradlew test` shows 188/188 passing)
- [ ] Constitutional validation passes (`./validation/constitutional/validate-constitution.sh`)
- [ ] STDOUT cleanliness verified (for MCP protocol changes)
- [ ] New tests added for new functionality
- [ ] Manual testing completed with steps documented below

### Manual Testing Steps
<!-- List the steps to manually test your changes -->
1. 
2. 
3. 

## Screenshots/Logs
<!-- If applicable, add screenshots or relevant log outputs -->

## Breaking Changes
<!-- List any breaking changes and migration steps if applicable -->
- [ ] No breaking changes

<!-- If there are breaking changes, describe them here: -->

## Checklist
<!-- Final checklist before submitting -->
- [ ] My code follows the project's code style
- [ ] I have performed a self-review of my own code
- [ ] I have commented my code, particularly in hard-to-understand areas
- [ ] My changes generate no new warnings
- [ ] I have updated the documentation accordingly
- [ ] The commit message follows [Conventional Commits](https://www.conventionalcommits.org/) format

## Additional Notes
<!-- Any additional information that reviewers should know -->