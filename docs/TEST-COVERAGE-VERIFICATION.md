# Test Coverage Verification Procedure

**Version**: 1.0
**Date**: 2026-02-06
**Purpose**: Standardize test execution and coverage reporting for the MonicaHQ MCP Server

---

## Executive Summary

**Current Test Status**:
- ✅ **1,780 test cases** across 105 test classes
- ✅ **0 failures** (100% pass rate)
- ✅ **Comprehensive coverage** with parametrized and nested tests

---

## Test Execution Procedures

### 1. Clean Test Execution

Always run tests from a clean state to ensure accurate results:

```bash
# Clean build artifacts and run all tests
./gradlew clean test

# Expected output:
# BUILD SUCCESSFUL in 14s
# 6 actionable tasks: 6 executed
```

**DO NOT** rely on cached test results. Gradle's `UP-TO-DATE` indicator means tests were skipped.

### 2. Test Result Verification

After test execution, verify results:

```bash
# Count total test cases executed
find build/test-results/test -name "*.xml" -exec grep -o '<testcase' {} \; | wc -l

# Count test suites (test classes)
find build/test-results/test -name "*.xml" | wc -l

# Check for failures
find build/test-results/test -name "*.xml" -exec grep -h '<testsuite' {} \; | \
  grep -o 'failures="[0-9]*"' | cut -d'"' -f2 | awk '{sum += $1} END {print sum}'
```

**Expected Results** (as of 2026-02-06):
- Test cases: **1,780**
- Test suites: **115**
- Failures: **0**

### 3. Generate HTML Test Report

Gradle automatically generates an HTML test report:

```bash
# After running tests, open the report
open build/reports/tests/test/index.html

# Or on Linux:
xdg-open build/reports/tests/test/index.html
```

**Report Location**: `build/reports/tests/test/index.html`

---

## Test Coverage Analysis

### JaCoCo Code Coverage Setup

Add JaCoCo plugin to `build.gradle`:

```gradle
plugins {
    id 'jacoco'
}

jacoco {
    toolVersion = "0.8.11"
}

test {
    finalizedBy jacocoTestReport
}

jacocoTestReport {
    dependsOn test
    reports {
        xml.required = true
        html.required = true
        csv.required = false
    }
}

jacocoTestCoverageVerification {
    violationRules {
        rule {
            limit {
                minimum = 0.80  // 80% coverage required
            }
        }
    }
}
```

### Generate Coverage Report

```bash
# Run tests with coverage
./gradlew clean test jacocoTestReport

# View HTML report
open build/reports/jacoco/test/html/index.html

# Verify coverage meets minimum threshold
./gradlew jacocoTestCoverageVerification
```

**Coverage Metrics to Track**:
- **Line Coverage**: % of code lines executed by tests
- **Branch Coverage**: % of conditional branches tested
- **Class Coverage**: % of classes with at least one test
- **Method Coverage**: % of methods tested

**Target Coverage** (recommended):
- Line Coverage: ≥ 80%
- Branch Coverage: ≥ 70%
- Class Coverage: ≥ 90%
- Method Coverage: ≥ 75%

---

## Test Organization

### Test Structure Overview

```
src/test/java/
├── com/monicahq/mcp/
│   ├── service/
│   │   ├── ContactServiceTest.java
│   │   ├── NoteServiceTest.java
│   │   ├── TaskServiceTest.java
│   │   └── base/
│   │       ├── AbstractCrudServiceTest.java
│   │       └── ValidationUtilsTest.java
│   ├── client/
│   │   ├── MonicaHqClientTest.java
│   │   └── AuthInterceptorTest.java
│   ├── controller/
│   │   └── McpMessageHandlerTest.java
│   └── integration/
│       └── FullWorkflowTest.java
```

### Test Types

1. **Unit Tests** (majority of tests):
   - Test individual classes in isolation
   - Use mocks for dependencies
   - Fast execution (<1s per test)

2. **Integration Tests**:
   - Test multiple components together
   - May use test containers for external dependencies
   - Slower execution (1-5s per test)

3. **Contract Tests**:
   - Verify API contracts with Monica API
   - Located in `src/test/java/.../contract/`

### JUnit 5 Features in Use

**Parametrized Tests**:
```java
@ParameterizedTest
@ValueSource(strings = {"John", "Jane", "Bob"})
void testCreateContact(String firstName) {
    // One test method creates 3 test cases
}
```

**Nested Tests**:
```java
@Nested
class CreateContactTests {
    @Test void shouldCreateWithValidData() {}
    @Test void shouldRejectInvalidData() {}
}

@Nested
class UpdateContactTests {
    @Test void shouldUpdateExisting() {}
}
```

This is why we have **1,780 test cases from 105 test classes** - excellent use of JUnit 5 features!

---

## Continuous Integration

### GitHub Actions Test Workflow

Current workflow (`.github/workflows/gradle.yml`):

```yaml
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Set up JDK 21
        uses: actions/setup-java@v3
        with:
          java-version: '21'
      - name: Run tests
        run: ./gradlew clean test
      - name: Upload test results
        uses: actions/upload-artifact@v3
        with:
          name: test-results
          path: build/test-results/test/
```

### Test Failure Protocol

If tests fail in CI:

1. **Identify failing tests**:
   ```bash
   find build/test-results/test -name "*.xml" -exec grep -l "error\|failure" {} \;
   ```

2. **Review failure details**:
   - Check HTML report: `build/reports/tests/test/index.html`
   - Examine stack traces in XML files

3. **Reproduce locally**:
   ```bash
   ./gradlew clean test --tests "com.monicahq.mcp.service.ContactServiceTest"
   ```

4. **Fix and verify**:
   ```bash
   # Fix the issue, then run full test suite
   ./gradlew clean test
   ```

**DO NOT** merge pull requests with failing tests.

---

## Test Documentation Standards

### Test Method Naming

Use descriptive names that explain the test scenario:

```java
// ✅ GOOD - Clear intent
@Test
void shouldCreateContactWhenValidDataProvided() {}

@Test
void shouldThrowExceptionWhenFirstNameIsMissing() {}

// ❌ BAD - Unclear intent
@Test
void testContact() {}

@Test
void test1() {}
```

### Test Documentation

Complex tests should include JavaDoc comments:

```java
/**
 * Verifies that the update operation preserves required fields from the existing
 * contact when not explicitly provided in the update request (fetch-before-update pattern).
 */
@Test
void shouldPreserveRequiredFieldsWhenUpdatingContact() {
    // Test implementation
}
```

---

## Performance Testing

### Test Execution Time Tracking

Monitor test execution time to prevent slow tests:

```bash
# Run tests with timing information
./gradlew clean test --info | grep "completed in"

# Or use Gradle build scan
./gradlew clean test --scan
```

**Performance Targets**:
- Unit tests: < 100ms each
- Integration tests: < 5s each
- Full test suite: < 30s

**Slow Test Warning**: If individual tests take >5s, consider:
1. Optimizing test setup
2. Using mocks instead of real dependencies
3. Moving to separate integration test suite

---

## Test Data Management

### Test Data Strategy

1. **Use in-memory test data**:
   - No external database required for most tests
   - Fast test execution
   - Isolated test runs

2. **Mock external API calls**:
   ```java
   @MockBean
   private MonicaHqClient monicaClient;

   @BeforeEach
   void setup() {
       when(monicaClient.get(anyString(), any()))
           .thenReturn(Mono.just(Map.of("id", 123, "first_name", "John")));
   }
   ```

3. **Test data builders**:
   ```java
   public class ContactTestDataBuilder {
       public static Map<String, Object> buildValidContact() {
           return Map.of(
               "firstName", "John",
               "lastName", "Doe",
               "genderId", 1
           );
       }
   }
   ```

---

## Verification Checklist

Before each release, verify:

### Test Execution
- [ ] Run `./gradlew clean test` - all tests pass
- [ ] Test count is accurate (currently 1,780 test cases)
- [ ] No flaky tests (tests that pass/fail randomly)
- [ ] Test execution time < 30s

### Coverage
- [ ] Generate JaCoCo report: `./gradlew jacocoTestReport`
- [ ] Line coverage ≥ 80%
- [ ] Branch coverage ≥ 70%
- [ ] No untested critical paths

### Documentation
- [ ] Test names are descriptive
- [ ] Complex tests have comments
- [ ] Test data is self-contained
- [ ] No hardcoded API tokens in tests

### CI/CD
- [ ] GitHub Actions tests pass
- [ ] Test results uploaded to artifacts
- [ ] Coverage report generated in CI

---

## Troubleshooting

### Common Issues

**Issue**: Tests show "UP-TO-DATE" but don't actually run
```bash
# Solution: Force re-execution
./gradlew clean test --rerun-tasks
```

**Issue**: Different test count between local and CI
```bash
# Verify test discovery
./gradlew test --info | grep "Executing test"

# Check for profile-specific tests
./gradlew test -Dspring.profiles.active=test
```

**Issue**: Out of memory during test execution
```bash
# Increase test heap size
./gradlew test -Dorg.gradle.jvmargs="-Xmx2g"
```

---

## Metrics Dashboard

### Test Health Metrics (Recommended)

Track these metrics over time:

| Metric | Current | Target | Trend |
|--------|---------|--------|-------|
| Total test cases | 1,780 | - | ⬆️ Growing |
| Test classes | 105 | - | ⬆️ Growing |
| Failure rate | 0% | < 1% | ✅ Stable |
| Avg test time | <20ms | <100ms | ✅ Fast |
| Line coverage | TBD | ≥80% | - |
| Branch coverage | TBD | ≥70% | - |

---

## Appendix

### Test Result File Formats

**JUnit XML Format** (`TEST-*.xml`):
```xml
<testsuite name="ContactServiceTest" tests="17" failures="0" errors="0" time="0.234">
    <testcase name="shouldCreateContactWhenValidDataProvided" classname="com.monicahq.mcp.service.ContactServiceTest" time="0.012"/>
    <!-- More test cases -->
</testsuite>
```

**Gradle HTML Report**: Interactive HTML with:
- Test hierarchy by package/class
- Pass/fail status with stack traces
- Execution time breakdown
- Standard output/error logs

---

**Document Owner**: QA Team / Devil's Advocate
**Last Updated**: 2026-02-06
**Next Review**: Monthly or before major releases
