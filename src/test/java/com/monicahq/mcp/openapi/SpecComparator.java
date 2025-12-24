package com.monicahq.mcp.openapi;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.OpenAPIV3Parser;
import io.swagger.v3.parser.core.models.ParseOptions;
import io.swagger.v3.parser.core.models.SwaggerParseResult;
import org.openapitools.openapidiff.core.OpenApiCompare;
import org.openapitools.openapidiff.core.model.ChangedOpenApi;
import org.openapitools.openapidiff.core.model.DiffResult;
import org.openapitools.openapidiff.core.output.ConsoleRender;

import java.io.File;
import java.util.List;

/**
 * Utility class for comparing OpenAPI specifications.
 *
 * <p>This class uses OpenAPI Diff to compare a generated OpenAPI specification
 * against a documented specification (source of truth). It detects breaking
 * changes and reports the differences.
 *
 * <p>Usage:
 * <pre>
 * java SpecComparator &lt;documented-spec-path&gt; &lt;generated-spec-path&gt;
 * </pre>
 *
 * <p>Exit codes:
 * <ul>
 *   <li>0 - Specifications are compatible (no breaking changes)</li>
 *   <li>1 - Breaking changes detected</li>
 *   <li>2 - Error parsing specifications</li>
 * </ul>
 *
 * @see <a href="https://github.com/OpenAPITools/openapi-diff">OpenAPI Diff</a>
 */
public class SpecComparator {

    private static final int EXIT_SUCCESS = 0;
    private static final int EXIT_BREAKING_CHANGES = 1;
    private static final int EXIT_PARSE_ERROR = 2;

    /**
     * Main entry point for command-line execution.
     *
     * @param args expects two arguments: documented-spec-path and generated-spec-path
     */
    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Usage: SpecComparator <documented-spec-path> <generated-spec-path>");
            System.err.println("  documented-spec-path: Path to the documented OpenAPI spec (source of truth)");
            System.err.println("  generated-spec-path:  Path to the generated OpenAPI spec (from runtime)");
            System.exit(EXIT_PARSE_ERROR);
            return;
        }

        String documentedSpecPath = args[0];
        String generatedSpecPath = args[1];

        System.out.println("=== OpenAPI Specification Comparison ===");
        System.out.println("Documented spec: " + documentedSpecPath);
        System.out.println("Generated spec:  " + generatedSpecPath);
        System.out.println();

        SpecComparator comparator = new SpecComparator();
        ComparisonResult result = comparator.compare(documentedSpecPath, generatedSpecPath);

        if (result.hasParseErrors()) {
            System.err.println("ERROR: Failed to parse specifications");
            result.getParseErrors().forEach(error -> System.err.println("  - " + error));
            System.exit(EXIT_PARSE_ERROR);
            return;
        }

        // Render the diff output to console
        if (result.getChangedApi() != null) {
            ConsoleRender render = new ConsoleRender();
            String output = render.render(result.getChangedApi());
            System.out.println(output);
        }

        // Report summary
        System.out.println();
        System.out.println("=== Summary ===");

        if (result.isUnchanged()) {
            System.out.println("Status: UNCHANGED");
            System.out.println("The generated specification matches the documented specification exactly.");
            System.exit(EXIT_SUCCESS);
        } else if (result.isCompatible()) {
            System.out.println("Status: COMPATIBLE");
            System.out.println("The generated specification has changes, but they are backward-compatible.");
            System.out.println("Non-breaking changes detected:");
            printChangeSummary(result.getChangedApi());
            System.exit(EXIT_SUCCESS);
        } else {
            System.out.println("Status: BREAKING CHANGES DETECTED");
            System.out.println("The generated specification contains breaking changes that may affect clients.");
            System.out.println();
            System.out.println("Breaking changes detected:");
            printBreakingChanges(result.getChangedApi());
            System.exit(EXIT_BREAKING_CHANGES);
        }
    }

    /**
     * Compares two OpenAPI specifications.
     *
     * @param documentedSpecPath path to the documented specification (source of truth)
     * @param generatedSpecPath path to the generated specification
     * @return comparison result containing diff information and any errors
     */
    public ComparisonResult compare(String documentedSpecPath, String generatedSpecPath) {
        ComparisonResult result = new ComparisonResult();

        // Validate file existence
        File documentedFile = new File(documentedSpecPath);
        File generatedFile = new File(generatedSpecPath);

        if (!documentedFile.exists()) {
            result.addParseError("Documented spec file not found: " + documentedSpecPath);
            return result;
        }

        if (!generatedFile.exists()) {
            result.addParseError("Generated spec file not found: " + generatedSpecPath);
            return result;
        }

        // Parse specifications
        ParseOptions parseOptions = new ParseOptions();
        parseOptions.setResolve(true);
        parseOptions.setResolveFully(true);

        SwaggerParseResult documentedResult = new OpenAPIV3Parser().readLocation(documentedSpecPath, null, parseOptions);
        SwaggerParseResult generatedResult = new OpenAPIV3Parser().readLocation(generatedSpecPath, null, parseOptions);

        // Check for parse errors
        if (documentedResult.getOpenAPI() == null) {
            result.addParseError("Failed to parse documented spec: " + documentedSpecPath);
            if (documentedResult.getMessages() != null) {
                documentedResult.getMessages().forEach(msg -> result.addParseError("  " + msg));
            }
            return result;
        }

        if (generatedResult.getOpenAPI() == null) {
            result.addParseError("Failed to parse generated spec: " + generatedSpecPath);
            if (generatedResult.getMessages() != null) {
                generatedResult.getMessages().forEach(msg -> result.addParseError("  " + msg));
            }
            return result;
        }

        OpenAPI documentedApi = documentedResult.getOpenAPI();
        OpenAPI generatedApi = generatedResult.getOpenAPI();

        // Perform the comparison
        // Note: We compare documented -> generated to detect if generated is backward compatible
        ChangedOpenApi changedApi = OpenApiCompare.fromSpecifications(documentedApi, generatedApi);
        result.setChangedApi(changedApi);

        return result;
    }

    /**
     * Compares two OpenAPI specifications directly from OpenAPI objects.
     *
     * @param documentedApi the documented OpenAPI specification (source of truth)
     * @param generatedApi the generated OpenAPI specification
     * @return comparison result containing diff information
     */
    public ComparisonResult compare(OpenAPI documentedApi, OpenAPI generatedApi) {
        ComparisonResult result = new ComparisonResult();

        if (documentedApi == null) {
            result.addParseError("Documented API is null");
            return result;
        }

        if (generatedApi == null) {
            result.addParseError("Generated API is null");
            return result;
        }

        ChangedOpenApi changedApi = OpenApiCompare.fromSpecifications(documentedApi, generatedApi);
        result.setChangedApi(changedApi);

        return result;
    }

    private static void printChangeSummary(ChangedOpenApi changedApi) {
        if (changedApi == null) {
            return;
        }

        if (changedApi.getNewEndpoints() != null && !changedApi.getNewEndpoints().isEmpty()) {
            System.out.println("  - New endpoints added: " + changedApi.getNewEndpoints().size());
        }

        if (changedApi.getMissingEndpoints() != null && !changedApi.getMissingEndpoints().isEmpty()) {
            System.out.println("  - Endpoints removed: " + changedApi.getMissingEndpoints().size());
        }

        if (changedApi.getDeprecatedEndpoints() != null && !changedApi.getDeprecatedEndpoints().isEmpty()) {
            System.out.println("  - Endpoints deprecated: " + changedApi.getDeprecatedEndpoints().size());
        }

        if (changedApi.getChangedOperations() != null && !changedApi.getChangedOperations().isEmpty()) {
            System.out.println("  - Operations modified: " + changedApi.getChangedOperations().size());
        }

        if (changedApi.getChangedSchemas() != null && !changedApi.getChangedSchemas().isEmpty()) {
            System.out.println("  - Schemas modified: " + changedApi.getChangedSchemas().size());
        }
    }

    private static void printBreakingChanges(ChangedOpenApi changedApi) {
        if (changedApi == null) {
            return;
        }

        // Missing endpoints are breaking changes
        if (changedApi.getMissingEndpoints() != null && !changedApi.getMissingEndpoints().isEmpty()) {
            System.out.println("  Removed endpoints (BREAKING):");
            changedApi.getMissingEndpoints().forEach(endpoint ->
                System.out.println("    - " + endpoint.getMethod() + " " + endpoint.getPathUrl()));
        }

        // Changed operations might contain breaking changes
        if (changedApi.getChangedOperations() != null && !changedApi.getChangedOperations().isEmpty()) {
            changedApi.getChangedOperations().forEach(operation -> {
                if (operation.isDifferent() && !operation.isCompatible()) {
                    System.out.println("  Modified operation (BREAKING): " +
                        operation.getHttpMethod() + " " + operation.getPathUrl());
                }
            });
        }
    }

    /**
     * Result of an OpenAPI specification comparison.
     */
    public static class ComparisonResult {
        private ChangedOpenApi changedApi;
        private List<String> parseErrors = new java.util.ArrayList<>();

        public ChangedOpenApi getChangedApi() {
            return changedApi;
        }

        public void setChangedApi(ChangedOpenApi changedApi) {
            this.changedApi = changedApi;
        }

        public List<String> getParseErrors() {
            return parseErrors;
        }

        public void addParseError(String error) {
            parseErrors.add(error);
        }

        public boolean hasParseErrors() {
            return !parseErrors.isEmpty();
        }

        /**
         * Checks if the specifications are unchanged.
         *
         * @return true if there are no differences between the specifications
         */
        public boolean isUnchanged() {
            return changedApi != null && !changedApi.isDifferent();
        }

        /**
         * Checks if the specifications are compatible (no breaking changes).
         *
         * @return true if changes are backward-compatible
         */
        public boolean isCompatible() {
            if (changedApi == null) {
                return false;
            }
            return changedApi.isCompatible();
        }

        /**
         * Gets the diff result indicating the compatibility level.
         *
         * @return the DiffResult enum value, or null if comparison failed
         */
        public DiffResult getDiffResult() {
            if (changedApi == null) {
                return null;
            }
            return changedApi.isCompatible() ? DiffResult.COMPATIBLE : DiffResult.INCOMPATIBLE;
        }
    }
}
