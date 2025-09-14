package com.monicahq.mcp.framework;

import java.util.List;
import java.util.Map;

public class EntityOperationMetadata {
    
    private final String operation;
    private final String description;
    private final Map<String, Object> inputSchema;
    private final Map<String, Object> outputSchema;
    private final List<String> requiredFields;
    private final List<String> optionalFields;
    private final boolean requiresAuth;
    private final String httpMethod;
    private final String endpoint;

    public EntityOperationMetadata(String operation, String description, 
                                 Map<String, Object> inputSchema, 
                                 Map<String, Object> outputSchema,
                                 List<String> requiredFields,
                                 List<String> optionalFields,
                                 boolean requiresAuth,
                                 String httpMethod,
                                 String endpoint) {
        this.operation = operation;
        this.description = description;
        this.inputSchema = inputSchema;
        this.outputSchema = outputSchema;
        this.requiredFields = requiredFields;
        this.optionalFields = optionalFields;
        this.requiresAuth = requiresAuth;
        this.httpMethod = httpMethod;
        this.endpoint = endpoint;
    }

    public String getOperation() {
        return operation;
    }

    public String getDescription() {
        return description;
    }

    public Map<String, Object> getInputSchema() {
        return inputSchema;
    }

    public Map<String, Object> getOutputSchema() {
        return outputSchema;
    }

    public List<String> getRequiredFields() {
        return requiredFields;
    }

    public List<String> getOptionalFields() {
        return optionalFields;
    }

    public boolean isRequiresAuth() {
        return requiresAuth;
    }

    public String getHttpMethod() {
        return httpMethod;
    }

    public String getEndpoint() {
        return endpoint;
    }

    public static Builder builder() {
        return new Builder();
    }

    public static class Builder {
        private String operation;
        private String description;
        private Map<String, Object> inputSchema;
        private Map<String, Object> outputSchema;
        private List<String> requiredFields;
        private List<String> optionalFields;
        private boolean requiresAuth = true;
        private String httpMethod;
        private String endpoint;

        public Builder operation(String operation) {
            this.operation = operation;
            return this;
        }

        public Builder description(String description) {
            this.description = description;
            return this;
        }

        public Builder inputSchema(Map<String, Object> inputSchema) {
            this.inputSchema = inputSchema;
            return this;
        }

        public Builder outputSchema(Map<String, Object> outputSchema) {
            this.outputSchema = outputSchema;
            return this;
        }

        public Builder requiredFields(List<String> requiredFields) {
            this.requiredFields = requiredFields;
            return this;
        }

        public Builder optionalFields(List<String> optionalFields) {
            this.optionalFields = optionalFields;
            return this;
        }

        public Builder requiresAuth(boolean requiresAuth) {
            this.requiresAuth = requiresAuth;
            return this;
        }

        public Builder httpMethod(String httpMethod) {
            this.httpMethod = httpMethod;
            return this;
        }

        public Builder endpoint(String endpoint) {
            this.endpoint = endpoint;
            return this;
        }

        public EntityOperationMetadata build() {
            return new EntityOperationMetadata(
                operation, description, inputSchema, outputSchema,
                requiredFields, optionalFields, requiresAuth, httpMethod, endpoint
            );
        }
    }
}