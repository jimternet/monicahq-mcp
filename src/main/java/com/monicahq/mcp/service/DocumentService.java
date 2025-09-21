package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class DocumentService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> createDocument(Map<String, Object> arguments) {
        log.info("Creating document with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            validateDocumentCreateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.post("/documents", apiRequest)
                .map(this::formatDocumentResponse)
                .doOnSuccess(result -> log.info("Document created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create document: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for document creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getDocument(Map<String, Object> arguments) {
        log.info("Getting document with arguments: {}", arguments);
        
        try {
            Long documentId = extractDocumentId(arguments);
            
            return monicaClient.get("/documents/" + documentId, null)
                .map(this::formatDocumentResponse)
                .doOnSuccess(result -> log.info("Document retrieved successfully: {}", documentId))
                .doOnError(error -> log.error("Failed to get document {}: {}", documentId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for document retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updateDocument(Map<String, Object> arguments) {
        log.info("Updating document with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            Long documentId = extractDocumentId(mutableArguments);
            validateDocumentUpdateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.put("/documents/" + documentId, apiRequest)
                .map(this::formatDocumentResponse)
                .doOnSuccess(result -> log.info("Document updated successfully: {}", documentId))
                .doOnError(error -> log.error("Failed to update document {}: {}", documentId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for document update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deleteDocument(Map<String, Object> arguments) {
        log.info("Deleting document with arguments: {}", arguments);
        
        try {
            Long documentId = extractDocumentId(arguments);
            
            return monicaClient.delete("/documents/" + documentId)
                .map(response -> {
                    String deletionMessage = String.format("Document %d deleted successfully", documentId);
                    return Map.of(
                        "content", List.of(Map.of(
                            "type", "text",
                            "text", deletionMessage
                        )),
                        "data", Map.of("deleted", true, "id", documentId)
                    );
                })
                .doOnSuccess(result -> log.info("Document deleted successfully: {}", documentId))
                .doOnError(error -> log.error("Failed to delete document {}: {}", documentId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for document deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listDocuments(Map<String, Object> arguments) {
        log.info("Listing documents with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/documents", queryParams)
                .map(this::formatDocumentsListResponse)
                .doOnSuccess(result -> log.info("Documents listed successfully"))
                .doOnError(error -> log.error("Failed to list documents: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error listing documents: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validateDocumentCreateArguments(Map<String, Object> arguments) {
        if (!arguments.containsKey("contactId") || arguments.get("contactId") == null) {
            throw new IllegalArgumentException("contactId is required");
        }
        if (!arguments.containsKey("filename") || arguments.get("filename") == null || 
            arguments.get("filename").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("filename is required");
        }
    }

    private void validateDocumentUpdateArguments(Map<String, Object> arguments) {
        // For updates, fields are optional
    }

    private Long extractDocumentId(Map<String, Object> arguments) {
        Object idObj = arguments.get("id");
        if (idObj == null) {
            throw new IllegalArgumentException("id is required");
        }
        
        if (idObj instanceof Number) {
            return ((Number) idObj).longValue();
        }
        
        try {
            return Long.parseLong(idObj.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("id must be a valid number");
        }
    }

    private Map<String, Object> mapToApiFormat(Map<String, Object> arguments) {
        Map<String, Object> apiRequest = new HashMap<>();
        
        if (arguments.containsKey("contactId")) {
            apiRequest.put("contact_id", arguments.get("contactId"));
        }
        if (arguments.containsKey("filename")) {
            apiRequest.put("filename", arguments.get("filename"));
        }
        if (arguments.containsKey("originalFilename")) {
            apiRequest.put("original_filename", arguments.get("originalFilename"));
        }
        if (arguments.containsKey("mimeType")) {
            apiRequest.put("mime_type", arguments.get("mimeType"));
        }
        if (arguments.containsKey("size")) {
            apiRequest.put("size", arguments.get("size"));
        }
        if (arguments.containsKey("description")) {
            apiRequest.put("description", arguments.get("description"));
        }
        
        return apiRequest;
    }

    private Map<String, String> buildListQueryParams(Map<String, Object> arguments) {
        Map<String, String> queryParams = new HashMap<>();
        
        if (arguments.containsKey("limit")) {
            queryParams.put("limit", arguments.get("limit").toString());
        } else {
            queryParams.put("limit", "10");
        }
        
        if (arguments.containsKey("page")) {
            queryParams.put("page", arguments.get("page").toString());
        } else {
            queryParams.put("page", "1");
        }
        
        return queryParams;
    }

    private Map<String, Object> formatDocumentResponse(Map<String, Object> apiResponse) {
        Map<String, Object> rawApiData;
        Map<String, Object> documentData;
        
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            rawApiData = rawData;
            documentData = mapFromApiFormat(rawData);
        } else {
            rawApiData = apiResponse;
            documentData = mapFromApiFormat(apiResponse);
        }
        
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", documentData);
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> formatDocumentsListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> documents = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedDocuments = documents.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedDocuments);
        
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) apiResponse.get("meta");
        if (meta != null) {
            result.put("meta", meta);
        }
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> mapFromApiFormat(Map<String, Object> apiData) {
        Map<String, Object> result = new HashMap<>();
        
        apiData.forEach((key, value) -> {
            switch (key) {
                case "contact_id" -> result.put("contactId", value);
                case "original_filename" -> result.put("originalFilename", value);
                case "mime_type" -> result.put("mimeType", value);
                case "download_url" -> result.put("downloadUrl", value);
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}