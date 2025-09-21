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
public class PhotoService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> createPhoto(Map<String, Object> arguments) {
        log.info("Creating photo with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            validatePhotoCreateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.post("/photos", apiRequest)
                .map(this::formatPhotoResponse)
                .doOnSuccess(result -> log.info("Photo created successfully: {}", result))
                .doOnError(error -> log.error("Failed to create photo: {}", error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for photo creation: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> getPhoto(Map<String, Object> arguments) {
        log.info("Getting photo with arguments: {}", arguments);
        
        try {
            Long photoId = extractPhotoId(arguments);
            
            return monicaClient.get("/photos/" + photoId, null)
                .map(this::formatPhotoResponse)
                .doOnSuccess(result -> log.info("Photo retrieved successfully: {}", photoId))
                .doOnError(error -> log.error("Failed to get photo {}: {}", photoId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for photo retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> updatePhoto(Map<String, Object> arguments) {
        log.info("Updating photo with arguments: {}", arguments);
        
        try {
            Map<String, Object> mutableArguments = new HashMap<>(arguments);
            Long photoId = extractPhotoId(mutableArguments);
            validatePhotoUpdateArguments(mutableArguments);
            Map<String, Object> apiRequest = mapToApiFormat(mutableArguments);
            
            return monicaClient.put("/photos/" + photoId, apiRequest)
                .map(this::formatPhotoResponse)
                .doOnSuccess(result -> log.info("Photo updated successfully: {}", photoId))
                .doOnError(error -> log.error("Failed to update photo {}: {}", photoId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for photo update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> deletePhoto(Map<String, Object> arguments) {
        log.info("Deleting photo with arguments: {}", arguments);
        
        try {
            Long photoId = extractPhotoId(arguments);
            
            return monicaClient.delete("/photos/" + photoId)
                .map(response -> {
                    String deletionMessage = String.format("Photo %d deleted successfully", photoId);
                    return Map.of(
                        "content", List.of(Map.of(
                            "type", "text",
                            "text", deletionMessage
                        )),
                        "data", Map.of("deleted", true, "id", photoId)
                    );
                })
                .doOnSuccess(result -> log.info("Photo deleted successfully: {}", photoId))
                .doOnError(error -> log.error("Failed to delete photo {}: {}", photoId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for photo deletion: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listPhotos(Map<String, Object> arguments) {
        log.info("Listing photos with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/photos", queryParams)
                .map(this::formatPhotosListResponse)
                .doOnSuccess(result -> log.info("Photos listed successfully"))
                .doOnError(error -> log.error("Failed to list photos: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error listing photos: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private void validatePhotoCreateArguments(Map<String, Object> arguments) {
        if (!arguments.containsKey("contactId") || arguments.get("contactId") == null) {
            throw new IllegalArgumentException("contactId is required");
        }
        if (!arguments.containsKey("filename") || arguments.get("filename") == null || 
            arguments.get("filename").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("filename is required");
        }
    }

    private void validatePhotoUpdateArguments(Map<String, Object> arguments) {
        // For updates, fields are optional
    }

    private Long extractPhotoId(Map<String, Object> arguments) {
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
        if (arguments.containsKey("width")) {
            apiRequest.put("width", arguments.get("width"));
        }
        if (arguments.containsKey("height")) {
            apiRequest.put("height", arguments.get("height"));
        }
        if (arguments.containsKey("filesize")) {
            apiRequest.put("filesize", arguments.get("filesize"));
        }
        if (arguments.containsKey("mimeType")) {
            apiRequest.put("mime_type", arguments.get("mimeType"));
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

    private Map<String, Object> formatPhotoResponse(Map<String, Object> apiResponse) {
        Map<String, Object> rawApiData;
        Map<String, Object> photoData;
        
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            rawApiData = rawData;
            photoData = mapFromApiFormat(rawData);
        } else {
            rawApiData = apiResponse;
            photoData = mapFromApiFormat(apiResponse);
        }
        
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", photoData);
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> formatPhotosListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> photos = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedPhotos = photos.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedPhotos);
        
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
                case "created_at" -> result.put("createdAt", value);
                case "updated_at" -> result.put("updatedAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}