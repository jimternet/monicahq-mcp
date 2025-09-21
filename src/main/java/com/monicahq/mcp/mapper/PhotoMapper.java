package com.monicahq.mcp.mapper;

import com.monicahq.mcp.dto.Photo;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class PhotoMapper {

    public Photo toDto(Map<String, Object> apiData) {
        if (apiData == null) {
            return null;
        }

        return Photo.builder()
            .id(getLong(apiData, "id"))
            .contactId(getLong(apiData, "contact_id"))
            .filename(getString(apiData, "filename"))
            .originalFilename(getString(apiData, "original_filename"))
            .width(getInteger(apiData, "width"))
            .height(getInteger(apiData, "height"))
            .filesize(getLong(apiData, "filesize"))
            .mimeType(getString(apiData, "mime_type"))
            .link(getString(apiData, "link"))
            .createdAt(getLocalDateTime(apiData, "created_at"))
            .updatedAt(getLocalDateTime(apiData, "updated_at"))
            .build();
    }

    public Map<String, Object> fromDto(Photo photo) {
        if (photo == null) {
            return null;
        }

        return Map.of(
            "contact_id", photo.getContactId() != null ? photo.getContactId() : "",
            "filename", photo.getFilename() != null ? photo.getFilename() : "",
            "original_filename", photo.getOriginalFilename() != null ? photo.getOriginalFilename() : "",
            "width", photo.getWidth() != null ? photo.getWidth() : "",
            "height", photo.getHeight() != null ? photo.getHeight() : "",
            "filesize", photo.getFilesize() != null ? photo.getFilesize() : "",
            "mime_type", photo.getMimeType() != null ? photo.getMimeType() : ""
        );
    }

    public void updateFromDto(Photo target, Photo source) {
        if (target == null || source == null) {
            return;
        }

        if (source.getContactId() != null) {
            target.setContactId(source.getContactId());
        }
        if (source.getFilename() != null) {
            target.setFilename(source.getFilename());
        }
        if (source.getOriginalFilename() != null) {
            target.setOriginalFilename(source.getOriginalFilename());
        }
        if (source.getWidth() != null) {
            target.setWidth(source.getWidth());
        }
        if (source.getHeight() != null) {
            target.setHeight(source.getHeight());
        }
        if (source.getFilesize() != null) {
            target.setFilesize(source.getFilesize());
        }
        if (source.getMimeType() != null) {
            target.setMimeType(source.getMimeType());
        }
    }

    private Long getLong(Map<String, Object> map, String key) {
        Object value = map.get(key);
        if (value == null) return null;
        if (value instanceof Number) return ((Number) value).longValue();
        try {
            return Long.parseLong(value.toString());
        } catch (NumberFormatException e) {
            return null;
        }
    }

    private Integer getInteger(Map<String, Object> map, String key) {
        Object value = map.get(key);
        if (value == null) return null;
        if (value instanceof Number) return ((Number) value).intValue();
        try {
            return Integer.parseInt(value.toString());
        } catch (NumberFormatException e) {
            return null;
        }
    }

    private String getString(Map<String, Object> map, String key) {
        Object value = map.get(key);
        return value != null ? value.toString() : null;
    }

    private java.time.LocalDateTime getLocalDateTime(Map<String, Object> map, String key) {
        String value = getString(map, key);
        if (value == null) return null;
        try {
            return java.time.LocalDateTime.parse(value.replace("Z", ""));
        } catch (Exception e) {
            return null;
        }
    }
}