package com.monicahq.mcp.mapper;

import com.monicahq.mcp.dto.Address;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class AddressMapper {

    public Address toDto(Map<String, Object> apiData) {
        if (apiData == null) {
            return null;
        }

        return Address.builder()
            .id(getLong(apiData, "id"))
            .contactId(getLong(apiData, "contact_id"))
            .name(getString(apiData, "name"))
            .street(getString(apiData, "street"))
            .city(getString(apiData, "city"))
            .province(getString(apiData, "province"))
            .postalCode(getString(apiData, "postal_code"))
            .country(getString(apiData, "country"))
            .latitude(getDouble(apiData, "latitude"))
            .longitude(getDouble(apiData, "longitude"))
            .createdAt(getLocalDateTime(apiData, "created_at"))
            .updatedAt(getLocalDateTime(apiData, "updated_at"))
            .build();
    }

    public Map<String, Object> fromDto(Address address) {
        if (address == null) {
            return null;
        }

        return Map.of(
            "contact_id", address.getContactId() != null ? address.getContactId() : "",
            "name", address.getName() != null ? address.getName() : "",
            "street", address.getStreet() != null ? address.getStreet() : "",
            "city", address.getCity() != null ? address.getCity() : "",
            "province", address.getProvince() != null ? address.getProvince() : "",
            "postal_code", address.getPostalCode() != null ? address.getPostalCode() : "",
            "country", address.getCountry() != null ? address.getCountry() : "",
            "latitude", address.getLatitude() != null ? address.getLatitude() : "",
            "longitude", address.getLongitude() != null ? address.getLongitude() : ""
        );
    }

    public void updateFromDto(Address target, Address source) {
        if (target == null || source == null) {
            return;
        }

        if (source.getContactId() != null) {
            target.setContactId(source.getContactId());
        }
        if (source.getName() != null) {
            target.setName(source.getName());
        }
        if (source.getStreet() != null) {
            target.setStreet(source.getStreet());
        }
        if (source.getCity() != null) {
            target.setCity(source.getCity());
        }
        if (source.getProvince() != null) {
            target.setProvince(source.getProvince());
        }
        if (source.getPostalCode() != null) {
            target.setPostalCode(source.getPostalCode());
        }
        if (source.getCountry() != null) {
            target.setCountry(source.getCountry());
        }
        if (source.getLatitude() != null) {
            target.setLatitude(source.getLatitude());
        }
        if (source.getLongitude() != null) {
            target.setLongitude(source.getLongitude());
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

    private Double getDouble(Map<String, Object> map, String key) {
        Object value = map.get(key);
        if (value == null) return null;
        if (value instanceof Number) return ((Number) value).doubleValue();
        try {
            return Double.parseDouble(value.toString());
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