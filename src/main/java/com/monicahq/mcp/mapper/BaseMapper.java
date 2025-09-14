package com.monicahq.mcp.mapper;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface BaseMapper {
    
    DateTimeFormatter ISO_DATE_TIME = DateTimeFormatter.ISO_DATE_TIME;
    DateTimeFormatter ISO_DATE = DateTimeFormatter.ISO_DATE;
    
    default Long objectToLong(Object obj) {
        if (obj == null) return null;
        if (obj instanceof Long) return (Long) obj;
        if (obj instanceof Integer) return ((Integer) obj).longValue();
        if (obj instanceof String) return Long.parseLong((String) obj);
        return null;
    }
    
    default String objectToString(Object obj) {
        if (obj == null) return null;
        return obj.toString();
    }
    
    default Boolean objectToBoolean(Object obj) {
        if (obj == null) return null;
        if (obj instanceof Boolean) return (Boolean) obj;
        if (obj instanceof String) return Boolean.parseBoolean((String) obj);
        return null;
    }
    
    default Integer objectToInteger(Object obj) {
        if (obj == null) return null;
        if (obj instanceof Integer) return (Integer) obj;
        if (obj instanceof Long) return ((Long) obj).intValue();
        if (obj instanceof String) return Integer.parseInt((String) obj);
        return null;
    }
    
    default LocalDateTime objectToLocalDateTime(Object obj) {
        if (obj == null) return null;
        if (obj instanceof LocalDateTime) return (LocalDateTime) obj;
        if (obj instanceof String) return LocalDateTime.parse((String) obj, ISO_DATE_TIME);
        return null;
    }
    
    default LocalDate objectToLocalDate(Object obj) {
        if (obj == null) return null;
        if (obj instanceof LocalDate) return (LocalDate) obj;
        if (obj instanceof String) return LocalDate.parse((String) obj, ISO_DATE);
        return null;
    }
    
    @SuppressWarnings("unchecked")
    default List<Long> objectToLongList(Object obj) {
        if (obj == null) return null;
        if (obj instanceof List) return (List<Long>) obj;
        return null;
    }
    
    default Map<String, Object> createMap() {
        return new HashMap<>();
    }
}
