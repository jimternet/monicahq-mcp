package com.monicahq.mcp.mapper;

import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface RelationshipMapper {
    // Manual mapping implementation will be done in service layer
    // MapStruct is too complex for Map<String,Object> to DTO conversions
}