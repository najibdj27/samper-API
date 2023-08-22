package com.unper.samper.model.dto;

import java.util.Set;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class GetAllPrevillageByAdminResponseDto {
    private Integer id;
    private String name;
    private String nameDisplay;
    private String nameDb;
    private String url;
    private String description;
    private Set<String> previllages;
}
