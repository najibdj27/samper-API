package com.unper.samper.model.dto;

import com.unper.samper.model.BasePrevillage;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class PrevilllageResponseDto {
    private Integer id;

    private BasePrevillage basePrevillage;

    private String url;

    private String previllage;
}
