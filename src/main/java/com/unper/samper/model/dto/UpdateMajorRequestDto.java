package com.unper.samper.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class UpdateMajorRequestDto {
    private Long id;

    private String majorCode;

    private String majorName;

    private Long majorHeadId;
}
