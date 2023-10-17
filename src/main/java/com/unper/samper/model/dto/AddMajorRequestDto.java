package com.unper.samper.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class AddMajorRequestDto {
    private String majorCode;

    private String name;

    private Long majorHeadId;
}
