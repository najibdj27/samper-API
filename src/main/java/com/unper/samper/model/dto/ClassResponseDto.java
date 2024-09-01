package com.unper.samper.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class ClassResponseDto {
    private Long id;

    private LectureResponseDto lecture;

    private MajorResponseDto major;

    private String name;
}
