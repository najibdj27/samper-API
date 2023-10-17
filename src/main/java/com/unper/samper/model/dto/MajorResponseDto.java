package com.unper.samper.model.dto;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class MajorResponseDto {
    private Long id;

    private String majorCode;

    private String name;

    private LectureResponseDto majorHead;

    private List<SubjectResponseDto> subjects;
}
