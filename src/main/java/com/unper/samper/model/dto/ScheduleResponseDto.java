package com.unper.samper.model.dto;

import java.util.Calendar;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class ScheduleResponseDto {
    private Long id;

    private ClassResponseDto kelas;

    private SubjectResponseDto subject;

    private LectureResponseDto lecture;

    private Calendar timeStart;

    private Calendar timeEnd;

    private Boolean isActive;
}
