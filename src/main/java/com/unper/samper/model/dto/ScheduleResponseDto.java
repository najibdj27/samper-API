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

    private Long classId;

    private Long subjectId;

    private Calendar timeStart;

    private Calendar timeEnd;

    private Boolean isActive;
}
