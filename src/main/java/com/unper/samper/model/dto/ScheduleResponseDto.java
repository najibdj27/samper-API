package com.unper.samper.model.dto;

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

    private String meetingOrder;

    private String timeStart;

    private String timeEnd;

    private Short creditAmount;

    private Boolean isActive;

    private Boolean geolocationFlag;
}
