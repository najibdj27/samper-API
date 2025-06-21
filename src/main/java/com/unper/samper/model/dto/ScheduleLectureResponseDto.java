package com.unper.samper.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class ScheduleLectureResponseDto {
    private Long id;

    private ClassResponseDto kelas;

    private SubjectResponseDto subject;

    private LectureResponseDto lecture;

    private String meetingOrder;

    private String timeStart;

    private String timeEnd;

    private String openTime;

    private String closeTime;

    private Short creditAmount;

    private Boolean isActive;

    private Boolean geolocationFlag;
}
