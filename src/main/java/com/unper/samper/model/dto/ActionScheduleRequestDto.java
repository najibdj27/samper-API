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
public class ActionScheduleRequestDto {
    private Long scheduleId;

    private Double longitude;

    private Double latitude;

    private Boolean geolocationFlag;

    private String imageBase64;
}
