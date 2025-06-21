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
public class ScheduleHistoryResponseDto {
    private Long id;

    private ScheduleResponseDto schedule;

    private String openTime;

    private Double openLongitude;
    
    private Double openLatitue;
    
    private String closeTime;

    private Double closeLongitude;
    
    private Double closeLatitude;
}
