package com.unper.samper.model.dto;

import java.util.Calendar;

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
public class ScheduleHistoryReqeustDto {
    
    private Long scheduleId;

    private Calendar time;

    private Double longitude;

    private Double latitude;
}
