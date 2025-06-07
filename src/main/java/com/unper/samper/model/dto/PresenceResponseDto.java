package com.unper.samper.model.dto;

import java.util.Calendar;

import com.fasterxml.jackson.annotation.JsonFormat;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class PresenceResponseDto {
    private Long id;

    private StudentResponseDto student;

    private ScheduleResponseDto schedule;

    @JsonFormat(pattern = "YYYY-MM-DD HH:mm")
    private Calendar time;

    private Character type;

    private Double longitude;

    private Double latitude;
}
