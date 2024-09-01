package com.unper.samper.model.dto;

import java.time.LocalDateTime;

import org.springframework.data.geo.Point;

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
    private LocalDateTime time;

    private Character status;

    private Point location;
}
