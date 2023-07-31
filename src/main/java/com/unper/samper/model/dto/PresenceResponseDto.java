package com.unper.samper.model.dto;

import java.time.LocalDateTime;

import org.springframework.data.geo.Point;

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

    private Long studentId;

    private Long scheduleId;

    private LocalDateTime checkIn;

    private LocalDateTime checkOut;

    private Point checkInLocation;

    private Point checkOutLocation;
}
