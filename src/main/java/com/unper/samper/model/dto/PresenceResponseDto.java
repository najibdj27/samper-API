package com.unper.samper.model.dto;

import java.time.LocalDateTime;

import org.springframework.data.geo.Point;

import com.unper.samper.model.Student;
import com.unper.samper.model.Class;

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

    private Student student;

    private Class kelas;

    private ScheduleResponseDto schedule;

    private LocalDateTime checkin;

    private LocalDateTime checkout;

    private Point location;
}
