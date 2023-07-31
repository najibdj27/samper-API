package com.unper.samper.model.dto;

import org.springframework.data.geo.Point;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class PresenceCheckInRequestDto {

    private Long scheduleId;

    private Point cehckInLocation;
}
