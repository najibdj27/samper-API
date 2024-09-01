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
public class PresenceRecordRequestDto {

    private Long scheduleId;

    private Point location;
}
