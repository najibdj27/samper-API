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
public class PresenceCheckOutRequestDto {
    private Long id;

    private Point checkOutLocation;
}
