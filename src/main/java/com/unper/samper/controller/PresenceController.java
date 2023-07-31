package com.unper.samper.controller;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.exception.DifferentClassException;
import com.unper.samper.exception.OnScheduleException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.ScheduleNotActiveException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.Presence;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.PresenceCheckInRequestDto;
import com.unper.samper.model.dto.PresenceCheckOutRequestDto;
import com.unper.samper.model.dto.PresenceResponseDto;
import com.unper.samper.service.impl.PresenceServiceImpl;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

@SecurityRequirement(name = "bearer-key")
@Tag(name = "Presence")
@RequestMapping("/presence")
@RestController
public class PresenceController {
    @Autowired
    PresenceServiceImpl presenceServiceImpl;

    @PreAuthorize("hasAuthority('ROLE_LECTURE')")
    @Operation(summary = "Get all presence data by current lecture")
    @GetMapping("/getallbylecture")
    public ResponseEntity<?> getAllByLecture() throws ResourceNotFoundException {
        List<Presence> presenceList = presenceServiceImpl.getAllByLecture();
        List<PresenceResponseDto> responseDtoList = new ArrayList<>();
        presenceList.forEach(presence -> {
            PresenceResponseDto responseDto = PresenceResponseDto.builder()
                .id(presence.getId())
                .studentId(presence.getStudent().getId())
                .scheduleId(presence.getSchedule().getId())
                .checkIn(presence.getCheckIn())
                .checkInLocation(presence.getCheckInLocation())
                .checkOut(presence.getCheckOut())
                .checkOutLocation(presence.getCheckOutLocation())
                .build();
            responseDtoList.add(responseDto);
        });
        Map<String, Integer> metaData = new HashMap<>();
        metaData.put("_total", presenceList.size());   
        return ResponseHandler.generateSuccessResponseWithMeta(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDtoList, metaData);
    }

    @PreAuthorize("hasAuthority('ROLE_STUDENT')")
    @Operation(summary = "Record check in presence")
    @PostMapping("/checkin")
    public ResponseEntity<?> checkIn(@RequestBody PresenceCheckInRequestDto requestDto) throws ResourceNotFoundException, DifferentClassException, ScheduleNotActiveException, OnScheduleException{
        Presence presence = presenceServiceImpl.checkIn(requestDto);
        PresenceResponseDto responseDto = PresenceResponseDto.builder()
            .id(presence.getId())
            .studentId(presence.getStudent().getId())
            .scheduleId(presence.getSchedule().getId())
            .checkInLocation(presence.getCheckInLocation())
            .checkIn(presence.getCheckIn())
            .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.PRESENCE_SUCCESS.getMessage(), responseDto);
    }

    @PreAuthorize("hasAuthority('ROLE_STUDENT')")
    @Operation(summary = "Record check out presence")
    @PatchMapping("/checkout")
    public ResponseEntity<?> checkOut(@RequestBody PresenceCheckOutRequestDto requestDto) throws ResourceNotFoundException, ScheduleNotActiveException {
        Presence presence = presenceServiceImpl.checkOut(requestDto);
        PresenceResponseDto responseDto = PresenceResponseDto.builder()
            .id(presence.getId())
            .studentId(presence.getStudent().getId())
            .scheduleId(presence.getSchedule().getId())
            .checkIn(presence.getCheckIn())
            .checkInLocation(presence.getCheckInLocation())
            .checkOut(presence.getCheckOut())
            .checkOutLocation(presence.getCheckOutLocation())
            .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.PRESENCE_SUCCESS.getMessage(), responseDto);
    }
}
