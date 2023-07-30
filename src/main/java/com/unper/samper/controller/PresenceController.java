package com.unper.samper.controller;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.exception.DifferentClassException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.ScheduleNotActiveException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.Presence;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.PresenceCheckInRequestDto;
import com.unper.samper.model.dto.PresenceCheckOutRequestDto;
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

    @PreAuthorize("hasAuthority('ROLE_ADMIN') or hasAuthority('ROLE_LECTURE') or hasAuthority('ROLE_STUDENT')")
    @Operation(summary = "Get all presence data")
    public ResponseEntity<?> getAll() throws ResourceNotFoundException {
        List<Presence> presenceList = presenceServiceImpl.getAll();
        Map<String, Integer> metaData = new HashMap<>();
        metaData.put("_total", presenceList.size());   
        return ResponseHandler.generateSuccessResponseWithMeta(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), metaData, presenceList);
    }

    @PreAuthorize("hasAuthority('ROLE_STUDENT')")
    @Operation(summary = "Record check in presence")
    @PostMapping("/checkin")
    public ResponseEntity<?> checkIn(PresenceCheckInRequestDto requestDto) throws ResourceNotFoundException, DifferentClassException, ScheduleNotActiveException{
        Presence presence = presenceServiceImpl.checkIn(requestDto);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.PRESENCE_SUCCESS.getMessage(), presence);
    }

    @PreAuthorize("hasAuthority('ROLE_STUDENT')")
    @Operation(summary = "Record check out presence")
    public ResponseEntity<?> checkOut(PresenceCheckOutRequestDto requestDto) throws ResourceNotFoundException, ScheduleNotActiveException {
        Presence presence = presenceServiceImpl.checkOut(requestDto);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.PRESENCE_SUCCESS.getMessage(), presence);
    }
}
