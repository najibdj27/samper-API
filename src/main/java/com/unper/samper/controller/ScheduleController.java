package com.unper.samper.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.AddScheduleRequestDto;
import com.unper.samper.service.impl.ScheduleServiceImpl;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

@SecurityRequirement(name = "bearer-key")
@Tag(name = "6. Schedule Controller")
@RestController
@RequestMapping("/schedule")
public class ScheduleController {
    @Autowired
    ScheduleServiceImpl scheduleServiceImpl;

    @Operation(summary = "Add new schedule")
    @PreAuthorize("hasAuthority('ROLE_ADMIN') or hasAuthority('ROLE_LECTURE')")
    @PostMapping("/add")
    public ResponseEntity<?> add(@RequestBody AddScheduleRequestDto requestDto) throws ResourceNotFoundException, ResourceAlreadyExistException {
        scheduleServiceImpl.add(requestDto);
        return ResponseHandler.generateSuccessResponse(HttpStatus.CREATED, EResponseMessage.INSERT_DATA_SUCCESS.getMessage(), null);
    }
}