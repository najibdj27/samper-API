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
import com.unper.samper.model.dto.AddClassRequestDto;
import com.unper.samper.service.impl.ClassServiceImpl;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

@SecurityRequirement(name = "bearer-key")
@Tag(name = "6. Class Controller")
@RestController
@RequestMapping("/class")
public class ClassController {
    @Autowired
    ClassServiceImpl classServiceImpl;

    @Operation(summary = "Add new class")
    @PreAuthorize("hasAuthority('ROLE_ADMIN') or hasAuthority('ROLE_LECTURE')")
    @PostMapping("/add")
    public ResponseEntity<?> add(@RequestBody AddClassRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException {
        classServiceImpl.addClass(requestDto);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.INSERT_DATA_SUCCESS.getMessage(), null);
    }
}
