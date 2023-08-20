package com.unper.samper.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.RegisterAdminRequestDto;
import com.unper.samper.model.dto.RegisterLectureRequestDto;
import com.unper.samper.model.dto.RegisterStudentRequestDto;
import com.unper.samper.service.impl.RegistrationServiceImpl;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

@Tag(name = "Registration")
@RestController
@RequestMapping("/registration")
public class RegistrationController {
    @Autowired
    RegistrationServiceImpl registrationServiceImpl;
    
    @Operation(summary = "Register new student")
    @PostMapping("/registerstudent")
    public ResponseEntity<?> registerStudent(@RequestBody RegisterStudentRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException{
        registrationServiceImpl.registerStudent(requestDto);
        return ResponseHandler.generateSuccessResponse(HttpStatus.CREATED, EResponseMessage.REGISTRATION_SUCCESS.getMessage(), null);
    }
    
    @Operation(summary = "Register new lecture")
    @PostMapping("/registerlecture")
    public ResponseEntity<?> registerLecture(@RequestBody RegisterLectureRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException{
        registrationServiceImpl.registerLecture(requestDto);
        return ResponseHandler.generateSuccessResponse(HttpStatus.CREATED, EResponseMessage.REGISTRATION_SUCCESS.getMessage(), null);
    }

    @Operation(summary = "Register new admin")
    @PostMapping("/registeradmin")
    public ResponseEntity<?> registerAdmin(@RequestBody RegisterAdminRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException{
        registrationServiceImpl.registerAdmin(requestDto);
        return ResponseHandler.generateSuccessResponse(HttpStatus.CREATED, EResponseMessage.REGISTRATION_SUCCESS.getMessage(), null);
    }
}
