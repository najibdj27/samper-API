package com.unper.samper.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.dto.RegisterLectureRequestDto;
import com.unper.samper.model.dto.RegisterStudentRequestDto;
import com.unper.samper.service.impl.RegistrationServiceImpl;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

@Tag(name = "2. Registration Controller")
@RestController
@RequestMapping("/registration")
public class RegistrationController {
    @Autowired
    RegistrationServiceImpl registrationServiceImpl;
    
    @Operation(summary = "Register new student")
    @PostMapping("/registerstudent")
    public ResponseEntity<?> registerStudent(@RequestBody RegisterStudentRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException{
        return registrationServiceImpl.registerStudent(requestDto);
    }
    
    @Operation(summary = "Register new lecture")
    @PostMapping("/registerlecture")
    public ResponseEntity<?> registerLecture(@RequestBody RegisterLectureRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException{
        return registrationServiceImpl.registerLecture(requestDto);
    }
}
