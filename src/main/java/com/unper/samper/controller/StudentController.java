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
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.Role;
import com.unper.samper.model.Student;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.StudentResponseDto;
import com.unper.samper.model.dto.UserResponseDto;
import com.unper.samper.service.impl.StudentServiceImpl;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

@Tag(name = "3. Student Controller")
@RestController
@RequestMapping("/student")
@SecurityRequirement(name = "bearer-key")
public class StudentController {
    @Autowired
    StudentServiceImpl studentServiceImpl;
    
    /***
     * Get data of all student 
     * @throws ResourceNotFoundException
     */
    @Operation(summary = "Get data of all student")
    @PreAuthorize("hasAuthority('ROLE_ADMIN') or hasAuthority('ROLE_LECTURE')")
    @GetMapping("/all")
    public ResponseEntity<?> getAll() throws ResourceNotFoundException {
        List<Student> studentList = studentServiceImpl.getAll();

        List<StudentResponseDto> responseDtoList = new ArrayList<>();
        studentList.forEach(student -> {
            List<String> roleList = new ArrayList<>();
            for (Role role : student.getUser().getRoles()) {
                roleList.add(role.getName().toString());
            }
            UserResponseDto userResponseDto = UserResponseDto.builder()
                .id(student.getUser().getId())
                .firstName(student.getUser().getFirstName())
                .lastName(student.getUser().getLastName())
                .dateOfBirth(student.getUser().getDateOfBirth())
                .username(student.getUser().getUsername())
                .email(student.getUser().getEmail())
                .phoneNumber(student.getUser().getPhoneNumber())
                .roles(roleList)
                .build();
            
            StudentResponseDto responseDto = StudentResponseDto.builder()
                .NIM(student.getNIM())
                .user(userResponseDto)
                .build();
            responseDtoList.add(responseDto);
        });

        Map<String, Object> metaData = new HashMap<>();
        metaData.put("_total", responseDtoList.size());
        return ResponseHandler.generateSuccessResponseWithMeta(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDtoList, metaData);
    }
}
