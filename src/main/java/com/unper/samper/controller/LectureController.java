package com.unper.samper.controller;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.Lecture;
import com.unper.samper.model.Role;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.LectureResponseDto;
import com.unper.samper.model.dto.UserResponseDto;
import com.unper.samper.service.impl.LectureServiceImpl;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

@Tag(name = "Lecture")
@RestController
@RequestMapping("/lecture")
@SecurityRequirement(name = "bearer-key")
public class LectureController {
    @Autowired
    LectureServiceImpl lectureServiceImpl;
    
    @Operation(summary = "Get all data of lectures")
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('LECTURE') or hasAuthority('STUDENT')")
    @GetMapping("/all")
    public ResponseEntity<?> getAll() throws ResourceNotFoundException {
        List<Lecture> lectureList = lectureServiceImpl.getAll();
        List<LectureResponseDto> lectureResponseDtoList = new ArrayList<>();
        lectureList.forEach(lecture -> {
            List<String> roleList = new ArrayList<>();
            for (Role role : lecture.getUser().getRoles()) {
                roleList.add(role.getName().toString());
            }
            UserResponseDto userResponseDto = UserResponseDto.builder()
                .id(lecture.getUser().getId())
                .firstName(lecture.getUser().getFirstName())
                .lastName(lecture.getUser().getLastName())
                .dateOfBirth(lecture.getUser().getDateOfBirth())
                .username(lecture.getUser().getUsername())
                .email(lecture.getUser().getEmail())
                .phoneNumber(lecture.getUser().getPhoneNumber())
                .roles(roleList)
                .build();
            LectureResponseDto responseDto = LectureResponseDto.builder()
                .id(lecture.getId())
                .NIP(lecture.getNIP())
                .user(userResponseDto)
                .build();
            lectureResponseDtoList.add(responseDto);
        });
        return ResponseHandler.generateSuccessResponseWithMeta(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), lectureResponseDtoList, null);
    }

    @Operation(summary = "Get lecture data by id")
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('LECTURE') or hasAuthority('STUDENT')")
    @GetMapping("/get/{id}")
    public ResponseEntity<?> getById(@PathVariable("id") Long id) throws ResourceNotFoundException {
        Lecture lecture = lectureServiceImpl.getById(id);
        List<String> roleList = new ArrayList<>();
        for (Role role : lecture.getUser().getRoles()) {
            roleList.add(role.getName().toString());
        }
        UserResponseDto userResponseDto = UserResponseDto.builder()
            .id(lecture.getUser().getId())
            .firstName(lecture.getUser().getFirstName())
            .lastName(lecture.getUser().getLastName())
            .dateOfBirth(lecture.getUser().getDateOfBirth())
            .username(lecture.getUser().getUsername())
            .email(lecture.getUser().getEmail())
            .phoneNumber(lecture.getUser().getPhoneNumber())
            .roles(roleList)
            .build();
        LectureResponseDto responseDto = LectureResponseDto.builder()
            .id(lecture.getId())
            .NIP(lecture.getNIP())
            .user(userResponseDto)
            .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDto);
    }

    @Operation(summary = "Soft delete a lecture")
    @PreAuthorize("hasAuthority('ADMIN')")
    @PatchMapping("/delete/{id}")
    public ResponseEntity<?> delete(@PathVariable("id") Long id) throws ResourceNotFoundException {
        lectureServiceImpl.delete(id);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.DELETE_SUCCESS.getMessage(), null);
    }
}
