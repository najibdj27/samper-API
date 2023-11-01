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
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.Role;
import com.unper.samper.model.Subject;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.AddSubjectLecturesRequestDto;
import com.unper.samper.model.dto.AddSubjectrequestDto;
import com.unper.samper.model.dto.LectureResponseDto;
import com.unper.samper.model.dto.SubjectResponseDto;
import com.unper.samper.model.dto.UserResponseDto;
import com.unper.samper.service.impl.SubjectServiceImpl;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

@Tag(name = "Subject")
@RestController
@RequestMapping("/subject")
@SecurityRequirement(name = "bearer-key")
public class SubjectController {
    @Autowired
    SubjectServiceImpl subjectServiceImpl;

    @Operation(summary = "Get data of all subjects")
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('LECTURE') or hasAuthority('STUDENT')")
    @GetMapping("/all")
    public ResponseEntity<?> getAll() throws ResourceNotFoundException {
        List<Subject> subjectList = subjectServiceImpl.getAll();
        List<SubjectResponseDto> responseDtoList = new ArrayList<>();
        subjectList.forEach(subject -> {
            List<LectureResponseDto> lectureResponseDtoList = new ArrayList<>();
            subject.getLectures().forEach(lecture -> {
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
                LectureResponseDto lectureResponseDto = LectureResponseDto.builder()
                    .id(lecture.getId())
                    .NIP(lecture.getNIP())
                    .user(userResponseDto)
                    .build();
                lectureResponseDtoList.add(lectureResponseDto);
            });
            SubjectResponseDto responseDto = SubjectResponseDto.builder()
                .id(subject.getId())
                .name(subject.getName())
                .lecture(lectureResponseDtoList)
                .build();
            responseDtoList.add(responseDto);
        });
        return ResponseHandler.generateSuccessResponseWithMeta(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDtoList, null);
    }

    @Operation(summary = "Get subject by id")
    @PreAuthorize("hasAuthorization('ADMIN') or hasAuthority('LECTURE') or hasAuthority('STUDENT')")
    @GetMapping("/get/{id}")
    public ResponseEntity<?> getById(@PathVariable("id") Long id) throws ResourceNotFoundException {
        Subject subject = subjectServiceImpl.getById(id);
        List<LectureResponseDto> lectureResponseDtoList = new ArrayList<>();
        subject.getLectures().forEach(lecture -> {
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
            LectureResponseDto lectureResponseDto = LectureResponseDto.builder()
                .id(lecture.getId())
                .NIP(lecture.getNIP())
                .user(userResponseDto)
                .build();
            lectureResponseDtoList.add(lectureResponseDto);
        });
        SubjectResponseDto responseDto = SubjectResponseDto.builder()
            .id(subject.getId())
            .name(subject.getName())
            .lecture(lectureResponseDtoList)
            .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDto);
    }

    @Operation(summary = "Create a new subject")
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('LECTURE')")
    @PostMapping("/add")
    public ResponseEntity<?> add(@RequestBody AddSubjectrequestDto requestDto) throws ResourceAlreadyExistException{
        subjectServiceImpl.addSubject(requestDto);
        return ResponseHandler.generateSuccessResponse(HttpStatus.CREATED, EResponseMessage.INSERT_DATA_SUCCESS.getMessage(), null);
    }

    @Operation(summary = "Add lectures data of a subject")
    @PreAuthorize("hasAuthority('ADMIN')")
    @PatchMapping("/add/lectures")
    public ResponseEntity<?> addSubjectLectures(@RequestBody AddSubjectLecturesRequestDto requestDto) throws ResourceNotFoundException{
        Subject newSubject = subjectServiceImpl.addSubjectLecture(requestDto);
        List<LectureResponseDto> lectureResponseDtoList = new ArrayList<>();
        newSubject.getLectures().forEach(lecture -> {
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
            LectureResponseDto lectureResponseDto = LectureResponseDto.builder()
                .id(lecture.getId())
                .NIP(lecture.getNIP())
                .user(userResponseDto)
                .build();
            lectureResponseDtoList.add(lectureResponseDto);
        });
        SubjectResponseDto responseDto = SubjectResponseDto.builder()
            .id(newSubject.getId())
            .name(newSubject.getName())
            .lecture(lectureResponseDtoList)
            .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.EDIT_DATA_SUCCESS.getMessage(), responseDto);
    }

    @Operation(summary = "Soft delete a subject")
    @PreAuthorize("hasAuthority('ADMIN')")
    @PatchMapping("/delete/{id}")
    public ResponseEntity<?> delete(@PathVariable("id") Long id) throws ResourceNotFoundException { 
        subjectServiceImpl.delete(id);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.DELETE_SUCCESS.getMessage(), null);
    }
}
