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
import com.unper.samper.model.Major;
import com.unper.samper.model.Role;
import com.unper.samper.model.Subject;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.AddMajorRequestDto;
import com.unper.samper.model.dto.LectureResponseDto;
import com.unper.samper.model.dto.MajorResponseDto;
import com.unper.samper.model.dto.SubjectResponseDto;
import com.unper.samper.model.dto.UpdateMajorRequestDto;
import com.unper.samper.model.dto.UserResponseDto;
import com.unper.samper.service.impl.LectureServiceImpl;
import com.unper.samper.service.impl.MajorServiceImpl;
import com.unper.samper.service.impl.SubjectServiceImpl;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

@SecurityRequirement(name = "bearer-key")
@Tag(name = "Major")
@RequestMapping("/major")
@RestController
public class MajorController {
    @Autowired
    MajorServiceImpl majorServiceImpl;

    @Autowired
    SubjectServiceImpl subjectServiceImpl;

    @Autowired
    LectureServiceImpl lectureServiceImpl;

    @Operation(summary = "Get all data of major")
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('LECTURE')")
    @GetMapping("/all")
    public ResponseEntity<?> getAll() throws ResourceNotFoundException {
        List<Major> majorList = majorServiceImpl.getAll();
        List<MajorResponseDto> responseDtoList = new ArrayList<>();
        majorList.forEach(major -> {
            List<String> roleList = new ArrayList<>();
            for (Role role : major.getMajorHead().getUser().getRoles()) {
                roleList.add(role.getName().toString());
            }
            UserResponseDto userResponseDto = UserResponseDto.builder()
                .id(major.getMajorHead().getUser().getId())
                .firstName(major.getMajorHead().getUser().getFirstName())
                .lastName(major.getMajorHead().getUser().getLastName())
                .dateOfBirth(major.getMajorHead().getUser().getDateOfBirth())
                .username(major.getMajorHead().getUser().getUsername())
                .email(major.getMajorHead().getUser().getEmail())
                .phoneNumber(major.getMajorHead().getUser().getPhoneNumber())
                .roles(roleList)
                .build();
            LectureResponseDto lectureResponseDto = LectureResponseDto.builder()
                .id(major.getMajorHead().getId())
                .NIP(major.getMajorHead().getNIP())
                .user(userResponseDto)
                .build();
            List<Subject> subjectList = new ArrayList<>();
            try {
                subjectList = subjectServiceImpl.getByMajor(major.getId());
            } catch (ResourceNotFoundException e) {
                e.printStackTrace();
            }
            List<SubjectResponseDto> subjectResponseDtoList = new ArrayList<>();
            subjectList.forEach(subject -> {
                SubjectResponseDto subjectResponseDto = SubjectResponseDto.builder()
                    .id(subject.getId())
                    .name(subject.getName())
                    .lecture(null)
                    .build();
                subjectResponseDtoList.add(subjectResponseDto);
            }); 
            
            MajorResponseDto responseDto = MajorResponseDto.builder()
                .id(major.getId())
                .majorCode(major.getMajorCode())
                .name(major.getName())
                .majorHead(lectureResponseDto)
                .subjects(subjectResponseDtoList)
                .build();
            responseDtoList.add(responseDto);
        });
        return ResponseHandler.generateSuccessResponseWithMeta(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDtoList, null);
    }

    @Operation(summary = "Get major data by id")
    @PreAuthorize("hasAuthority('ADMIN')")
    @GetMapping("/get/{id}")
    public ResponseEntity<?> getById(@PathVariable("id") Long id) throws ResourceNotFoundException{
        Major major = majorServiceImpl.getById(id);
        List<String> roleList = new ArrayList<>();
        for (Role role : major.getMajorHead().getUser().getRoles()) {
            roleList.add(role.getName().toString());
        }
        UserResponseDto userResponseDto = UserResponseDto.builder()
            .id(major.getMajorHead().getUser().getId())
            .firstName(major.getMajorHead().getUser().getFirstName())
            .lastName(major.getMajorHead().getUser().getLastName())
            .dateOfBirth(major.getMajorHead().getUser().getDateOfBirth())
            .username(major.getMajorHead().getUser().getUsername())
            .email(major.getMajorHead().getUser().getEmail())
            .phoneNumber(major.getMajorHead().getUser().getPhoneNumber())
            .roles(roleList)
            .build();
        LectureResponseDto lectureResponseDto = LectureResponseDto.builder()
            .id(major.getMajorHead().getId())
            .NIP(major.getMajorHead().getNIP())
            .user(userResponseDto)
            .build();
        List<Subject> subjectList = new ArrayList<>();
        try {
            subjectList = subjectServiceImpl.getByMajor(major.getId());
        } catch (ResourceNotFoundException e) {
            e.printStackTrace();
        }
        List<SubjectResponseDto> subjectResponseDtoList = new ArrayList<>();
        subjectList.forEach(subject -> {
            SubjectResponseDto subjectResponseDto = SubjectResponseDto.builder()
                .id(subject.getId())
                .name(subject.getName())
                .lecture(null)
                .build();
            subjectResponseDtoList.add(subjectResponseDto);
        }); 
        
        MajorResponseDto responseDto = MajorResponseDto.builder()
            .id(major.getId())
            .majorCode(major.getMajorCode())
            .name(major.getName())
            .majorHead(lectureResponseDto)
            .subjects(subjectResponseDtoList)
            .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDto);
    }

    @Operation(summary = "Insert new data of major")
    @PreAuthorize("hasAuthority('ADMIN')")
    @PostMapping("/add")
    public ResponseEntity<?> add(@RequestBody AddMajorRequestDto requestDto) throws ResourceNotFoundException, ResourceAlreadyExistException{
        Major major = majorServiceImpl.add(requestDto);
        List<String> roleList = new ArrayList<>();
        for (Role role : major.getMajorHead().getUser().getRoles()) {
            roleList.add(role.getName().toString());
        }
        UserResponseDto userResponseDto = UserResponseDto.builder()
            .id(major.getMajorHead().getUser().getId())
            .firstName(major.getMajorHead().getUser().getFirstName())
            .lastName(major.getMajorHead().getUser().getLastName())
            .dateOfBirth(major.getMajorHead().getUser().getDateOfBirth())
            .username(major.getMajorHead().getUser().getUsername())
            .email(major.getMajorHead().getUser().getEmail())
            .phoneNumber(major.getMajorHead().getUser().getPhoneNumber())
            .roles(roleList)
            .build();
        LectureResponseDto lectureResponseDto = LectureResponseDto.builder()
            .id(major.getMajorHead().getId())
            .NIP(major.getMajorHead().getNIP())
            .user(userResponseDto)
            .build();
        List<Subject> subjectList = new ArrayList<>();
        try {
            subjectList = subjectServiceImpl.getByMajor(major.getId());
        } catch (ResourceNotFoundException e) {
            e.printStackTrace();
        }
        List<SubjectResponseDto> subjectResponseDtoList = new ArrayList<>();
        subjectList.forEach(subject -> {
            SubjectResponseDto subjectResponseDto = SubjectResponseDto.builder()
                .id(subject.getId())
                .name(subject.getName())
                .lecture(null)
                .build();
            subjectResponseDtoList.add(subjectResponseDto);
        }); 
        
        MajorResponseDto responseDto = MajorResponseDto.builder()
            .id(major.getId())
            .majorCode(major.getMajorCode())
            .name(major.getName())
            .majorHead(lectureResponseDto)
            .subjects(subjectResponseDtoList)
            .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.CREATED, EResponseMessage.INSERT_DATA_SUCCESS.getMessage(), responseDto);
    }

    @Operation(summary = "Edit data of a major")
    @PreAuthorize("hasAuthority('ADMIN')")
    @PatchMapping("/update")
    public ResponseEntity<?> update(@RequestBody UpdateMajorRequestDto requestDto) throws ResourceNotFoundException, ResourceAlreadyExistException {
        Major major = majorServiceImpl.update(requestDto);
        List<String> roleList = new ArrayList<>();
        for (Role role : major.getMajorHead().getUser().getRoles()) {
            roleList.add(role.getName().toString());
        }
        UserResponseDto userResponseDto = UserResponseDto.builder()
            .id(major.getMajorHead().getUser().getId())
            .firstName(major.getMajorHead().getUser().getFirstName())
            .lastName(major.getMajorHead().getUser().getLastName())
            .dateOfBirth(major.getMajorHead().getUser().getDateOfBirth())
            .username(major.getMajorHead().getUser().getUsername())
            .email(major.getMajorHead().getUser().getEmail())
            .phoneNumber(major.getMajorHead().getUser().getPhoneNumber())
            .roles(roleList)
            .build();
        LectureResponseDto lectureResponseDto = LectureResponseDto.builder()
            .id(major.getMajorHead().getId())
            .NIP(major.getMajorHead().getNIP())
            .user(userResponseDto)
            .build();
        List<Subject> subjectList = new ArrayList<>();
        try {
            subjectList = subjectServiceImpl.getByMajor(major.getId());
        } catch (ResourceNotFoundException e) {
            e.printStackTrace();
        }
        List<SubjectResponseDto> subjectResponseDtoList = new ArrayList<>
        ();
        subjectList.forEach(subject -> {
            SubjectResponseDto subjectResponseDto = SubjectResponseDto.builder()
                .id(subject.getId())
                .name(subject.getName())
                .lecture(null)
                .build();
            subjectResponseDtoList.add(subjectResponseDto);
        }); 
        
        MajorResponseDto responseDto = MajorResponseDto.builder()
            .id(major.getId())
            .majorCode(major.getMajorCode())
            .name(major.getName())
            .majorHead(lectureResponseDto)
            .subjects(subjectResponseDtoList)
            .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.EDIT_DATA_SUCCESS.getMessage(), responseDto);
    }

    @Operation(summary = "Soft delete major data")
    @PreAuthorize("hasAuthority('ADMIN')")
    @PatchMapping("/delete/{id}")
    public ResponseEntity<?> delete(@PathVariable("id") Long id) throws ResourceNotFoundException {
        majorServiceImpl.delete(id);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.DELETE_SUCCESS.getMessage(), null);
    }
}
