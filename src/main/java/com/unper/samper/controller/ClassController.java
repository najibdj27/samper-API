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
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.AddClassRequestDto;
import com.unper.samper.model.Class;
import com.unper.samper.model.Role;
import com.unper.samper.model.dto.ClassResponseDto;
import com.unper.samper.model.dto.LectureResponseDto;
import com.unper.samper.model.dto.UserResponseDto;
import com.unper.samper.service.impl.ClassServiceImpl;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

@SecurityRequirement(name = "bearer-key")
@Tag(name = "Class Controller")
@RestController
@RequestMapping("/class")
public class ClassController {
    @Autowired
    ClassServiceImpl classServiceImpl;

    @Operation(summary = "Get all data of classes")
    @PreAuthorize("hasAuthority('ADMIN')")
    @GetMapping("/all")
    public ResponseEntity<?> getAll() throws ResourceNotFoundException{
        List<Class> classList = classServiceImpl.getAll();
        List<ClassResponseDto> responseDtoList = new ArrayList<>();
        classList.forEach( kelas -> {
            List<String> roleList = new ArrayList<>();
            for (Role role : kelas.getLecture().getUser().getRoles()) {
                roleList.add(role.getName().toString());
            }
            UserResponseDto userResponseDto = UserResponseDto.builder()
                .id(kelas.getLecture().getUser().getId())
                .firstName(kelas.getLecture().getUser().getFirstName())
                .lastName(kelas.getLecture().getUser().getLastName())
                .dateOfBirth(kelas.getLecture().getUser().getDateOfBirth())
                .username(kelas.getLecture().getUser().getUsername())
                .email(kelas.getLecture().getUser().getEmail())
                .phoneNumber(kelas.getLecture().getUser().getPhoneNumber())
                .roles(roleList)
                .build();
            LectureResponseDto lectureResponseDto = LectureResponseDto.builder()
                .id(kelas.getLecture().getId())
                .NIP(kelas.getLecture().getNIP())
                .user(userResponseDto)
                .build();
            ClassResponseDto responseDto = ClassResponseDto.builder()
                .Id(kelas.getId())
                .lecture(lectureResponseDto)
                .name(kelas.getName())
                .build();
            responseDtoList.add(responseDto);
        });
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDtoList);
    }

    @Operation(summary = "Get a class by id")
    @PreAuthorize("hasAthority('ADMIN') or hasAuthority('LECTURE') or hasAuthority('STUDENT')")
    @GetMapping("/get/{id}")
    public ResponseEntity<?> getById(@PathVariable("id") Long id) throws ResourceNotFoundException {   
        Class kelas = classServiceImpl.getById(id);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), kelas);
    }

    @Operation(summary = "Add a new class")
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('LECTURE')")
    @PostMapping("/add")
    public ResponseEntity<?> add(@RequestBody AddClassRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException {
        classServiceImpl.addClass(requestDto);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.INSERT_DATA_SUCCESS.getMessage(), null);
    }

    @Operation(summary = "Soft delete a class")
    @PreAuthorize("hasAuthority('ADMIN')")
    @PatchMapping("/delete/{id}")
    public ResponseEntity<?> delete(@PathVariable("id") Long id) throws ResourceNotFoundException {
        classServiceImpl.delete(id);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.DELETE_SUCCESS.getMessage(), id);
    }
}
