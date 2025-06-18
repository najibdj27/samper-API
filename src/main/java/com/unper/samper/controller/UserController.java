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
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.StatusNotFoundException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.Lecture;
import com.unper.samper.model.Role;
import com.unper.samper.model.Student;
import com.unper.samper.model.User;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.ChangeUserStatusRequestDto;
import com.unper.samper.model.dto.ClassResponseDto;
import com.unper.samper.model.dto.LectureResponseDto;
import com.unper.samper.model.dto.StudentResponseDto;
import com.unper.samper.model.dto.UserResponseDto;
import com.unper.samper.service.impl.LectureServiceImpl;
import com.unper.samper.service.impl.StudentServiceImpl;
import com.unper.samper.service.impl.UserServiceImpl;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

@SecurityRequirement(name = "bearer-key")
@Tag(name = "User")
@RestController
@RequestMapping("/user")
public class UserController {
    @Autowired
    UserServiceImpl userServiceImpl;

    @Autowired
    StudentServiceImpl studentServiceImpl;

    @Autowired
    LectureServiceImpl lectureServiceImpl;
    
    /***
     * Get all data of user
     * @return 
     * @throws ResourceNotFoundException
     */
    @Operation(summary = "Get all data of user")
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('LECTURE') or hasAuthority('STUDENT')")
    @GetMapping("/all")
    public ResponseEntity<?> getAll() throws ResourceNotFoundException {
        List<User> userList = userServiceImpl.getAll();
        List<UserResponseDto> responseDtoList = new ArrayList<>();
        userList.forEach(user -> {
            List<String> roleList = new ArrayList<>();
            for (Role role : user.getRoles()) {
                roleList.add(role.getName().toString());
            }
            UserResponseDto responseDto = UserResponseDto.builder()
                .id(user.getId())
                .firstName(user.getFirstName())
                .lastName(user.getLastName())
                .dateOfBirth(user.getDateOfBirth())
                .username(user.getUsername())
                .email(user.getEmail())
                .phoneNumber(user.getPhoneNumber())
                .roles(roleList)
                .build();
            responseDtoList.add(responseDto);
        });

        // meta data
        Map<String, Object> metaData = new HashMap<>();
        metaData.put("_total", responseDtoList.size());

        return ResponseHandler.generateSuccessResponseWithMeta(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDtoList,  metaData);
    }

    @Operation(summary =  "Get summary data of user profile summary")
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('LECTURE') or hasAuthority('STUDENT')")
    @GetMapping("/profilesummary")
    public ResponseEntity<?> getProfileSummary(@RequestParam(value = "userId", required = true) Long userId) throws ResourceNotFoundException {
        User user = userServiceImpl.getById(userId);
        List<String> roleList = new ArrayList<>();
        Object responseDto = null;
        for (Role role : user.getRoles()) {
            roleList.add(role.getName().toString());
        }
        if (roleList.contains("LECTURE") && roleList.contains("ADMIN")) {
            
        } else if (roleList.contains("ADMIN")) {
            
        } else if (roleList.contains("LECTURE")) {
            Lecture lecture = lectureServiceImpl.getByUser(user);
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
            responseDto = LectureResponseDto.builder()
                .id(lecture.getId())
                .NIP(lecture.getNIP())
                .user(userResponseDto)
                .build();
        } else if (roleList.contains("STUDENT")) {
            Student student = studentServiceImpl.getByUser(user);
            UserResponseDto userResponseDto = UserResponseDto.builder()
                .id(user.getId())
                .firstName(user.getFirstName())
                .lastName(user.getLastName())
                .dateOfBirth(user.getDateOfBirth())
                .username(user.getUsername())
                .email(user.getEmail())
                .phoneNumber(user.getPhoneNumber())
                .roles(roleList)
                .build();
            ClassResponseDto classResponseDto = ClassResponseDto.builder()
                .id(student.getKelas().getId())
                .lecture(null)
                .name(student.getKelas().getName())
                .build(); 
            responseDto = StudentResponseDto.builder()
                .id(student.getId())
                .NIM(student.getNIM())
                .user(userResponseDto)
                .kelas(classResponseDto)
                .isLeader(student.getIsLeader())
                .build();
            } 
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDto);
    }

    @Operation(summary =  "Change user status")
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('LECTURE')")
    @PatchMapping("/change-status")
    public ResponseEntity<?> activateUser(@RequestBody ChangeUserStatusRequestDto requestDto) throws ResourceNotFoundException, StatusNotFoundException{
        userServiceImpl.changeStatus(requestDto.getUserId(), requestDto.getStatus());
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.CHANGE_USER_STATUS_SUCCESS.getMessage(), null);
    }
}
