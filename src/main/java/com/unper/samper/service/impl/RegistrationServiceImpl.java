package com.unper.samper.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.User;
import com.unper.samper.model.Class;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.AddLectureRequestDto;
import com.unper.samper.model.dto.AddStudentRequestDto;
import com.unper.samper.model.dto.RegisterLectureRequestDto;
import com.unper.samper.model.dto.RegisterStudentRequestDto;
import com.unper.samper.model.dto.SignUpRequestDto;
import com.unper.samper.service.RegistrationService;

@Service
public class RegistrationServiceImpl implements RegistrationService {

    @Autowired
    AuthenticationServiceImpl authenticationServiceImpl;

    @Autowired
    StudentServiceImpl studentServiceImpl;

    @Autowired
    LectureServiceImpl lectureServiceImpl;

    @Autowired
    ClassServiceImpl classServiceImpl;

    @Override
    @Transactional(rollbackFor = {ResourceAlreadyExistException.class, ResourceNotFoundException.class})
    public ResponseEntity<?> registerStudent(RegisterStudentRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException {
        SignUpRequestDto signUpRequestDto = SignUpRequestDto.builder()
            .firstName(requestDto.getFirstName())
            .lastName(requestDto.getLastName())
            .dateOfBirth(requestDto.getDateOfBirth())
            .username(requestDto.getUsername())
            .email(requestDto.getEmail())
            .phoneNumber(requestDto.getPhoneNumber())
            .password(requestDto.getPassword())
            .build();
        User newUser = authenticationServiceImpl.registerUser(signUpRequestDto);

        Class kelas = classServiceImpl.getById(requestDto.getClassId());  
        AddStudentRequestDto addStudentRequestDto = AddStudentRequestDto.builder()
            .user(newUser)
            .kelas(kelas)
            .NIM(requestDto.getNIM())
            .build();
        studentServiceImpl.add(addStudentRequestDto);

        return ResponseHandler.generateSuccessResponse(HttpStatus.CREATED, EResponseMessage.REGISTRATION_SUCCESS.getMessage(), null);
    }

    @Override
    @Transactional(rollbackFor = {ResourceAlreadyExistException.class, ResourceNotFoundException.class})
    public ResponseEntity<?> registerLecture(RegisterLectureRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException {
        SignUpRequestDto signUpRequestDto = SignUpRequestDto.builder()
            .firstName(requestDto.getFirstName())
            .lastName(requestDto.getLastName())
            .dateOfBirth(requestDto.getDateOfBirth())
            .username(requestDto.getUsername())
            .email(requestDto.getEmail())
            .phoneNumber(requestDto.getPhoneNumber())
            .password(requestDto.getPassword())
            .build();
        User newUser = authenticationServiceImpl.registerUser(signUpRequestDto);

        AddLectureRequestDto addLectureRequestDto = AddLectureRequestDto.builder()
            .NIP(requestDto.getNIP())
            .user(newUser)
            .build();
        lectureServiceImpl.add(addLectureRequestDto);

        return ResponseHandler.generateSuccessResponse(HttpStatus.CREATED, EResponseMessage.REGISTRATION_SUCCESS.getMessage(), null);
    }
    
}
