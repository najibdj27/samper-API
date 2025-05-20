package com.unper.samper.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.MissingFormatArgumentException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.unper.samper.exception.ExternalAPIException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.User;
import com.unper.samper.model.Class;
import com.unper.samper.model.constant.ERole;
import com.unper.samper.model.dto.AddAdminRequestDto;
import com.unper.samper.model.dto.AddLectureRequestDto;
import com.unper.samper.model.dto.AddStudentRequestDto;
import com.unper.samper.model.dto.RegisterAdminRequestDto;
import com.unper.samper.model.dto.RegisterLectureRequestDto;
import com.unper.samper.model.dto.RegisterStudentRequestDto;
import com.unper.samper.model.dto.SignUpRequestDto;
import com.unper.samper.repository.RoleRepository;
import com.unper.samper.service.RegistrationService;

@Service
public class RegistrationServiceImpl implements RegistrationService {

    @Autowired
    AuthenticationServiceImpl authenticationServiceImpl;

    @Autowired
    RoleRepository roleRepository;

    @Autowired
    StudentServiceImpl studentServiceImpl;

    @Autowired
    LectureServiceImpl lectureServiceImpl;

    @Autowired
    AdminServiceImpl adminServiceImpl;

    @Autowired
    ClassServiceImpl classServiceImpl;

    @Override
    @Transactional(rollbackFor = {ResourceAlreadyExistException.class, ResourceNotFoundException.class})
    public void registerStudent(RegisterStudentRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException, JsonMappingException, JsonProcessingException, ExternalAPIException {
        if (requestDto.getFaceData().isEmpty()) {
            throw new MissingFormatArgumentException("Face data is null");
        }
        
        List<ERole> eRoleList = new ArrayList<>();
        eRoleList.add(ERole.STUDENT);
        SignUpRequestDto signUpRequestDto = SignUpRequestDto.builder()
            .firstName(requestDto.getFirstName())
            .lastName(requestDto.getLastName())
            .dateOfBirth(requestDto.getDateOfBirth())
            .username(requestDto.getUsername())
            .email(requestDto.getEmail())
            .phoneNumber(requestDto.getPhoneNumber())
            .password(requestDto.getPassword())
            .roles(eRoleList)
            .build();
        User newUser = authenticationServiceImpl.registerUser(signUpRequestDto);

        Class kelas = classServiceImpl.getById(requestDto.getClassId());  
        AddStudentRequestDto addStudentRequestDto = AddStudentRequestDto.builder()
            .user(newUser)
            .kelas(kelas)
            .NIM(requestDto.getNIM())
            .isLeader(requestDto.getIsLeader())
            .build();
        studentServiceImpl.add(addStudentRequestDto);
    }

    @Override
    @Transactional(rollbackFor = {ResourceAlreadyExistException.class, ResourceNotFoundException.class})
    public void registerLecture(RegisterLectureRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException, JsonMappingException, JsonProcessingException, ExternalAPIException {
        if (requestDto.getFaceData().isEmpty()) {
            throw new MissingFormatArgumentException("Face data is null");
        }

        List<ERole> eRoleList = new ArrayList<>();
        eRoleList.add(ERole.LECTURE);
        SignUpRequestDto signUpRequestDto = SignUpRequestDto.builder()
            .firstName(requestDto.getFirstName())
            .lastName(requestDto.getLastName())
            .dateOfBirth(requestDto.getDateOfBirth())
            .username(requestDto.getUsername())
            .email(requestDto.getEmail())
            .phoneNumber(requestDto.getPhoneNumber())
            .password(requestDto.getPassword())
            .faceData(requestDto.getFaceData())
            .roles(eRoleList)
            .build();
        User newUser = authenticationServiceImpl.registerUser(signUpRequestDto);

        AddLectureRequestDto addLectureRequestDto = AddLectureRequestDto.builder()
            .NIP(requestDto.getNIP())
            .user(newUser)
            .build();
        lectureServiceImpl.add(addLectureRequestDto);
    }

    @Override
    @Transactional(rollbackFor = {ResourceAlreadyExistException.class, ResourceNotFoundException.class})
    public void registerAdmin(RegisterAdminRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException, JsonMappingException, JsonProcessingException, ExternalAPIException {
        List<ERole> eRoleList = new ArrayList<>();
        eRoleList.add(ERole.ADMIN);
        SignUpRequestDto signUpRequestDto = SignUpRequestDto.builder()
            .firstName(requestDto.getFirstName())
            .lastName(requestDto.getLastName())
            .dateOfBirth(requestDto.getDateOfBirth())
            .username(requestDto.getUsername())
            .email(requestDto.getEmail())
            .phoneNumber(requestDto.getPhoneNumber())
            .password(requestDto.getPassword())
            .faceData(null)
            .roles(eRoleList)
            .build();
        User newUser = authenticationServiceImpl.registerUser(signUpRequestDto);

        AddAdminRequestDto addAdminRequestDto = AddAdminRequestDto.builder()
            .user(newUser)
            .NIP(requestDto.getNIP())
            .previllagesId(requestDto.getPrevillages())
            .build();
        adminServiceImpl.add(addAdminRequestDto);
    }
    
}
