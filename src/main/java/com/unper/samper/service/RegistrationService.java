package com.unper.samper.service;

import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.dto.RegisterAdminRequestDto;
import com.unper.samper.model.dto.RegisterLectureRequestDto;
import com.unper.samper.model.dto.RegisterStudentRequestDto;

@Service
public interface RegistrationService {
    void registerStudent(RegisterStudentRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException;

    void registerLecture(RegisterLectureRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException;

    void registerAdmin(RegisterAdminRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException;
}
