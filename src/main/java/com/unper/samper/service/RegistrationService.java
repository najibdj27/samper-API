package com.unper.samper.service;

import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.unper.samper.exception.ExternalAPIException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.dto.RegisterAdminRequestDto;
import com.unper.samper.model.dto.RegisterLectureRequestDto;
import com.unper.samper.model.dto.RegisterStudentRequestDto;

@Service
public interface RegistrationService {
    void registerStudent(RegisterStudentRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException, JsonMappingException, JsonProcessingException, ExternalAPIException;

    void registerLecture(RegisterLectureRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException, JsonMappingException, JsonProcessingException, ExternalAPIException;

    void registerAdmin(RegisterAdminRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException, JsonMappingException, JsonProcessingException, ExternalAPIException;
}
