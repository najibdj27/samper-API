package com.unper.samper.service;

import java.io.IOException;
import java.util.Map;
import java.util.UUID;

import javax.mail.MessagingException;

import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.unper.samper.exception.ExternalAPIException;
import com.unper.samper.exception.InvalidTokenException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.TemplateNotFoundException;
import com.unper.samper.exception.WrongOTPException;
import com.unper.samper.model.dto.ConfirmOTPRequestDto;
import com.unper.samper.model.dto.ConfirmOTPResponseDto;
import com.unper.samper.model.dto.RegisterAdminRequestDto;
import com.unper.samper.model.dto.RegisterLectureRequestDto;
import com.unper.samper.model.dto.RegisterStudentRequestDto;
import com.unper.samper.model.dto.RegistrationEligibilityRequestDto;
import com.unper.samper.model.dto.SendEmailOTPRequestDto;

@Service
public interface RegistrationService {
    void registerStudent(UUID requestToken, RegisterStudentRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException, JsonMappingException, JsonProcessingException, ExternalAPIException, InvalidTokenException, IOException, InterruptedException;

    void registerLecture(UUID requestToken, RegisterLectureRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException, JsonMappingException, JsonProcessingException, ExternalAPIException, InvalidTokenException, IOException, InterruptedException;

    void registerAdmin(RegisterAdminRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException, JsonMappingException, JsonProcessingException, ExternalAPIException, IOException, InterruptedException;

    void sendRegistrationOTP(SendEmailOTPRequestDto requestDto) throws MessagingException, ResourceNotFoundException, ResourceAlreadyExistException, TemplateNotFoundException;

    Map<String, Map<String, String>> registrationEligibilityCheck(RegistrationEligibilityRequestDto requestDto);

    ConfirmOTPResponseDto confirmRegistrationOTP(ConfirmOTPRequestDto requestDto) throws WrongOTPException, ResourceNotFoundException, ResourceAlreadyExistException;
}
