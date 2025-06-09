package com.unper.samper.controller;

import java.io.IOException;
import java.util.Map;
import java.util.UUID;

import javax.mail.MessagingException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.exception.ExternalAPIException;
import com.unper.samper.exception.InvalidTokenException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.TemplateNotFoundException;
import com.unper.samper.exception.WrongOTPException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.constant.EStatus;
import com.unper.samper.model.dto.ConfirmOTPRequestDto;
import com.unper.samper.model.dto.ConfirmOTPResponseDto;
import com.unper.samper.model.dto.RegisterAdminRequestDto;
import com.unper.samper.model.dto.RegisterLectureRequestDto;
import com.unper.samper.model.dto.RegisterStudentRequestDto;
import com.unper.samper.model.dto.RegistrationEligibilityRequestDto;
import com.unper.samper.model.dto.RegistrationEligibilityResponseDto;
import com.unper.samper.model.dto.SendEmailOTPRequestDto;
import com.unper.samper.service.RegistrationService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

@Tag(name = "Registration")
@RestController
@RequestMapping("/registration")
public class RegistrationController {
    @Autowired
    RegistrationService registrationService;
    
    @Operation(summary = "Register new student")
    @PostMapping("/registerstudent")
    public ResponseEntity<?> registerStudent(@RequestParam("token") UUID requestToken, @RequestBody RegisterStudentRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException, ExternalAPIException, InvalidTokenException, IOException, InterruptedException{
        registrationService.registerStudent(requestToken, requestDto);
        return ResponseHandler.generateSuccessResponse(HttpStatus.CREATED, EResponseMessage.REGISTRATION_SUCCESS.getMessage(), null);
    }
    
    @Operation(summary = "Register new lecture")
    @PostMapping("/registerlecture")
    public ResponseEntity<?> registerLecture(@RequestParam("token") UUID requestToken, @RequestBody RegisterLectureRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException, ExternalAPIException, InvalidTokenException, IOException, InterruptedException{
        registrationService.registerLecture(requestToken, requestDto);
        return ResponseHandler.generateSuccessResponse(HttpStatus.CREATED, EResponseMessage.REGISTRATION_SUCCESS.getMessage(), null);
    }

    @Operation(summary = "Register new admin")
    @PostMapping("/registeradmin")
    public ResponseEntity<?> registerAdmin(@RequestBody RegisterAdminRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException, ExternalAPIException, IOException, InterruptedException{
        registrationService.registerAdmin(requestDto);
        return ResponseHandler.generateSuccessResponse(HttpStatus.CREATED, EResponseMessage.REGISTRATION_SUCCESS.getMessage(), null);
    }

    @Operation(summary = "Send OTP for validate email address of new registered user")
    @PostMapping("/send-otp")
    public ResponseEntity<?> sendRegistrationOTP(@RequestBody SendEmailOTPRequestDto requestDto) throws MessagingException, ResourceNotFoundException, ResourceAlreadyExistException, TemplateNotFoundException{
        registrationService.sendRegistrationOTP(requestDto);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.SUCCESS_SEND_EMAIL_OTP.getMessage(), null);
    }
    
    @Operation(summary = "Validate OTP for validation of email address of new registered user")
    @PostMapping("/validate-otp")
    public ResponseEntity<?> confirmRegistrationOTP(@RequestBody ConfirmOTPRequestDto requestDto) throws WrongOTPException, ResourceNotFoundException, ResourceAlreadyExistException{
        ConfirmOTPResponseDto responseDto = registrationService.confirmRegistrationOTP(requestDto);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.SUCCESS_VALIDATE_OTP.getMessage(), responseDto);
    }
    
    
    @Operation(summary = "Validate OTP for validation of email address of new registered user")
    @PostMapping("/eligibility-check")
    public ResponseEntity<?> registrationEligibilityCheck(@RequestBody RegistrationEligibilityRequestDto requestDto){
        Map<String, Map<String, String>> eligbilityMap = registrationService.registrationEligibilityCheck(requestDto);
        char eligibilityStatus =  EStatus.ELIGIBLE.getCode();    
        
        if (eligbilityMap.size() > 0) {
            eligibilityStatus =  EStatus.NOT_ELIGIBLE.getCode();
        }

        RegistrationEligibilityResponseDto responseDto = RegistrationEligibilityResponseDto.builder()
            .eligibilityStatus(eligibilityStatus)
            .field(eligbilityMap)
            .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, "Success check registration eligibility!", responseDto);
    }
}
