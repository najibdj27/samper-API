package com.unper.samper.service;

import java.io.IOException;
import java.util.UUID;

import javax.mail.MessagingException;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.unper.samper.exception.ActivityNotAllowedException;
import com.unper.samper.exception.ExpiredTokenException;
import com.unper.samper.exception.ExternalAPIException;
import com.unper.samper.exception.InvalidTokenException;
import com.unper.samper.exception.PasswordNotMatchException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.SignInFailException;
import com.unper.samper.exception.TemplateNotFoundException;
import com.unper.samper.exception.WrongOTPException;
import com.unper.samper.model.User;
import com.unper.samper.model.dto.ConfirmOTPRequestDto;
import com.unper.samper.model.dto.ConfirmOTPResponseDto;
import com.unper.samper.model.dto.JwtResponseDto;
import com.unper.samper.model.dto.ResetPasswordRequestDto;
import com.unper.samper.model.dto.SendEmailOTPRequestDto;
import com.unper.samper.model.dto.SignInRequestDto;
import com.unper.samper.model.dto.RegisterUserRequestDto;

public interface AuthenticationService {
    JwtResponseDto authenticateUser(SignInRequestDto requestDto) throws SignInFailException, ResourceNotFoundException, ActivityNotAllowedException;

    String refreshAuthToken(String refreshToken) throws ExpiredTokenException;

    void sendChangePasswordOTP(SendEmailOTPRequestDto requestDto) throws ResourceNotFoundException, MessagingException, TemplateNotFoundException;

    ConfirmOTPResponseDto confirmResetPasswordOTP(ConfirmOTPRequestDto requestDto) throws WrongOTPException, ResourceNotFoundException, ResourceAlreadyExistException;
    
    void resetPassword(UUID token, ResetPasswordRequestDto requestDto) throws PasswordNotMatchException, ResourceNotFoundException, ExpiredTokenException, InvalidTokenException;
    
    User registerUser(RegisterUserRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException, ExternalAPIException, JsonMappingException, JsonProcessingException, IOException, InterruptedException;

    User getCurrentUser() throws ResourceNotFoundException;

    Boolean checkTokenExpiration(String token);
}
