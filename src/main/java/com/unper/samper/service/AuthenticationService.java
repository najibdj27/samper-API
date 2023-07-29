package com.unper.samper.service;

import java.util.UUID;

import javax.mail.MessagingException;

import org.springframework.http.ResponseEntity;

import com.unper.samper.exception.ExpiredTokenException;
import com.unper.samper.exception.PasswordNotMatchException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.SignInFailException;
import com.unper.samper.exception.WrongOTPException;
import com.unper.samper.model.User;
import com.unper.samper.model.dto.ConfirmOTPRequestDto;
import com.unper.samper.model.dto.ForgetPasswordRequestDto;
import com.unper.samper.model.dto.ResetPasswordRequestDto;
import com.unper.samper.model.dto.SignInRequestDto;
import com.unper.samper.model.dto.SignUpRequestDto;

public interface AuthenticationService {
    ResponseEntity<?> authenticateUser(SignInRequestDto requestDto) throws SignInFailException;

    ResponseEntity<?> changePassword(ForgetPasswordRequestDto requestDto) throws ResourceNotFoundException, MessagingException;
    
    ResponseEntity<?> confirmOTP(ConfirmOTPRequestDto requestDto) throws WrongOTPException, ResourceNotFoundException;
    
    ResponseEntity<?> resetPassword(UUID token, ResetPasswordRequestDto requestDto) throws PasswordNotMatchException, ResourceNotFoundException, ExpiredTokenException;
    
    User registerUser(SignUpRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException;

    User getCurrentUser();
}
