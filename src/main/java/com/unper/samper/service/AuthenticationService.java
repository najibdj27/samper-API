package com.unper.samper.service;

import java.util.UUID;

import javax.mail.MessagingException;

import com.unper.samper.exception.ExpiredTokenException;
import com.unper.samper.exception.InvalidTokenException;
import com.unper.samper.exception.PasswordNotMatchException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.SignInFailException;
import com.unper.samper.exception.WrongOTPException;
import com.unper.samper.model.User;
import com.unper.samper.model.dto.ConfirmOTPRequestDto;
import com.unper.samper.model.dto.ConfirmOTPResponseDto;
import com.unper.samper.model.dto.ForgetPasswordRequestDto;
import com.unper.samper.model.dto.JwtResponseDto;
import com.unper.samper.model.dto.RefreshTokenRequestDto;
import com.unper.samper.model.dto.RefreshTokenResponseDto;
import com.unper.samper.model.dto.ResetPasswordRequestDto;
import com.unper.samper.model.dto.SignInRequestDto;
import com.unper.samper.model.dto.SignUpRequestDto;

public interface AuthenticationService {
    JwtResponseDto authenticateUser(SignInRequestDto requestDto) throws SignInFailException, ResourceNotFoundException;

    RefreshTokenResponseDto refreshAuthToken(RefreshTokenRequestDto requestDto) throws ResourceNotFoundException, InvalidTokenException;

    void changePassword(ForgetPasswordRequestDto requestDto) throws ResourceNotFoundException, MessagingException;
    
    ConfirmOTPResponseDto confirmOTP(ConfirmOTPRequestDto requestDto) throws WrongOTPException, ResourceNotFoundException;
    
    void resetPassword(UUID token, ResetPasswordRequestDto requestDto) throws PasswordNotMatchException, ResourceNotFoundException, ExpiredTokenException;
    
    User registerUser(SignUpRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException;

    User getCurrentUser() throws ResourceNotFoundException;

    Boolean checkTokenExpiration(String token);

    void deleteExpiredToken() throws ResourceNotFoundException;
}
