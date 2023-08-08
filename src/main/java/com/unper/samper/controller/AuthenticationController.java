package com.unper.samper.controller;

import java.util.UUID;

import javax.mail.MessagingException;
import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.exception.ExpiredTokenException;
import com.unper.samper.exception.PasswordNotMatchException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.SignInFailException;
import com.unper.samper.exception.WrongOTPException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.CheckExpiredJwtTokenResponseDto;
import com.unper.samper.model.dto.ConfirmOTPRequestDto;
import com.unper.samper.model.dto.ConfirmOTPResponseDto;
import com.unper.samper.model.dto.ForgetPasswordRequestDto;
import com.unper.samper.model.dto.JwtResponseDto;
import com.unper.samper.model.dto.ResetPasswordRequestDto;
import com.unper.samper.model.dto.SignInRequestDto;
import com.unper.samper.service.impl.AuthenticationServiceImpl;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

@Tag(name = "1. Auth Controller")
@CrossOrigin(origins = "*", maxAge = 3600)
@RestController
@RequestMapping("/auth")
public class AuthenticationController {
    @Autowired
    AuthenticationServiceImpl authenticationServiceImpl;

    /***
     * Sign in and get the token for access
     * @param signInRequest
     * @return
     * @throws SignInFailException
     */
    @Operation(summary = "Sign in and get the token for access")
    @PostMapping("/signin")
    public ResponseEntity<?> authenticate(@Valid @RequestBody SignInRequestDto requestDto) throws SignInFailException {
        JwtResponseDto responseDto = authenticationServiceImpl.authenticateUser(requestDto);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, "Successfully login!", responseDto);
    }

    /***
     * Forget password
     * @param forgetPasswordRequestDTO
     * @return
     * @throws ResourceNotFoundException
     * @throws MessagingException
     */
    @Operation(summary = "Get OTP to reset password")
    @PostMapping("/forgetpassword")
    public ResponseEntity<?> forgetPassword(@Valid @RequestBody ForgetPasswordRequestDto requestDto) throws ResourceNotFoundException, MessagingException {
        authenticationServiceImpl.changePassword(requestDto);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, "OTP has been sent to your email!", null);
    }

    /***
     * Confirm OTP
     * @param confirmOTPRequestDTO
     * @return
     * @throws WrongOTPException
     * @throws ResourceNotFoundException
     */
    @Operation(summary = "Confirm OTP to get the token to reset the password")
    @PostMapping("/confirmotp")
    public ResponseEntity<?> confirmOTP(@Valid @RequestBody ConfirmOTPRequestDto requestDto) throws WrongOTPException, ResourceNotFoundException {
        ConfirmOTPResponseDto responseDto = authenticationServiceImpl.confirmOTP(requestDto);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, "OTP has been confirmed!", responseDto);
    }

    /***
     * Reset password
     * @param token
     * @param resetPasswordRequestDTO
     * @return
     * @throws PasswordNotMatchException
     * @throws ResourceNotFoundException
     * @throws ExpiredTokenException
     */
    @Operation(summary = "Reset the password")
    @PatchMapping("/reset_password")
    public ResponseEntity<?> resetPassword(@RequestParam("token") UUID token, @Valid @RequestBody ResetPasswordRequestDto requestDto) throws PasswordNotMatchException, ResourceNotFoundException, ExpiredTokenException {
        authenticationServiceImpl.resetPassword(token, requestDto);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, "Password has been reset successfully!", null);
    }

    @Operation(summary = "Check your token is expired")
    @GetMapping("/checktoken")
    public ResponseEntity<?> checkExpiredJwtToken(@RequestParam("token") String token) {
        Boolean isActive = authenticationServiceImpl.checkTokenExpiration(token);
        CheckExpiredJwtTokenResponseDto responseDto = CheckExpiredJwtTokenResponseDto.builder().isActive(isActive).build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDto);
    }
}
