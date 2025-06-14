package com.unper.samper.controller;

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.MissingFormatArgumentException;

import org.apache.tomcat.util.http.fileupload.impl.SizeLimitExceededException;
import org.cloudinary.json.JSONException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.multipart.support.MissingServletRequestPartException;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import com.unper.samper.exception.ActivityNotAllowedException;
import com.unper.samper.exception.DifferentClassException;
import com.unper.samper.exception.ExpiredTokenException;
import com.unper.samper.exception.ExternalAPIException;
import com.unper.samper.exception.FaceNotMatchedException;
import com.unper.samper.exception.GeolocationException;
import com.unper.samper.exception.InvalidTokenException;
import com.unper.samper.exception.NoAccessException;
import com.unper.samper.exception.OnScheduleException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.ScheduleNotActiveException;
import com.unper.samper.exception.ScheduleUnavailableException;
import com.unper.samper.exception.SignInFailException;
import com.unper.samper.exception.WrongOTPException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.constant.EErrorCode;

import io.jsonwebtoken.ExpiredJwtException;


@ControllerAdvice
public class ErrorExceptionHandlingController extends ResponseEntityExceptionHandler {
    
    @Override
    protected ResponseEntity<Object> handleMissingServletRequestPart(MissingServletRequestPartException ex, HttpHeaders headers, HttpStatus status, WebRequest request) {
        var error = EErrorCode.INVALIND_REQUEST;
        return ResponseHandler.generateErrorResponse(HttpStatus.NOT_FOUND, ex.getMessage(), error.getCode(), error.getDescription());
    }

    @Override
    protected ResponseEntity<Object> handleHttpMessageNotReadable(HttpMessageNotReadableException ex, HttpHeaders headers, HttpStatus status, WebRequest request) {
        var error = EErrorCode.MALFORMED_JSON;
        return ResponseHandler.generateErrorResponse(HttpStatus.NOT_FOUND, ex.getMessage(), error.getCode(), error.getDescription());
    }

    @Override
    protected ResponseEntity<Object> handleMethodArgumentNotValid(MethodArgumentNotValidException ex, HttpHeaders headers, HttpStatus status, WebRequest request) {
        var error = EErrorCode.INVALIND_REQUEST;
        List<String> messages = new ArrayList<>();
        ex.getFieldErrors().forEach(err -> {
            messages.add(err.getField() + " " + err.getDefaultMessage());
        });
        return ResponseHandler.generateValidationErrorResponse(HttpStatus.BAD_REQUEST, messages, error.getCode(), error.getDescription());
    }

    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<?> handleIllegalArgumentException(IllegalArgumentException e) {
        var error = EErrorCode.NOT_FOUND;
        return ResponseHandler.generateErrorResponse(HttpStatus.INTERNAL_SERVER_ERROR, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(SizeLimitExceededException.class)
    public ResponseEntity<?> handleSizeLimitExceededException(SizeLimitExceededException e) {
        var error = EErrorCode.FILE_TOO_LARGE;
        return ResponseHandler.generateErrorResponse(HttpStatus.NOT_ACCEPTABLE, e.getMessage(), error.getCode(), error.getDescription());
    }
    @ExceptionHandler(ResourceNotFoundException.class)
    public ResponseEntity<?> handleResourceNotFoundException(ResourceNotFoundException e) {
        var error = EErrorCode.NOT_FOUND;
        return ResponseHandler.generateErrorResponse(HttpStatus.NOT_FOUND, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(FileNotFoundException.class)
    public ResponseEntity<?> handleFileNotFoundException(FileNotFoundException e) {
        var error = EErrorCode.FILE_NOT_EXIST;
        return ResponseHandler.generateErrorResponse(HttpStatus.NOT_FOUND, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(ResourceAlreadyExistException.class)
    public ResponseEntity<?> handleResourceAlreadyExistException(ResourceAlreadyExistException e) {
        var error = EErrorCode.RESOURCE_EXIST;
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(SignInFailException.class)
    public ResponseEntity<?> handleSignInFailException(SignInFailException e) {
        var error = EErrorCode.INVALID_CRED;
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(WrongOTPException.class)
    public ResponseEntity<?> handleWrongOTPException(WrongOTPException e) {
        var error = EErrorCode.WRONG_OTP;
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }
    
    @ExceptionHandler(ExpiredTokenException.class)
    public ResponseEntity<?> handleExpiredTokenException(ExpiredTokenException e) {
        var error = EErrorCode.TOKEN_INVALID;
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }
    
    @ExceptionHandler(ExpiredJwtException.class)
    public ResponseEntity<?> handleExpiredJwtException(ExpiredJwtException e) {
        var error = EErrorCode.TOKEN_INVALID;
        return ResponseHandler.generateErrorResponse(HttpStatus.UNAUTHORIZED, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(ScheduleNotActiveException.class)
    public ResponseEntity<?> handleScheduleNotActiveException(ScheduleNotActiveException e) {
        var error = EErrorCode.SCHEDULE_NOT_ACTIVE;
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(ScheduleUnavailableException.class)
    public ResponseEntity<?> handleScheduleUnavailableException(ScheduleUnavailableException e) {
        var error = EErrorCode.SCHEDULE_UNAVAILABLE;
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(DifferentClassException.class)
    public ResponseEntity<?> handleDifferentClassException(DifferentClassException e) {
        var error = EErrorCode.DIFFERENT_CLASS;
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(NoAccessException.class)
    public ResponseEntity<?> handleNoAccessException(NoAccessException e) {
        var error = EErrorCode.UNAUTHORIZED;
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(OnScheduleException.class)
    public ResponseEntity<?> handleOnScheduleException(OnScheduleException e) {
        var error = EErrorCode.ON_SCHEDULE;
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(ActivityNotAllowedException.class)
    public ResponseEntity<?> handleActivityNotAllowedException(ActivityNotAllowedException e) {
        var error = EErrorCode.ACTIVITY_NOT_ALLOWED;
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(JSONException.class)
    public ResponseEntity<?> handleJSONException(JSONException e) {
        var error = EErrorCode.JSON_STRING;
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }
    
    @ExceptionHandler(InvalidTokenException.class)
    public ResponseEntity<?> handleInvalidTokenException(InvalidTokenException e) {
        var error = EErrorCode.TOKEN_INVALID;
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(ExternalAPIException.class)
    public ResponseEntity<?> handleExternalApiException(ExternalAPIException e) {
        var error = EErrorCode.EXT_API_ERR;
        return ResponseHandler.generateErrorResponse(HttpStatus.GATEWAY_TIMEOUT, e.getMessage(), error.getCode(), error.getDescription());
    }
    
    @ExceptionHandler(FaceNotMatchedException.class)
    public ResponseEntity<?> handleFaceNotMatchedException(FaceNotMatchedException e) {
        var error = EErrorCode.FACE_NOT_MATCH;
        return ResponseHandler.generateErrorResponse(HttpStatus.NOT_ACCEPTABLE, e.getMessage(), error.getCode(), error.getDescription());
    }
   
    @ExceptionHandler(MissingFormatArgumentException.class)
    public ResponseEntity<?> handleMissingFormatArgumentException(MissingFormatArgumentException e) {
        var error = EErrorCode.MISSING_PARAM;
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }
   
    @ExceptionHandler(GeolocationException.class)
    public ResponseEntity<?> handleGeolocationException(GeolocationException e) {
        var error = EErrorCode.LOCATION_NOT_IN_RANGE;
        return ResponseHandler.generateErrorResponse(HttpStatus.NOT_ACCEPTABLE, e.getMessage(), error.getCode(), error.getDescription());
    }

}
