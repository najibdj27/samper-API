package com.unper.samper.controller;

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;

import org.apache.tomcat.util.http.fileupload.impl.SizeLimitExceededException;
import org.cloudinary.json.JSONException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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


@ControllerAdvice
public class ErrorExceptionHandlingController extends ResponseEntityExceptionHandler {
    private static final Logger logger = LoggerFactory.getLogger(ErrorExceptionHandlingController.class);

    private static final String loggerLine = "---------------------------------------";

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
        logger.error(loggerLine);
        logger.error(error.getDescription());
        logger.error(loggerLine);
        return ResponseHandler.generateValidationErrorResponse(HttpStatus.BAD_REQUEST, messages, error.getCode(), error.getDescription());
    }

    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<?> handleIllegalArgumentException(IllegalArgumentException e) {
        var error = EErrorCode.NOT_FOUND;
        logger.error(loggerLine);
        logger.error(error.getDescription());
        logger.error(loggerLine);
        return ResponseHandler.generateErrorResponse(HttpStatus.NOT_FOUND, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(SizeLimitExceededException.class)
    public ResponseEntity<?> handleSizeLimitExceededException(SizeLimitExceededException e) {
        var error = EErrorCode.FILE_TOO_LARGE;
        logger.error(loggerLine);
        logger.error(error.getDescription());
        logger.error(loggerLine);
        return ResponseHandler.generateErrorResponse(HttpStatus.NOT_ACCEPTABLE, e.getMessage(), error.getCode(), error.getDescription());
    }
    @ExceptionHandler(ResourceNotFoundException.class)
    public ResponseEntity<?> handleResourceNotFoundException(ResourceNotFoundException e) {
        var error = EErrorCode.NOT_FOUND;
        logger.error(loggerLine);
        logger.error(error.getDescription());
        logger.error(loggerLine);
        return ResponseHandler.generateErrorResponse(HttpStatus.NOT_FOUND, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(FileNotFoundException.class)
    public ResponseEntity<?> handleFileNotFoundException(FileNotFoundException e) {
        var error = EErrorCode.FILE_NOT_EXIST;
        logger.error(loggerLine);
        logger.error(error.getDescription());
        logger.error(loggerLine);
        return ResponseHandler.generateErrorResponse(HttpStatus.NOT_FOUND, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(ResourceAlreadyExistException.class)
    public ResponseEntity<?> handleResourceAlreadyExistException(ResourceAlreadyExistException e) {
        var error = EErrorCode.RESOURCE_EXIST;
        logger.error(loggerLine);
        logger.error(error.getDescription());
        logger.error(loggerLine);
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(SignInFailException.class)
    public ResponseEntity<?> handleSignInFailException(SignInFailException e) {
        var error = EErrorCode.INVALID_CRED;
        logger.error(loggerLine);
        logger.error(error.getDescription());
        logger.error(loggerLine);
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(WrongOTPException.class)
    public ResponseEntity<?> handleWrongOTPException(WrongOTPException e) {
        var error = EErrorCode.WRONG_OTP;
        logger.error(loggerLine);
        logger.error(error.getDescription());
        logger.error(loggerLine);
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }
    
    @ExceptionHandler(ExpiredTokenException.class)
    public ResponseEntity<?> handleExpiredTokenException(ExpiredTokenException e) {
        var error = EErrorCode.TOKEN_INVALID;
        logger.error(loggerLine);
        logger.error(error.getDescription());
        logger.error(loggerLine);
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(ScheduleNotActiveException.class)
    public ResponseEntity<?> handleScheduleNotActiveException(ScheduleNotActiveException e) {
        var error = EErrorCode.SCHEDULE_NOT_ACTIVE;
        logger.error(loggerLine);
        logger.error(error.getDescription());
        logger.error(loggerLine);
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(ScheduleUnavailableException.class)
    public ResponseEntity<?> handleScheduleUnavailableException(ScheduleUnavailableException e) {
        var error = EErrorCode.SCHEDULE_UNAVAILABLE;
        logger.error(loggerLine);
        logger.error(error.getDescription());
        logger.error(loggerLine);
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(DifferentClassException.class)
    public ResponseEntity<?> handleDifferentClassException(DifferentClassException e) {
        var error = EErrorCode.DIFFERENT_CLASS;
        logger.error(loggerLine);
        logger.error(error.getDescription());
        logger.error(loggerLine);
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(NoAccessException.class)
    public ResponseEntity<?> handleNoAccessException(NoAccessException e) {
        var error = EErrorCode.UNAUTHORIZED;
        logger.error(loggerLine);
        logger.error(error.getDescription());
        logger.error(loggerLine);
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(OnScheduleException.class)
    public ResponseEntity<?> handleOnScheduleException(OnScheduleException e) {
        var error = EErrorCode.ON_SCHEDULE;
        logger.error(loggerLine);
        logger.error(error.getDescription());
        logger.error(loggerLine);
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(ActivityNotAllowedException.class)
    public ResponseEntity<?> handleActivityNotAllowedException(ActivityNotAllowedException e) {
        var error = EErrorCode.ACTIVITY_NOT_ALLOWED;
        logger.error(loggerLine);
        logger.error(error.getDescription());
        logger.error(loggerLine);
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }

    @ExceptionHandler(JSONException.class)
    public ResponseEntity<?> handleJSONException(JSONException e) {
        var error = EErrorCode.JSON_STRING;
        logger.error(loggerLine);
        logger.error(error.getDescription());
        logger.error(loggerLine);
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }
    
    @ExceptionHandler(InvalidTokenException.class)
    public ResponseEntity<?> handleInvalidTokenException(InvalidTokenException e) {
        var error = EErrorCode.TOKEN_INVALID;
        logger.error(loggerLine);
        logger.error(error.getDescription());
        logger.error(loggerLine);
        return ResponseHandler.generateErrorResponse(HttpStatus.BAD_REQUEST, e.getMessage(), error.getCode(), error.getDescription());
    }

}
