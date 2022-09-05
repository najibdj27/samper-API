package com.unper.samper.api.controller;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

import com.unper.samper.constant.EErrorCode;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.helper.ResponseHandler;

@ControllerAdvice
public class ExceptionController {
    @ExceptionHandler(ResourceNotFoundException.class)
    public ResponseEntity<?> handleResourceNotFoundException(ResourceNotFoundException e) {
        var error = EErrorCode.NOT_FOUND;
        return ResponseHandler.generateErrorResponse(HttpStatus.NOT_FOUND, e.getMessage(), error.getCode(), error.getDescription());
    }
}
