package com.unper.samper.api.service;

import org.springframework.http.ResponseEntity;

import com.unper.samper.domain.dto.SignInRequestDTO;
import com.unper.samper.exception.ResourceNotFoundException;

public interface AuthService {
    ResponseEntity<?> authenticateUser(SignInRequestDTO requestDTO) throws ResourceNotFoundException;
}
