package com.unper.samper.service;


import com.unper.samper.domain.dto.SignInRequestDTO;
import com.unper.samper.domain.dto.SignInResponseDTO;
import com.unper.samper.exception.ResourceNotFoundException;

public interface AuthService {
    SignInResponseDTO authenticate(SignInRequestDTO requestDTO) throws ResourceNotFoundException;
}
