package com.unper.samper.api.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.api.service.impl.AuthServiceImpl;
import com.unper.samper.domain.dto.SignInRequestDTO;
import com.unper.samper.domain.dto.SignInResponseDTO;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.helper.ResponseHandler;

@RestController
@RequestMapping("/api/v1/auth")
public class AuthController {
    @Autowired
    AuthServiceImpl authService;

    @GetMapping("/sign_in")
    public ResponseEntity<?> authentication(@RequestBody SignInRequestDTO requestDTO) throws ResourceNotFoundException{
        SignInResponseDTO response = authService.authenticate(requestDTO);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, "Successfully signed in", response);
    }

}
