package com.unper.samper.api.service.impl;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.password.PasswordEncoder;

import com.unper.samper.api.service.AuthService;
import com.unper.samper.config.JwtUtils;
import com.unper.samper.domain.common.UserDetailsImpl;
import com.unper.samper.domain.dao.User;
import com.unper.samper.domain.dto.SignInRequestDTO;
import com.unper.samper.domain.dto.SignInResponseDTO;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.repository.UserRepository;

public class AuthServiceImpl implements AuthService {
    @Autowired
    AuthenticationManager authenticationManager;
    
    @Autowired
    UserRepository userRepository;

    @Autowired
    PasswordEncoder encoder;

    @Autowired
    JwtUtils jwtUtils;

    @Override
    public SignInResponseDTO authenticate(SignInRequestDTO requestDTO) throws ResourceNotFoundException {
        Optional<User> user = userRepository.findByUsername(requestDTO.getUsername());
        Boolean isPasswordCorrect = encoder.matches(requestDTO.getPassword(), user.get().getPassword());
        if (Boolean.FALSE.equals(userRepository.existsByUsername(requestDTO.getUsername()))) {
            throw new ResourceNotFoundException("Username or password is wrong!");
        }
        if (Boolean.FALSE.equals(isPasswordCorrect)) {
            throw new ResourceNotFoundException("Username or password is wrong!");
        }
        Authentication authentication = authenticationManager.authenticate(new UsernamePasswordAuthenticationToken(requestDTO.getUsername(), requestDTO.getPassword()));
        SecurityContextHolder.getContext().setAuthentication(authentication);
        String jwtToken = jwtUtils.generateJwtToken(authentication);
        UserDetailsImpl userDetails = (UserDetailsImpl) authentication.getPrincipal();
        List<String> roles = userDetails.getAuthorities().stream().map(item -> item.getAuthority()).collect(Collectors.toList());
        return new SignInResponseDTO(jwtToken, userDetails.getUsername(), roles);
    }
    
}
