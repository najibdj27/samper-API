package com.unper.samper.service.impl;

import java.time.Instant;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.InvalidTokenException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.RefreshToken;
import com.unper.samper.model.User;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.repository.RefreshTokenRepository;
import com.unper.samper.service.RefreshTokenService;

@Service
public class RefreshTokenServiceImpl implements RefreshTokenService {
    @Autowired
    RefreshTokenRepository refreshTokenRepository;

    @Autowired
    UserServiceImpl userServiceImpl;

    @Value("${com.unper.samper.refresh-token-expiration-ms}")
    private Long refreshTokenExpiration; 

    @Override
    public RefreshToken createRefreshToken(Long userId) throws ResourceNotFoundException {
        User user = userServiceImpl.getById(userId);
        if (refreshTokenRepository.existsByUser(user)) {
            RefreshToken existingRefreshToken = findByUser(user);
            refreshTokenRepository.delete(existingRefreshToken);
        }
        RefreshToken refreshToken = RefreshToken.builder()
            .user(user)
            .token(UUID.randomUUID().toString())
            .expiryDate(Instant.now().plusMillis(refreshTokenExpiration))
            .build();
        return refreshTokenRepository.save(refreshToken);
    }

    @Override
    public RefreshToken findByToken(String token) throws ResourceNotFoundException {
        return refreshTokenRepository.findByToken(token).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.REFRESH_TOKEN_NOT_EXIST.getMessage()));
    }

    @Override
    public RefreshToken verifyTokenExpiration(String token) throws ResourceNotFoundException, InvalidTokenException {
        RefreshToken refreshToken = findByToken(token);
        if (refreshToken.getExpiryDate().compareTo(Instant.now()) <= 0) {
            refreshTokenRepository.delete(refreshToken);
            throw new InvalidTokenException(EResponseMessage.REFRESH_TOKEN_EXPIRED.getMessage());
        }
        return refreshToken;
    }

    @Override
    public RefreshToken findByUser(User user) throws ResourceNotFoundException {
        return refreshTokenRepository.findByUser(user).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
    }
    
}
