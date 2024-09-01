package com.unper.samper.service;

import com.unper.samper.exception.InvalidTokenException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.RefreshToken;
import com.unper.samper.model.User;

public interface RefreshTokenService {
    RefreshToken createRefreshToken(Long userId) throws ResourceNotFoundException;

    RefreshToken findByUser(User user) throws ResourceNotFoundException;

    RefreshToken findByToken(String token) throws ResourceNotFoundException;

    RefreshToken verifyTokenExpiration(String token) throws ResourceNotFoundException, InvalidTokenException;
}
