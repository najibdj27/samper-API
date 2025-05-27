package com.unper.samper.service;

import java.util.Date;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Token;

public interface TokenService {
    Token getById(Long id) throws ResourceNotFoundException;

    Token getByKey(String key) throws ResourceNotFoundException;
    
    Token create(Token token) throws ResourceAlreadyExistException, ResourceNotFoundException;

    void deleteByKey(String key) throws ResourceNotFoundException;

    void houskeeper();

    boolean isExpired(Long id, Date date) throws ResourceNotFoundException;
}
