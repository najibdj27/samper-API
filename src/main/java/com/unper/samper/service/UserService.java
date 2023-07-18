package com.unper.samper.service;

import org.springframework.http.ResponseEntity;

import com.unper.samper.model.dto.AddUserRequestDto;

public interface UserService {
    ResponseEntity<?> getAll();

    ResponseEntity<?> getById();

    ResponseEntity<?> add(AddUserRequestDto dto);
}
