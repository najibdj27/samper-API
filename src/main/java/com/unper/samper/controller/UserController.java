package com.unper.samper.controller;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.UserResponseDto;
import com.unper.samper.service.impl.UserServiceImpl;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

@SecurityRequirement(name = "bearer-key")
@Tag(name = "2. User Controller")
@RestController
@RequestMapping("/user")
public class UserController {
    @Autowired
    UserServiceImpl userServiceImpl;
    
    /***
     * Get all data of user
     * @return 
     * @throws ResourceNotFoundException
     */
    @Operation(summary = "Get all data of user")
    @PreAuthorize("hasAuthority('ROLE_ADMIN') or hasAuthority('ROLE_LECTURE') or hasAuthority('ROLE_STUDENT')")
    @GetMapping("/all")
    public ResponseEntity<?> getAll() throws ResourceNotFoundException {
        List<UserResponseDto> responseDtoList = userServiceImpl.getAll();

        // meta data
        Map<String, Object> metaData = new HashMap<>();
        metaData.put("_total", responseDtoList.size());

        return ResponseHandler.generateSuccessResponseWithMeta(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDtoList,  metaData);
    }
}
