package com.unper.samper.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.Previllage;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.service.impl.AdminPrevillageServiceImpl;

import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

@SecurityRequirement(name = "bearer-key")
@Tag(name = "Admin Previllage")
@RestController
@RequestMapping("/adminprevillage")
public class AdminPrevillageController {
    @Autowired
    AdminPrevillageServiceImpl adminPrevillageServiceImpl;
    
    @GetMapping("/getbycurrentadmin")
    public ResponseEntity<?> getAllByCurrentAdmin() throws ResourceNotFoundException {
        List<Previllage> previllageList = adminPrevillageServiceImpl.getAllByCurrentAdmin();

        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), previllageList);
    }
}
