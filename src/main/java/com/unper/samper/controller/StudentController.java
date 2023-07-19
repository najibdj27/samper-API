package com.unper.samper.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.service.impl.StudentServiceImpl;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

@Tag(name = "1. Student Controller")
@RequestMapping("/student")
@RestController
public class StudentController {
    @Autowired
    StudentServiceImpl studentServiceImpl;
    
    /***
     * Get data of all student 
     * @throws ResourceNotFoundException
     */
    @Operation(summary = "Get data of all student")
    @RequestMapping("/all")
    public ResponseEntity<?> getAll() throws ResourceNotFoundException {
        return studentServiceImpl.getAll();
    }
}
