package com.unper.samper.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.Student;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.AddStudentRequestDto;
import com.unper.samper.model.dto.StudentResponseDto;
import com.unper.samper.model.dto.UserResponseDto;
import com.unper.samper.repository.StudentRepository;
import com.unper.samper.service.StudentService;

@Service
public class StudentServiceImpl implements StudentService {
    @Autowired
    UserServiceImpl userServiceImpl;

    @Autowired
    StudentRepository studentRepository;

    @Override
    public ResponseEntity<?> getAll() throws ResourceNotFoundException {
        List<Student> studentList = studentRepository.findAll();

        // check if data available
        if (studentList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        List<StudentResponseDto> responseDtoList = new ArrayList<>();
        studentList.forEach(student -> {
            UserResponseDto userResponseDto = new UserResponseDto();
            try {
                userResponseDto = userServiceImpl.getById(student.getId());
            } catch (ResourceNotFoundException e) {
                e.printStackTrace();
            }
            StudentResponseDto responseDto = StudentResponseDto.builder()
                .NIM(student.getNIM())
                .user(userResponseDto)
                .build();
            responseDtoList.add(responseDto);
        });

        Map<String, Object> metaData = new HashMap<>();
        metaData.put("_total", responseDtoList.size());

        return ResponseHandler.generateSuccessResponseWithMeta(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), metaData, responseDtoList);
    }

    @Override
    public ResponseEntity<?> getById(Long id) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'add'");
    }

    @Override
    public ResponseEntity<?> add(AddStudentRequestDto requestDto) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'add'");
    }

    @Override
    public ResponseEntity<?> delete(Long id) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'delete'");
    }
    
}
