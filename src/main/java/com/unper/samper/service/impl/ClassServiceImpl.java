package com.unper.samper.service.impl;

import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.Class;
import com.unper.samper.model.Lecture;
import com.unper.samper.model.Subject;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.AddClassRequestDto;
import com.unper.samper.repository.ClassRepository;
import com.unper.samper.service.ClassService;

@Service
public class ClassServiceImpl implements ClassService {
    @Autowired
    ClassRepository classRepository;

    @Autowired
    LectureServiceImpl lectureServiceImpl;

    @Autowired
    SubjectServiceImpl subjectServiceImpl;

    @Override
    public Class getById(Long id) throws ResourceNotFoundException {
        Optional<Class> kelasOptional = classRepository.findById(id); 
        // check if class exist
        if (kelasOptional.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        return kelasOptional.get();
    }

    @Override
    public ResponseEntity<?> addClass(AddClassRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException {
        if (Boolean.TRUE.equals(classRepository.existsByTittle(requestDto.getTittle()))) {
            throw new ResourceAlreadyExistException(EResponseMessage.INSERT_DATA_ALREADY_EXIST.getMessage());
        }

        Lecture lecture = lectureServiceImpl.getById(requestDto.getLectureId());
        Subject subject = subjectServiceImpl.getById(requestDto.getSubjectId());
        Class kelas = Class.builder()
            .lecture(lecture)
            .subject(subject)
            .tittle(requestDto.getTittle())
            .build();

        classRepository.save(kelas);
        return ResponseHandler.generateSuccessResponse(HttpStatus.CREATED, EResponseMessage.INSERT_DATA_SUCCESS.getMessage(), null);
    }
    
}
