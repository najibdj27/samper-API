package com.unper.samper.service;

import org.springframework.http.ResponseEntity;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Lecture;
import com.unper.samper.model.dto.AddLectureRequestDto;

public interface LectureService {
    ResponseEntity<?> getAll() throws ResourceNotFoundException;

    Lecture getById(Long id) throws ResourceNotFoundException;

    Lecture add(AddLectureRequestDto requestDto) throws ResourceAlreadyExistException;

    ResponseEntity<?> delete(Long id);
}
