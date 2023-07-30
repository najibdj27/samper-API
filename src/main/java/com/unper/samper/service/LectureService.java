package com.unper.samper.service;

import java.util.List;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Lecture;
import com.unper.samper.model.User;
import com.unper.samper.model.dto.AddLectureRequestDto;

public interface LectureService {
    List<Lecture> getAll() throws ResourceNotFoundException;

    Lecture getById(Long id) throws ResourceNotFoundException;

    Lecture getByUser(User user) throws ResourceNotFoundException;

    Lecture getCurrentLecture() throws ResourceNotFoundException;

    Lecture add(AddLectureRequestDto requestDto) throws ResourceAlreadyExistException;

    void delete(Long id);
}
