package com.unper.samper.service;

import java.util.List;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Lecture;
import com.unper.samper.model.User;
import com.unper.samper.model.dto.AddLectureRequestDto;
import com.unper.samper.model.dto.AddLectureSubjectRequestDto;

public interface LectureService {
    List<Lecture> getAll() throws ResourceNotFoundException;

    Lecture getById(Long id) throws ResourceNotFoundException;

    List<Lecture> getAllById(List<Long> id) throws ResourceNotFoundException;

    Lecture getByUser(User user) throws ResourceNotFoundException;

    Lecture getCurrentLecture() throws ResourceNotFoundException;

    Boolean existsByNIP(String nip);

    Lecture add(AddLectureRequestDto requestDto) throws ResourceAlreadyExistException;

    Lecture addSubject(AddLectureSubjectRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException;

    void delete(Long id) throws ResourceNotFoundException;
}
