package com.unper.samper.service.impl;

import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Lecture;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.AddLectureRequestDto;
import com.unper.samper.repository.LectureRepository;
import com.unper.samper.service.LectureService;

@Service
public class LectureServiceImpl implements LectureService {
    @Autowired
    LectureRepository lectureRepository;

    @Override
    public ResponseEntity<?> getAll() throws ResourceNotFoundException {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'getAll'");
    }

    @Override
    public Lecture getById(Long id) throws ResourceNotFoundException {
        Optional<Lecture> lecture = lectureRepository.findById(id);
        if (Boolean.FALSE.equals(lecture.isPresent())) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        return lecture.get();
    }

    @Override
    public Lecture add(AddLectureRequestDto requestDto) throws ResourceAlreadyExistException {
        Lecture lecture = Lecture.builder()
            .NIP(requestDto.getNIP())
            .user(requestDto.getUser())
            .build();
        
        // check if NIP already exist
        if (Boolean.TRUE.equals(lectureRepository.existsByNIP(requestDto.getNIP()))) {
            throw new ResourceAlreadyExistException(EResponseMessage.INSERT_DATA_ALREADY_EXIST.getMessage());
        }

        Lecture newLecture = lectureRepository.save(lecture);
        return newLecture;
    }

    @Override
    public ResponseEntity<?> delete(Long id) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'delete'");
    }

}
