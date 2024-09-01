package com.unper.samper.service;

import com.unper.samper.model.LectureSubject;
import com.unper.samper.model.Subject;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Class;
import com.unper.samper.model.Lecture;

public interface LectureSubjectService {
    LectureSubject getLectureSubjectBySubjectAndClass(Subject subject, Class kelas) throws ResourceNotFoundException;
    
    LectureSubject getByLectureAndSubject(Subject subject, Lecture lecture) throws ResourceNotFoundException;

    LectureSubject getById(Long id) throws ResourceNotFoundException;

    LectureSubject add(Lecture lecture, Subject subject) throws ResourceAlreadyExistException, ResourceNotFoundException;

    Boolean checkLectureSubjectClass(LectureSubject lectureSubject, Class kelas);
    
    void addClass(Long lectureSubjectId, Long classId) throws ResourceNotFoundException;
}

