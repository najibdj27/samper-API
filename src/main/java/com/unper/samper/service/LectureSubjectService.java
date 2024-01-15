package com.unper.samper.service;

import com.unper.samper.model.LectureSubject;
import com.unper.samper.model.Subject;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Class;

public interface LectureSubjectService {
    LectureSubject getLectureSubjectBySubjectAndClass(Subject subject, Class kelas) throws ResourceNotFoundException;
}
