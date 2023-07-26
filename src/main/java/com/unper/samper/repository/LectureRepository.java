package com.unper.samper.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.unper.samper.model.Lecture;

public interface LectureRepository extends JpaRepository<Lecture, Long> {
    Boolean existsByNIP(Long NIP);
}
