package com.unper.samper.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.unper.samper.model.Subject;

public interface SubjectRepository extends JpaRepository<Subject, Long> {
    Boolean existsByName(String name);
}
