package com.unper.samper.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.unper.samper.model.Student;

public interface StudentRepository extends JpaRepository<Student, Long> {
    
}
