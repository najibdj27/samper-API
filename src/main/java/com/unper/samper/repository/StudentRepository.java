package com.unper.samper.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import com.unper.samper.model.Student;

public interface StudentRepository extends JpaRepository<Student, Long> {
    Optional<Student> findByUser();
}
