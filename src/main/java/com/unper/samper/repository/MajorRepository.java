package com.unper.samper.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.unper.samper.model.Major;

public interface MajorRepository extends JpaRepository<Major, Long> {
    Boolean existsByMajorCode(String majorCode);
}
