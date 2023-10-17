package com.unper.samper.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.unper.samper.model.Subject;

public interface SubjectRepository extends JpaRepository<Subject, Long> {
    Boolean existsByName(String name);

    @Query(nativeQuery = true, value = "SELECT s.* FROM subject s JOIN subject_major sm ON sm.subject_id = s.id WHERE sm.major_id = :majorId")
    List<Subject> findAllByMajor(@Param("majorId") Long majorId);
}
