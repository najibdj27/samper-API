package com.unper.samper.repository;

import java.util.List;
import java.util.Optional;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.unper.samper.model.Subject;

public interface SubjectRepository extends JpaRepository<Subject, Long> {
    Boolean existsByName(String name);

    @Query(nativeQuery = true, value = "SELECT s.* FROM subject s JOIN subject_major sm ON sm.subject_id = s.id WHERE sm.major_id = :majorId")
    List<Subject> findAllByMajor(@Param("majorId") Long majorId);

    @Override
    @Query(value = "SELECT * FROM public.subject WHERE is_deleted = false", nativeQuery = true)
    List<Subject> findAll();

    @Override
    @Query(value = "SELECT * FROM public.subject WHERE is_deleted = false AND id = :id", nativeQuery = true)
    Optional<Subject> findById(@Param("id") Long id);

    @Override
    @Modifying
    @Transactional
    @Query(value = "UPDATE public.subject SET is_deleted = true WHERE id = :id", nativeQuery = true)
    void deleteById(@Param("id") Long id);
}
