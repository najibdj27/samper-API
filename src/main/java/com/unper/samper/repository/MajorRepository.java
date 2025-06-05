package com.unper.samper.repository;

import java.util.List;
import java.util.Optional;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.unper.samper.model.Major;

public interface MajorRepository extends JpaRepository<Major, Long> {
    Boolean existsByMajorCode(String majorCode);

    @Override
    @Query(value = "SELECT * FROM common.major WHERE is_deleted IS false", nativeQuery = true)
    List<Major> findAll();

    @Override
    @Query(value = "SELECT * FROM common.major WHERE id = :id AND is_deleted IS false", nativeQuery = true)
    Optional<Major> findById(@Param("id") Long id);

    @Override
    @Modifying
    @Transactional
    @Query(value = "UPDATE common.major SET is_deleted = true WHERE id = :id", nativeQuery = true)
    void deleteById(@Param("id") Long id);
}
