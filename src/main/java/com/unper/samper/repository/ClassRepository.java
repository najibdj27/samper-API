package com.unper.samper.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.unper.samper.model.Class;
import com.unper.samper.model.Lecture;

public interface ClassRepository extends JpaRepository<Class, Long> {
    Boolean existsByName(String name);

    @Override
    @Query(value = "SELECT * FROM common.class WHERE is_deleted = false", nativeQuery = true)
    List<Class> findAll();

    @Query("select c from Class c where c.lecture = :lecture")
    List<Class> getAllByLecture(Lecture lecture);

    @Override
    @Query(value = "SELECT * FROM common.class WHERE is_deleted = false AND id = :id", nativeQuery = true)
    Optional<Class> findById(@Param("id") Long id);

    @Override
    @Query(value = "UPDATE common.class SET is_deleted = true WHERE id = :id", nativeQuery = true)
    void deleteById(@Param("id") Long id);
}
