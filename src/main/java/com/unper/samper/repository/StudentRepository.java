package com.unper.samper.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.unper.samper.model.Student;
import com.unper.samper.model.User;

public interface StudentRepository extends JpaRepository<Student, Long> {
    Optional<Student> findByUser(User user);

    @Override
    @Query(value = "SELECT s.* FROM public.student s JOIN public.user u ON u.id = s.user_id WHERE u.is_deleted = false", nativeQuery = true)
    List<Student> findAll();
    
    @Override
    @Query(value = "SELECT s.* FROM public.student s JOIN public.user u ON u.id = s.user_id WHERE u.is_deleted = false AND s.id = :id", nativeQuery = true)
    Optional<Student> findById(@Param("id") Long id);
}
