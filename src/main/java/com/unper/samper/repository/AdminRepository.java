package com.unper.samper.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.unper.samper.model.Admin;
import com.unper.samper.model.User;

public interface AdminRepository extends JpaRepository<Admin, Long> {
    Optional<Admin> findByUser(User user);

    @Override
    @Query(value = "SELECT a.* FROM public.admin a JOIN public.user u ON a.user_id = u.id WHERE u.is_deleted = false", nativeQuery = true)
    List<Admin> findAll();

    @Override
    @Query(value = "SELECT a.* FROM public.admin a JOIN public.user u ON a.user_id = u.id WHERE u.is_deleted = false AND a.id = :id", nativeQuery = true)
    Optional<Admin> findById(@Param("id") Long id);
}
