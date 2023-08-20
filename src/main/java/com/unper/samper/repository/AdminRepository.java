package com.unper.samper.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.unper.samper.model.Admin;

public interface AdminRepository extends JpaRepository<Admin, Long> {
    
}
