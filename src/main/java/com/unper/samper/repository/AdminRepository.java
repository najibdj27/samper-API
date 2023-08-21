package com.unper.samper.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import com.unper.samper.model.Admin;
import com.unper.samper.model.User;

public interface AdminRepository extends JpaRepository<Admin, Long> {
    Optional<Admin> findByUser(User user);
}
