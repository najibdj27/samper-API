package com.unper.samper.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.unper.samper.model.User;

@Repository
public interface UserRepository extends JpaRepository<User, Long> {
    Boolean existsByUsernameIgnoreCaseOrEmailIgnoreCaseOrPhoneNumber(String username, String email, Integer phoneNumber);
}
