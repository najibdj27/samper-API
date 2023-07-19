package com.unper.samper.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import com.unper.samper.model.User;


public interface UserRepository extends JpaRepository<User, Long> {
    Boolean existsByUsernameIgnoreCaseOrEmailIgnoreCaseOrPhoneNumber(String username, String email, Integer phoneNumber);

    Optional<User> findByUsername(String username);
}
