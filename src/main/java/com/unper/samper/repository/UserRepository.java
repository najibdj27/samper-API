package com.unper.samper.repository;

import java.util.Optional;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.unper.samper.model.User;


public interface UserRepository extends JpaRepository<User, Long> {
    Boolean existsByUsernameIgnoreCaseOrEmailIgnoreCaseOrPhoneNumber(String username, String email, String phoneNumber);

    Boolean existsByUsername(String username);
    
    Boolean existsByEmail(String email);
    
    Boolean existsByPhoneNumber(String phoneNumber);
    
    Optional<User> findByUsername(String username);

    @Query("select u from User u where lower(u.username) = :val or lower(u.email) = :val")
    Optional<User> findByUsernameOrEmail(@Param("val") String usernameOrEmail);

    Optional<User> findByEmail(String email);

    @Override
    @Modifying
    @Transactional
    @Query(value = "UPDATE auth.user SET is_deleted = true WHERE id = :id", nativeQuery = true)
    void deleteById(@Param("id")Long id);
}
