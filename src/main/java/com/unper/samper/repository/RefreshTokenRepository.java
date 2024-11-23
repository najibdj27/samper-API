package com.unper.samper.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import com.unper.samper.model.RefreshToken;
import com.unper.samper.model.User;

public interface RefreshTokenRepository extends JpaRepository<RefreshToken, Long> {

    Optional<RefreshToken> findByUser(User user);

    Boolean existsByUser(User user);

    Optional<RefreshToken> findByToken(String token);

}
