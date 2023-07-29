package com.unper.samper.repository;

import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.transaction.annotation.Transactional;

import com.unper.samper.model.ResetPasswordToken;

public interface ResetPasswordTokenRepository extends JpaRepository<ResetPasswordToken, Integer> {
    @Modifying
    @Transactional
    @Query("DELETE FROM ResetPasswordToken r WHERE r.emailAddress = :emailAddress")
    void deleteByEmailAddress(@Param("emailAddress") String emailAddress);

    Optional<ResetPasswordToken> findByToken(UUID token);

    @Query("SELECT count(r)>0 FROM ResetPasswordToken r WHERE r.id = :id and r.expiredDate <= :date")
    Boolean isExpired(@Param("id") Integer id, @Param("date") Date date);

    @Query("SELECT r FROM ResetPasswordToken r where r.expiredDate <= CURRENT_TIMESTAMP")
    List<ResetPasswordToken> findExpiredToken();
}
