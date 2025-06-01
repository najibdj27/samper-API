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

import com.unper.samper.model.Token;
import com.unper.samper.model.constant.EType;


public interface TokenRepository extends JpaRepository<Token, Long> {
    @Modifying
    @Transactional
    @Query("DELETE FROM Token t WHERE t.key = :key")
    void deleteBykey(@Param("key") String key);

    Optional<Token> findByToken(UUID token);

    Optional<Token> findByKey(String key);

    Optional<Token> findByKeyAndType(String key, EType type);

    @Query("SELECT t FROM Token t WHERE t.expiredDate <= :expiredDate")
    List<Token> findByLessThanExpiredDate(Date expiredDate);
 
    @Query("SELECT count(t)>0 FROM Token t WHERE t.id = :id and t.expiredDate <= :date")
    Boolean isExpired(@Param("id") Long id, @Param("date") Date date);

    @Query("SELECT t FROM Token t where t.expiredDate <= CURRENT_TIMESTAMP")
    List<Token> findExpiredToken();
}
