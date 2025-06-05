package com.unper.samper.repository;

import java.util.List;
import java.util.Optional;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.unper.samper.model.Request;

public interface RequestRepository extends JpaRepository<Request, Long> {

    @Query(value = "SELECT * FROM schedule.request WHERE (:senderId is null or sender_id = :senderId) AND (:receiverId is null or receiver_id = :receiverId) AND (:requestTimeFrom is null or request_time >= TO_TIMESTAMP(:requestTimeFrom, 'YYYY-MM-DD')) AND (:requestTimeTo is null or request_time <= TO_TIMESTAMP(:requestTimeTo, 'YYYY-MM-DD')) AND is_deleted = false", nativeQuery = true)
    List<Request> findAllWithFilter(@Param("senderId") Long senderId, @Param("receiverId") Long receiverId, @Param("requestTimeFrom") String requestTimeFrom, @Param("requestTimeTo") String requestTimeTo);

    @Override
    @Query(value = "SELECT * FROM schedule.request WHERE is_deleted = false", nativeQuery = true)
    List<Request> findAll();

    @Override
    @Query(value = "SELECT * FROM schedule.request WHERE is_deleted = false AND id = :id", nativeQuery = true)
    Optional<Request> findById(@Param("id") Long id);

    @Override
    @Modifying
    @Transactional
    @Query(value = "UPDATE schedule.request SET is_deleted = true WHERE id = :id", nativeQuery = true)
    void deleteById(@Param("id") Long id);
}
