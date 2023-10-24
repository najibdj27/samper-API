package com.unper.samper.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.unper.samper.model.Lecture;
import com.unper.samper.model.User;

public interface LectureRepository extends JpaRepository<Lecture, Long> {
    @Override
    @Query(value = "SELECT l.* FROM public.lecture l JOIN public.user u ON l.user_id = u.id WHERE u.is_deleted IS false", nativeQuery = true)
    List<Lecture> findAll();

    @Override
    @Query(value = "SELECT l.* FROM public.lecture l JOIN public.user u ON l.user_id = u.id WHERE l.id = :lectureId AND u.is_deleted IS false", nativeQuery = true)
    Optional<Lecture> findById(@Param("lectureId") Long id);

    @Override
    @Query(value = "SELECT l.* FROM public.lecture l JOIN public.user u ON l.user_id = u.id WHERE l.id in (:lectureIds) AND u.is_deleted IS false", nativeQuery = true)
    List<Lecture> findAllById(@Param("lectureIds") Iterable<Long> ids);

    Boolean existsByNIP(String NIP);

    Optional<Lecture> findByUser(User user);
}
