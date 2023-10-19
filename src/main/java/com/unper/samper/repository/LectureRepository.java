package com.unper.samper.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import com.unper.samper.model.Lecture;
import com.unper.samper.model.User;

public interface LectureRepository extends JpaRepository<Lecture, Long> {
    @Query(value = "SELECT l.* FROM public.lecture l JOIN public.user u ON l.user_id = u.id where u.is_deleted IS NOT true", nativeQuery = true)    
    List<Lecture> findAllAvailable();

    Boolean existsByNIP(String NIP);

    Optional<Lecture> findByUser(User user);
}
