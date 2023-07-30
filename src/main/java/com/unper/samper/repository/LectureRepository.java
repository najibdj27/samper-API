package com.unper.samper.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import com.unper.samper.model.Lecture;
import com.unper.samper.model.User;

public interface LectureRepository extends JpaRepository<Lecture, Long> {
    Boolean existsByNIP(Long NIP);

    Optional<Lecture> findByUser(User user);
}
