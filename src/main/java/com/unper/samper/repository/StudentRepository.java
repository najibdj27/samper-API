package com.unper.samper.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.unper.samper.model.Class;
import com.unper.samper.model.Lecture;
import com.unper.samper.model.Student;
import com.unper.samper.model.User;

public interface StudentRepository extends JpaRepository<Student, Long> {
    Optional<Student> findByUser(User user);

    @Query(value = "SELECT s.* FROM profile.student s WHERE s.class_id = :classId AND s.is_leader = true", nativeQuery = true)
    Optional<Student> findStudentLeaderByClass(@Param("classId") Long classId);

    @Override
    @Query(value = "SELECT s.* FROM profile.student s LEFT JOIN auth.user u ON u.id = s.user_id WHERE u.is_deleted = false", nativeQuery = true)
    List<Student> findAll();

    @Query("select s from Student s where s.user.isDeleted = false and s.kelas = :kelas")
    List<Student> findAllByClass(@Param("kelas") Class kelas);

    @Query("select s from Student s where s.user.isDeleted = false and s.kelas.lecture = :lecture and s.kelas = :kelas")
    List<Student> findAllByLectureAndClass(@Param("lecture") Lecture lecture, @Param("kelas") Class kelas);
    
    @Override
    @Query(value = "SELECT s.* FROM profile.student s LEFT JOIN auth.user u ON u.id = s.user_id WHERE u.is_deleted = false AND s.id = :id", nativeQuery = true)
    Optional<Student> findById(@Param("id") Long id);

    Boolean existsByNIM(String nim);
}
