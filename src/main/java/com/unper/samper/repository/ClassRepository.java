package com.unper.samper.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import com.unper.samper.model.Class;

public interface ClassRepository extends JpaRepository<Class, Long> {
    Boolean existsByName(String name);
}
