package com.unper.samper.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.unper.samper.model.Presence;

public interface PresenceRepository extends JpaRepository<Presence, Long> {
    
}
