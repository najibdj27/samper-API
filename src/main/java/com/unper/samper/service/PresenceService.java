package com.unper.samper.service;

import java.util.List;

import org.springframework.data.geo.Point;

import com.unper.samper.domain.dao.Presence;

public interface PresenceService {
    List<Presence> getAll();

    Presence get();

    void record(Point location);

    void confirm();
}
