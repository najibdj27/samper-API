package com.unper.samper.service;

import org.springframework.http.ResponseEntity;

import com.unper.samper.exception.ExternalAPIException;

public interface ExternalAPIService {
    ResponseEntity<String> faceCompare(String imageBase64_1, String imageBase64_2) throws ExternalAPIException;
}
