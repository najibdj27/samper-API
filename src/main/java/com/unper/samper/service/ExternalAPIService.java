package com.unper.samper.service;

import java.io.IOException;

import org.springframework.http.ResponseEntity;

import com.unper.samper.exception.ExternalAPIException;

public interface ExternalAPIService {
    ResponseEntity<String> faceplusplusFaceCompare(String faceToken1, String imageBase64_2) throws ExternalAPIException;

    ResponseEntity<String> faceplusplusDetect(String imageBase64) throws ExternalAPIException;

    ResponseEntity<String> faceplusplusCreateFaceSet(Long outerId, String displayName, String faceToken) throws ExternalAPIException;

    ResponseEntity<String> faceplusplusGetDetail(String facesetToken) throws ExternalAPIException;

    ResponseEntity<String> faceplusplusSetUserId(String faceToken, String userId) throws ExternalAPIException;

    ResponseEntity<String> faceplusplusGetFaceDetail(String faceToken) throws ExternalAPIException;
    
    ResponseEntity<?> cloudinaryUploadBase64Image(String base64Image, String folderPath) throws ExternalAPIException, IOException;
}
