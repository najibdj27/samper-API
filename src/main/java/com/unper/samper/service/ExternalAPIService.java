package com.unper.samper.service;

import java.io.IOException;
import java.util.Map;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.unper.samper.exception.ExternalAPIException;

public interface ExternalAPIService {
    Map<?,?> faceplusplusFaceCompare(String faceToken1, String imageBase64_2) throws ExternalAPIException, JsonMappingException, JsonProcessingException;

    Map<?,?> faceplusplusDetect(String imageBase64) throws ExternalAPIException, JsonMappingException, JsonProcessingException;

    Map<?,?> faceplusplusCreateFaceSet(Long outerId, String displayName, String faceToken) throws ExternalAPIException, JsonMappingException, JsonProcessingException;

    Map<?,?> faceplusplusGetDetail(String facesetToken) throws ExternalAPIException, JsonMappingException, JsonProcessingException;

    Map<?,?> faceplusplusSetUserId(String faceToken, String userId) throws ExternalAPIException, JsonMappingException, JsonProcessingException;

    Map<?,?> faceplusplusGetFaceDetail(String faceToken) throws ExternalAPIException, JsonMappingException, JsonProcessingException;
    
    Map<?,?> cloudinaryUploadBase64Image(String base64Image, String folderPath) throws ExternalAPIException, IOException;
}
