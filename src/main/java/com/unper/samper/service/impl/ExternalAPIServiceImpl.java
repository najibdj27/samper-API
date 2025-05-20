package com.unper.samper.service.impl;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

import com.unper.samper.exception.ExternalAPIException;
import com.unper.samper.service.ExternalAPIService;

@Service
public class ExternalAPIServiceImpl implements ExternalAPIService {

    @Value("${external-api.faceplusplus.base-url}")
    private String FACEPLUSPLUS_BASE_URL;

    @Value("${external-api.faceplusplus.api-key}")
    private String FACEPLUSPLUS_API_KEY;

    @Value("${external-api.faceplusplus.api-secret}")
    private String FACEPLUSPLUS_API_SECRET;

    @Value("${external-api.faceplusplus.end-point.FACE_COMPARE}")
    private String FACEPLUSPLUS_FACE_COMPARE;
    
    @Value("${external-api.faceplusplus.end-point.FACE_DETECT}")
    private String FACEPLUSPLUS_FACE_DETECT;
    
    @Value("${external-api.faceplusplus.end-point.CREATE}")
    private String FACEPLUSPLUS_FACESET_CREATE;
    
    @Value("${external-api.faceplusplus.end-point.GET_DETAIL}")
    private String FACEPLUSPLUS_GET_DETAIL;

    private final String FACE_ATTRIBUTES = "eyestatus,smiling,gender,headpose,eyegaze,mouthstatus";

    RestTemplate restTemplate = new RestTemplate();

    @Override
    public ResponseEntity<String> faceplusplusFaceCompare(String faceToken1, String imageBase64_2) throws ExternalAPIException {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);

        MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
        params.add("api_key", FACEPLUSPLUS_API_KEY); 
        params.add("api_secret", FACEPLUSPLUS_API_SECRET);
        params.add("face_token1", faceToken1);
        params.add("image_base64_2", imageBase64_2);
        
        HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(params, headers);

        ResponseEntity<String> response = restTemplate.postForEntity(FACEPLUSPLUS_BASE_URL+FACEPLUSPLUS_FACE_COMPARE, request, String.class);
        if (response.getStatusCode().equals(HttpStatus.OK)) {
            return response;
        } else {
            throw new ExternalAPIException("Failed when calling faceplusplus face compare API.");
        } 

    }
    
    @Override
    public ResponseEntity<String> faceplusplusDetect(String imageBase64) throws ExternalAPIException {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
        
        MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
        params.add("api_key", FACEPLUSPLUS_API_KEY);
        params.add("api_secret", FACEPLUSPLUS_API_SECRET);
        params.add("imageBase64", imageBase64);
        params.add("return_attributes", FACE_ATTRIBUTES);

        HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(params, headers);

        ResponseEntity<String> response = restTemplate.postForEntity(FACEPLUSPLUS_BASE_URL+FACEPLUSPLUS_FACE_COMPARE, request, String.class);
        if (response.getStatusCode().equals(HttpStatus.OK)) {
            return response;
        } else {
            throw new ExternalAPIException("Failed when calling faceplusplus face detect API.");
        }
    }

    @Override
    public ResponseEntity<String> faceplusplusCreateFaceSet(Long outerId, String displayName, String faceToken) throws ExternalAPIException{
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
        
        MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
        params.add("api_key", FACEPLUSPLUS_API_KEY);
        params.add("api_secret", FACEPLUSPLUS_API_SECRET);
        params.add("display_name", displayName);
        params.add("outer_id", Long.toString(outerId));
        params.add("face_token", faceToken);

        HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(params, headers);

        ResponseEntity<String> response = restTemplate.postForEntity(FACEPLUSPLUS_BASE_URL+FACEPLUSPLUS_FACESET_CREATE, request, String.class);
        if (response.getStatusCode().equals(HttpStatus.OK)) {
            return response;
        } else {
            throw new ExternalAPIException("Failed when calling faceplusplus faceset create API.");
        }
    }

    @Override
    public ResponseEntity<String> faceplusplusGetDetail(String facesetToken) throws ExternalAPIException {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
        
        MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
        params.add("api_key", FACEPLUSPLUS_API_KEY);
        params.add("api_secret", FACEPLUSPLUS_API_SECRET);
        params.add("faceset_token", facesetToken);

        HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(params, headers);

        ResponseEntity<String> response = restTemplate.postForEntity(FACEPLUSPLUS_BASE_URL+FACEPLUSPLUS_GET_DETAIL, request, String.class);
        if (response.getStatusCode().equals(HttpStatus.OK)) {
            return response;
        } else {
            throw new ExternalAPIException("Failed when calling faceplusplus get detail API.");
        }
    }
}
