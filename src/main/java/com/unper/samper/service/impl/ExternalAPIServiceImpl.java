package com.unper.samper.service.impl;

import java.io.IOException;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
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

import com.cloudinary.Cloudinary;
import com.cloudinary.utils.ObjectUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
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
    
    @Value("${external-api.faceplusplus.end-point.CREATE_FACESET}")
    private String FACEPLUSPLUS_FACESET_CREATE;
    
    @Value("${external-api.faceplusplus.end-point.GET_DETAIL_FACESET}")
    private String FACEPLUSPLUS_GET_DETAIL_FACESET;
    
    @Value("${external-api.faceplusplus.end-point.ADD_USER_ID_FACE}")
    private String FACEPLUSPLUS_ADD_USER_ID;
    
    @Value("${external-api.faceplusplus.end-point.GET_DETAIL_FACE}")
    private String FACEPLUSPLUS_GET_FACE_DETAIL;

    @Autowired
    private Cloudinary cloudinary;

    private final String FACE_ATTRIBUTES = "eyestatus,smiling,gender,headpose,eyegaze,mouthstatus";

    RestTemplate restTemplate = new RestTemplate();

    @Override
    public Map<?,?> faceplusplusFaceCompare(String faceToken1, String imageBase64_2) throws ExternalAPIException, JsonMappingException, JsonProcessingException {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);

        MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
        params.add("api_key", FACEPLUSPLUS_API_KEY); 
        params.add("api_secret", FACEPLUSPLUS_API_SECRET);
        params.add("face_token1", faceToken1);
        params.add("image_base64_2", imageBase64_2);
        
        HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(params, headers);

        ResponseEntity<String> response = restTemplate.postForEntity(FACEPLUSPLUS_BASE_URL+FACEPLUSPLUS_FACE_COMPARE, request, String.class);
        ObjectMapper mapper = new ObjectMapper();
        String responseBody = response.getBody();
        Map<?,?> responseMap = mapper.readValue(responseBody, Map.class);
        if (response.getStatusCode().equals(HttpStatus.OK)) {
            return responseMap;
        } else {
            throw new ExternalAPIException("Failed when calling faceplusplus face compare API.");
        } 

    }
    
    @Override
    public Map<?,?> faceplusplusDetect(String imageBase64) throws ExternalAPIException, JsonMappingException, JsonProcessingException {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
        
        MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
        params.add("api_key", FACEPLUSPLUS_API_KEY);
        params.add("api_secret", FACEPLUSPLUS_API_SECRET);
        params.add("image_base64", imageBase64);
        params.add("return_attributes", FACE_ATTRIBUTES);

        HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(params, headers);

        ResponseEntity<String> response = restTemplate.postForEntity(FACEPLUSPLUS_BASE_URL+FACEPLUSPLUS_FACE_DETECT, request, String.class);
        ObjectMapper mapper = new ObjectMapper();
        String responseBody = response.getBody();
        Map<?,?> responseMap = mapper.readValue(responseBody, Map.class);
        if (response.getStatusCode().equals(HttpStatus.OK)) {
            return responseMap;
        } else {
            throw new ExternalAPIException("Failed when calling faceplusplus face detect API.");
        }
    }

    @Override
    public Map<?,?> faceplusplusCreateFaceSet(Long outerId, String displayName, String faceToken) throws ExternalAPIException, JsonMappingException, JsonProcessingException{
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
        ObjectMapper mapper = new ObjectMapper();
        String responseBody = response.getBody();
        Map<?,?> responseMap = mapper.readValue(responseBody, Map.class);
        if (response.getStatusCode().equals(HttpStatus.OK)) {
            return responseMap;
        } else {
            throw new ExternalAPIException("Failed when calling faceplusplus faceset create API.");
        }
    }

    @Override
    public Map<?,?> faceplusplusGetDetail(String facesetToken) throws ExternalAPIException, JsonMappingException, JsonProcessingException {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
        
        MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
        params.add("api_key", FACEPLUSPLUS_API_KEY);
        params.add("api_secret", FACEPLUSPLUS_API_SECRET);
        params.add("faceset_token", facesetToken);

        HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(params, headers);

        ResponseEntity<String> response = restTemplate.postForEntity(FACEPLUSPLUS_BASE_URL+FACEPLUSPLUS_GET_DETAIL_FACESET, request, String.class);
        ObjectMapper mapper = new ObjectMapper();
        String responseBody = response.getBody();
        Map<?,?> responseMap = mapper.readValue(responseBody, Map.class);
        if (response.getStatusCode().equals(HttpStatus.OK)) {
            return responseMap;
        } else {
            throw new ExternalAPIException("Failed when calling faceplusplus get detail API.");
        }
    }
    
    @Override
    public Map<?,?> faceplusplusSetUserId(String faceToken, String userId) throws ExternalAPIException, JsonMappingException, JsonProcessingException {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
        
        MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
        params.add("api_key", FACEPLUSPLUS_API_KEY);
        params.add("api_secret", FACEPLUSPLUS_API_SECRET);
        params.add("face_token", faceToken);
        params.add("user_id", userId);
        
        HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(params, headers);
        
        ResponseEntity<String> response = restTemplate.postForEntity(FACEPLUSPLUS_BASE_URL+FACEPLUSPLUS_ADD_USER_ID, request, String.class);
        ObjectMapper mapper = new ObjectMapper();
        String responseBody = response.getBody();
        Map<?,?> responseMap = mapper.readValue(responseBody, Map.class);
        if (response.getStatusCode().equals(HttpStatus.OK)) {
            return responseMap;
        } else {
            throw new ExternalAPIException("Failed when calling faceplusplus set user id API.");
        }
    }
    
    @Override
    public Map<?,?> faceplusplusGetFaceDetail(String faceToken) throws ExternalAPIException, JsonMappingException, JsonProcessingException {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
        
        MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
        params.add("api_key", FACEPLUSPLUS_API_KEY);
        params.add("api_secret", FACEPLUSPLUS_API_SECRET);
        params.add("face_token", faceToken);
    
        HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(params, headers);
    
        ResponseEntity<String> response = restTemplate.postForEntity(FACEPLUSPLUS_BASE_URL+FACEPLUSPLUS_GET_FACE_DETAIL, request, String.class);
        ObjectMapper mapper = new ObjectMapper();
        String responseBody = response.getBody();
        Map<?,?> responseMap = mapper.readValue(responseBody, Map.class);
        if (response.getStatusCode().equals(HttpStatus.OK)) {
            return responseMap;
        } else {
            throw new ExternalAPIException("Failed when calling faceplusplus set user id API.");
        }
    }

    @Override
    public Map<?,?> cloudinaryUploadBase64Image(String base64Image, String folderPath) throws ExternalAPIException, IOException {
        if (base64Image.contains(",")) {
            base64Image = base64Image.split(",")[1];
        }

        Map<?,?> uploadResult = cloudinary.uploader().upload(
            "data:image/jpeg;base64,"+base64Image, 
            ObjectUtils.asMap(
                "resource_type", "image",
                "folder",folderPath)
        );

        return uploadResult;
    }
}
