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

    RestTemplate restTemplate = new RestTemplate();

    @Override
    public ResponseEntity<String> faceCompare(String imageBase64_1, String imageBase64_2) throws ExternalAPIException {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);

        MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
        params.add("api_key", FACEPLUSPLUS_API_KEY); 
        params.add("api_secret", FACEPLUSPLUS_API_SECRET);
        params.add("image_base64_1", imageBase64_1);
        params.add("image_base64_2", imageBase64_2);
        
        HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(params, headers);

        try {
            ResponseEntity<String> response = restTemplate.postForEntity(FACEPLUSPLUS_BASE_URL, request, String.class);
            if (response.getStatusCode().equals(HttpStatus.OK)) {
                return response;
            } else {
                throw new ExternalAPIException("Failed when calling face compare API.");
            } 

        } catch (Exception e) {
            // TODO: handle exception
            return null;
        }

    }
    
}
