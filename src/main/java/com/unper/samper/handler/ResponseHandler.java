package com.unper.samper.handler;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

public class ResponseHandler {
    public static ResponseEntity<Object> generateSuccessResponse(HttpStatus status, String message, Object data){
        Map<String, Object> responseMap = new HashMap<>();
        responseMap.put("http_status", status);
        responseMap.put("success", true);
        responseMap.put("message", message);
        responseMap.put("accessed_time", DateTimeFormatter.ofPattern("yyyy-MM-dd hh:mm:ss.SSS").format(ZonedDateTime.now()));
        responseMap.put("data", data);
        return new ResponseEntity<Object>(responseMap, status);
    }
    
    public static ResponseEntity<Object> generateSuccessResponseWithMeta(HttpStatus status, String message, Object data, Object metaData){
        Map<String, Object> responseMap = new HashMap<>();
        responseMap.put("http_status", status);
        responseMap.put("success", true);
        responseMap.put("message", message);
        responseMap.put("accessed_time", DateTimeFormatter.ofPattern("yyyy-MM-dd hh:mm:ss.SSS").format(ZonedDateTime.now()));
        responseMap.put("meta_data", metaData);
        responseMap.put("data", data);
        return new ResponseEntity<Object>(responseMap, status);
    }

    public static ResponseEntity<Object> generateSuccessResponseWithPagination(HttpStatus status, String message, Object data, Object pagination){
        Map<String, Object> responseMap = new HashMap<>();
        responseMap.put("http_status", status);
        responseMap.put("success", true);
        responseMap.put("message", message);
        responseMap.put("accessed_time", DateTimeFormatter.ofPattern("yyyy-MM-dd hh:mm:ss.SSS").format(ZonedDateTime.now()));
        responseMap.put("pagination", pagination);
        responseMap.put("data", data);
        return new ResponseEntity<Object>(responseMap, status);
    }

    public static ResponseEntity<Object> generateErrorResponse(HttpStatus status, String errorMessage, int errorCode, String errorDescription){
        Map<String, Object> responseMap = new HashMap<>();
        responseMap.put("hhtp_status", status);
        responseMap.put("success", false);
        responseMap.put("accessed_time", DateTimeFormatter.ofPattern("yyyy-MM-dd hh:mm:ss.SSS").format(ZonedDateTime.now()));
        responseMap.put("error_code", errorCode);
        responseMap.put("error_message", errorMessage);
        responseMap.put("error_description", errorDescription);
        return new ResponseEntity<Object>(responseMap, status);
    }

    public static ResponseEntity<Object> generateValidationErrorResponse(HttpStatus status, List<String> errorMessages, int errorCode, String errorDescription){
        Map<String, Object> responseMap = new HashMap<>();
        responseMap.put("hhtp_status", status);
        responseMap.put("success", false);
        responseMap.put("accessed_time", DateTimeFormatter.ofPattern("yyyy-MM-dd hh:mm:ss.SSS").format(ZonedDateTime.now()));
        responseMap.put("error_code", errorCode);
        responseMap.put("error_message", errorMessages);
        responseMap.put("error_description", errorDescription);
        return new ResponseEntity<Object>(responseMap, status);
    }
}
