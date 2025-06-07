package com.unper.samper.logging;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.reflect.MethodSignature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

@Aspect
@Component
public class RequestLoggingAspect {

    private static final Logger logger = LoggerFactory.getLogger(RequestLoggingAspect.class);
    private static final ObjectMapper objectMapper = new ObjectMapper();

    // ✅ List of URIs to exclude
    private static final Set<String> EXCLUDED_URIS = Set.of(
            "/auth/signin",
            "/auth/refreshtoken",
            "/auth/register*",
            "/auth/checktoken"
    );

    @Before("execution(* com.unper.samper.controller..*(..))") // Apply to all controllers in this package
    public void logRequest(JoinPoint joinPoint) throws Throwable {
        ServletRequestAttributes attributes = (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();

        if (attributes == null) return;

        HttpServletRequest request = attributes.getRequest();

        String httpMethod = request.getMethod();
        String uri = request.getRequestURI();

        
        Map<String, Object> paramMap = new HashMap<>();
        Object requestBody = null;
        
        MethodSignature methodSignature = (MethodSignature) joinPoint.getSignature();
        Method method = methodSignature.getMethod();
        Annotation[][] paramAnnotations = method.getParameterAnnotations();
        Object[] args = joinPoint.getArgs();
        
        for (int i = 0; i < paramAnnotations.length; i++) {
            for (Annotation annotation : paramAnnotations[i]) {
                if (annotation instanceof RequestParam) {
                    RequestParam requestParam = (RequestParam) annotation;
                    String name = !requestParam.value().isEmpty() ? requestParam.value() : methodSignature.getParameterNames()[i];
                    paramMap.put(name, args[i]);
                } else if (annotation instanceof RequestBody) {
                    requestBody = args[i];
                }
            }
        }
        
        logger.info("[HTTP REQUEST] {} {}", httpMethod, uri);
        if (EXCLUDED_URIS.contains(uri)) return;

        if (!paramMap.isEmpty()) {
            logger.info("[REQUEST PARAMS] {}", objectMapper.writeValueAsString(paramMap));
        }
        if (requestBody != null) {
            logger.info("[REQUEST BODY] {}", objectMapper.writeValueAsString(requestBody));
        }
    }
}
