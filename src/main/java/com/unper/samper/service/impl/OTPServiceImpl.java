package com.unper.samper.service.impl;

import java.util.concurrent.TimeUnit;

import javax.annotation.Nonnull;
import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.WrongOTPException;
import com.unper.samper.model.Token;
import com.unper.samper.model.constant.EType;
import com.unper.samper.model.dto.ConfirmOTPResponseDto;
import com.unper.samper.service.OTPService;

import lombok.NonNull;

@Service
public class OTPServiceImpl implements OTPService{
    @Autowired
    TokenServiceImpl tokenServiceImpl;

    @Value("${com.unper.samper.otp-expiration-ms}")
    int otpExpiration;

    private LoadingCache<String, Integer> otpCache;

    @PostConstruct
    public void init() {
        otpCache = CacheBuilder.newBuilder()
                .expireAfterWrite(otpExpiration, TimeUnit.MILLISECONDS)
                .build(new CacheLoader<String, Integer>() {
                    @Override
                    public Integer load(@Nonnull String key) {
                        return 0;
                    }
                });
    }

    @Override
    public int generateOTP(@NonNull String key) {
        int otp =  (int)(Math.random()*9000)+1000;
        otpCache.put(key, otp);
        return otp;
    }

    @Override
    public int getOTP(@NonNull String key) {
        try {
            return otpCache.get(key);
        } catch (Exception e) {
            return 0;
        }
    }

    @Override
    public void clearOTP(@NonNull String key) {
        otpCache.invalidate(key);
    }

    public ConfirmOTPResponseDto confirmOTP(String key, int otpValue, EType type) throws WrongOTPException, ResourceNotFoundException, ResourceAlreadyExistException {
        if (getOTP(key) == 0) {
            throw new ResourceNotFoundException("You have not generated OTP!");
        }else if (getOTP(key) != otpValue) {
            throw new WrongOTPException("Wrong OTP!");
        }
        Token newToken = Token.builder()
            .key(key)
            .type(type)
            .build();
        Token token = tokenServiceImpl.create(newToken);
        return new ConfirmOTPResponseDto(token.getToken().toString());
    }
}
