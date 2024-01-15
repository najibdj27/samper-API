package com.unper.samper.service.impl;

import java.util.concurrent.TimeUnit;

import javax.annotation.Nonnull;

import org.springframework.stereotype.Service;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.unper.samper.service.OTPService;

import lombok.NonNull;

@Service
public class OTPServiceImpl implements OTPService{
    private static final Integer EXPIRE_MINS = 5;

    private LoadingCache<String, Integer> otpCache;

    public OTPServiceImpl() {
        super();
        otpCache = CacheBuilder.newBuilder().expireAfterWrite(EXPIRE_MINS, TimeUnit.MINUTES).build(new CacheLoader<String, Integer>() {
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
}
