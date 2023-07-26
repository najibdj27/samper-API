package com.unper.samper.service;

public interface OTPService {
    int generateOTP(String key);

    int getOTP(String key);

    void clearOTP(String key);
}

