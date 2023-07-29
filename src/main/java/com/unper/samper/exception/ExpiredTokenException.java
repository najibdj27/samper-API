package com.unper.samper.exception;

public class ExpiredTokenException extends Throwable {
    public ExpiredTokenException(String message) {
        super(message);
    }
}
