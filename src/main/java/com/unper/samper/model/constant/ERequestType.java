package com.unper.samper.model.constant;

public enum ERequestType {
    LATE_RECORD("1"),
    PERMIT("2"),
    RESCHEDULE("3");

    private final String code;

    private ERequestType(String code) {
        this.code = code;
    }

    public String getCode() {
        return code;
    }

    @Override
    public String toString() {
        return code.toString();
    }

}
