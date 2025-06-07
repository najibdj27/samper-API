package com.unper.samper.model.constant;

public enum EType {
    RESET_PASSWORD("RP"),
    REGISTRATION("RG");

    private final String code;

    private EType(String code) {
        this.code = code;
    }

    public String getCode() {
        return code;
    }

    @Override
    public String toString() {
        return "code";
    }
}
