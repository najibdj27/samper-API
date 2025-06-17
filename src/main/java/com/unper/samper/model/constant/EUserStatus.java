package com.unper.samper.model.constant;

public enum EUserStatus {
    ACTIVE('A'),
    INACTIVE('I'),
    LEAVE('L');

    private final Character code;

    private EUserStatus(Character code) {
        this.code = code;
    }

    public Character getCode() {
        return code;
    }
}
