package com.model.repository.intf;

public enum CaseConvert {
    UPPER('U'),
    LOWER('L'),
    CAPITALIZE('C');

    public final char key;

    private CaseConvert(char key) {
        this.key = key;
    }

}
