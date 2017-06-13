package com.model.repository;

public enum DSColumnAlign {
    LEFT('L'),
    CENTER('C'),
    RIGHT('R');

    public final char key;

    private DSColumnAlign(char key) {
        this.key = key;
    }

}
