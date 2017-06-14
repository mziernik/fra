package com.model.repository.intf;

public enum Align {
    LEFT('L'),
    CENTER('C'),
    RIGHT('R');

    public final char key;

    private Align(char key) {
        this.key = key;
    }

}
