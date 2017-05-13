package com.model.dataset.intf;

public enum DSColumnAlign {
    LEFT('L'),
    CENTER('C'),
    RIGHT('R');

    public final char key;

    private DSColumnAlign(char key) {
        this.key = key;
    }

}
