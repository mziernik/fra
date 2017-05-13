package com.html.js;

public class CenterBox {

    String message;
    int delay;

    public CenterBox(String message, int delay) {
        this.message = message;
        this.delay = delay;
    }

    @Override
    public String toString() {
        return new Call("service.centerBox", message, delay).toString();
    }
}
