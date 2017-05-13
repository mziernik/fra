package com.servlet.views;

public class ViewOptions {

    public boolean autoConnect = true; // czy klient ma się automatycznie 
    //połączyć do serwera po wczytaniu dokumentu

    private final ViewController view;

    public ViewOptions(ViewController view) {
        this.view = view;
    }

}
