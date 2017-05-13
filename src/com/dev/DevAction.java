package com.dev;

import com.utils.Utils;
import com.utils.Is;
import java.util.HashSet;

/**
 * Akcja trybu deweloperskiego
 *
 * @author user
 */
public abstract class DevAction {

    public final String caption;  // etykieta przycisku  
    public HashSet<String> host = new HashSet<>();  // filtr hostów
    public HashSet<String> user = new HashSet<>(); // filtr użytkowników
    public double order = 0;
    public final String hash;

    public DevAction(String caption) {
        this.caption = caption;
        hash = Utils.randomId();
    }

    @Override
    public String toString() {
        return "Action " + caption;
    }

    public abstract void run() throws Exception;

}
