package com.utils.collections;

public enum OverloadAction {
    /**
     * porzuć element (noe dodawaj do listy)
     */
    SKIP,
    /**
     * usuń pierwszy element z listy (przesuń dane)
     */
    SHIFT,
    /**
     * usuń ostani element z listy
     */
    POP,
    /**
     * wygeneruj błąd
     */
    ERROR;

}
