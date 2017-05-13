package com.servers.websocket.exceptions;

import com.servers.websocket.framing.CloseFrame;

public class LimitExedeedException extends InvalidDataException {

    /**
     * Serializable
     */
    private static final long serialVersionUID = 6908339749836826785L;

    public LimitExedeedException() {
        super(CloseFrame.TOOBIG);
    }

    public LimitExedeedException(String s) {
        super(CloseFrame.TOOBIG, s);
    }

}
