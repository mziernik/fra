package com.servers.websocket.framing;

import java.nio.ByteBuffer;

import com.servers.websocket.exceptions.InvalidFrameException;

public interface Framedata {

    public enum Opcode {

        CONTINUOUS,
        TEXT,
        BINARY,
        PING,
        PONG,
        CLOSING
        // more to come
    }

    public boolean isFin();

    public boolean getTransfereMasked();

    public Opcode getOpcode();

    public ByteBuffer getPayloadData();

    public abstract void append(Framedata nextframe) throws InvalidFrameException;
}
