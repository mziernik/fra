package com.io.iobuff;

import java.io.IOException;

public interface IOBuffListener {

    public void onWrite(int b) throws IOException;

    public void onFlush(byte[] b) throws IOException;

    public boolean onClose() throws IOException;

    public void onDelete() throws IOException;

    public void onClear();

    public void onWrite(byte[] buff);

}
