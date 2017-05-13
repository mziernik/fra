package com.io.iobuff;

import com.mlogger.Log;

import java.io.*;

public class IOBufferInput extends InputStream {

    private byte[] rbuffer = new byte[0];
    private long rlenght = 0;
    int rpos = 0;
    private final IOBuffer iobuff;

    IOBufferInput(IOBuffer iobuff) {
        this.iobuff = iobuff;
    }

    public long getRededLength() {
        return rlenght;
    }

    @Override
    public int read() throws IOException {

        if (iobuff.isModified)
            iobuff.flush();

        if (rlenght >= iobuff.length)
            return -1;

        if (rpos >= rbuffer.length)
            readBlock();

        ++rlenght;
        return rbuffer[rpos++] & 0xFF;
    }

    @Override
    public int available() {
        return (int) (iobuff.length - rlenght);
    }

    @Override
    public void close() {
        rbuffer = new byte[0];
        synchronized (iobuff.oppenedInputs) {
            iobuff.oppenedInputs.remove(this);
        }
    }

    private void readBlock() throws IOException {

        if (iobuff.isModified)
            iobuff.flush();

        long size = iobuff.length - rlenght;
        if (size > 1024 * 100)
            size = 1024 * 100;

        rbuffer = new byte[(int) size];
        rpos = 0;

        if (iobuff.raf != null) {
            iobuff.raf.seek(rlenght);
            iobuff.raf.read(rbuffer);
            return;
        }
        byte[] arr = iobuff.memory.toByteArray();

        for (int i = 0; i < size; i++)
            rbuffer[i] = arr[(int) rlenght + i];

    }
}
