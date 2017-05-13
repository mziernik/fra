package com.io;

import com.utils.hashes.Hashes;
import com.utils.hashes.Hex;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.zip.CRC32;

public class HashOutputStream extends OutputStream {

    private final OutputStream out;
    private MessageDigest digest;
    private byte[] res;
    private long length = 0;
    private CRC32 crc;
    public boolean enabled = true;

    public HashOutputStream(Hashes.Hash hash, OutputStream out) {
        this.out = out;
        try {
            switch (hash) {
                case CRC32: {
                    crc = new CRC32();
                    break;
                }
                case MD5: {
                    digest = MessageDigest.getInstance("MD5");
                    break;
                }
                case SHA1: {
                    digest = MessageDigest.getInstance("SHA-1");
                    break;
                }
                case SHA256: {
                    digest = MessageDigest.getInstance("SHA-256");
                    break;
                }
            }
        } catch (NoSuchAlgorithmException ex) {
        }
    }

    @Override
    public void write(byte[] b, int off, int len) throws IOException {
        out.write(b, off, len);
        ++length;
        if (!enabled)
            return;
        if (crc != null)
            crc.update(b, off, len);
        else
            digest.update(b, off, len);
    }

    @Override
    public void write(int b) throws IOException {
        out.write(b);
        ++length;
        if (!enabled)
            return;
        byte bb = (byte) (b & 0xFF);
        if (crc != null)
            crc.update(bb);
        else
            digest.update(bb);
    }

    @Override
    public void flush() throws IOException {
        out.flush();
    }

    public long getLength() {
        return length;
    }

    public byte[] getAsBytes() {
        if (length == 0)
            return new byte[0];

        if (res == null)
            res = (crc != null
                    ? ByteBuffer.allocate(4).putInt((int) crc.getValue()).array()
                    : digest != null ? digest.digest() : new byte[0]);
        return res;
    }

    public int getCRC32() {
        if (crc == null)
            return 0;
        return (int) crc.getValue();
    }

    public String getAsString() {
        return Hex.toString(getAsBytes());
    }

    @Override
    public void close() throws IOException {
        out.close();
    }
}
