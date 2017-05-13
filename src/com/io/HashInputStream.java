package com.io;

import com.utils.hashes.Hashes;
import com.utils.hashes.Hex;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.zip.CRC32;

public class HashInputStream extends InputStream {

    private final InputStream in;
    private MessageDigest digest;
    private byte[] res;
    private long length = 0;
    private CRC32 crc;
    public boolean enabled = true;

    public HashInputStream(Hashes.Hash hash, InputStream in) {
        this.in = in;
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
    public int available() throws IOException {
        return in.available();
    }

    @Override
    public final int read() throws IOException {
        int v = in.read();
        ++length;
        if (!enabled)
            return v;
        if (crc != null && v != -1)
            crc.update((byte) (v & 0xFF));
        else if (digest != null && v != -1)
            digest.update((byte) (v & 0xFF));
        return v;
    }

    public long getLength() {
        return length;
    }

    public int getCRC32() {
        if (crc == null)
            return 0;
        return (int) crc.getValue();
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

    @Override
    public void close() throws IOException {
        in.close();
    }

    public String getAsString() {
        return Hex.toString(getAsBytes());
    }

}
