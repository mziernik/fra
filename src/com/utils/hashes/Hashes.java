package com.utils.hashes;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.zip.CRC32;

/**
 *
 * @author Miłosz Ziernik
 */
public class Hashes {

    public static enum Hash {

        CRC32,
        MD5,
        SHA1,
        SHA256
    }

    /**
     * 6-znakowy hasz (CRC32)
     *
     * @param source
     * @return
     */
    public static String idHash6(String source) {
        return source != null ? encode(Hashes.crc32b(source.getBytes())) : null;
    }

    public static String idHash6(Integer source) {
        return source != null ? encode(Hashes.crc32b(
                ByteBuffer.allocate(4).putInt(source).array())) : null;
    }

    public static String idHash6(Long source) {
        return source != null ? encode(Hashes.crc32b(
                ByteBuffer.allocate(7).putLong(source).array())) : null;
    }

    /**
     * 12-znakowy hasz (skrócone MD5)
     *
     * @param source
     * @return
     */
    public static String idHash12(String source) {
        if (source == null)
            return null;

        byte[] buff = Hashes.hashB(Hash.MD5, source);

        byte[] data = new byte[9];
        for (int i = 0; i < 8; i++)
            data[i] = (byte) (buff[i * 2] ^ buff[i * 2 + 1]);

        int xor = 0;
        for (byte b : buff)
            xor ^= b;

        data[8] = (byte) xor;
        return encode(data);
    }

    private static String encode(byte[] data) {
        return com.utils.hashes.Base64.encode(data)
                .replace("+", "a")
                .replace("/", "b")
                .replace("=", "");
    }

    /**
     * Zwraca 8-znakowy hasz bazujący na CRC32 z wartości source. Dodawany jest
     * również jeden bajt liczony jako xor ze wszystkich znaków oraz jeden bajt
     * reprezentujący liczbę znaków
     *
     * @param source
     * @return
     */
    public static String idHash8(String source) {
        if (source == null)
            return null;

        int xor = 0;
        for (char c : source.toCharArray())
            xor ^= c;

        byte[] buff = Arrays.copyOf(Hashes.hashB(Hash.CRC32, source), 6);
        buff[4] = (byte) xor;
        buff[5] = (byte) source.length();

        return com.utils.hashes.Base64.encode(buff)
                .replace("+", "a")
                .replace("/", "b")
                .replace("=", "");
    }

    public static byte[] md5B(String str) {
        return hashB(Hash.MD5, str.getBytes());
    }

    public static byte[] md5B(byte[] data) {
        return hashB(Hash.MD5, data);
    }

    public static String md5(String str) {
        return hash(Hash.MD5, str);
    }

    public static String md5(byte[] data) {
        return hash(Hash.MD5, data);
    }

    public static String hash(Hash hash, File file) throws IOException {
        BufferedInputStream in = new BufferedInputStream(new FileInputStream(file));
        try {
            return Hex.toString(hashB(hash, in));
        } finally {
            in.close();
        }
    }

    public static byte[] hashB(Hash hash, File file) throws IOException {
        BufferedInputStream in = new BufferedInputStream(new FileInputStream(file));
        try {
            return hashB(hash, in);
        } finally {
            in.close();
        }
    }

    public static String hash(Hash hash, String str) {
        return Hex.toString(hashB(hash, str.getBytes()));
    }

    public static byte[] hashB(Hash hash, String str) {
        return hashB(hash, str.getBytes());
    }

    public static String hash(Hash hash, byte[] buff) {
        try {
            return Hex.toString(hashB(hash, new ByteArrayInputStream(buff)));
        } catch (IOException ex) {
            return "";
        }
    }

    public static byte[] hashB(Hash hash, byte[] buff) {
        try {
            return hashB(hash, new ByteArrayInputStream(buff));
        } catch (IOException ex) {
            return new byte[0];
        }
    }

    public static String hash(Hash hash, InputStream is) throws IOException {
        return Hex.toString(hashB(hash, is));
    }

    public static byte[] hashB(Hash hash, InputStream is) throws IOException {
        MessageDigest digest = null;
        CRC32 crc = null;
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

            byte[] buffer = new byte[8192];
            int read;

            if (crc != null) {
                while ((read = is.read(buffer)) > 0)
                    crc.update(buffer, 0, read);
                return ByteBuffer.allocate(4).putInt((int) crc.getValue()).array();
            }

            if (digest != null) {
                while ((read = is.read(buffer)) > 0)
                    digest.update(buffer, 0, read);
                return digest.digest();
            }

            return new byte[0];

        } catch (NoSuchAlgorithmException ex) {
            return new byte[0];
        }
    }

    public static int crc32(byte[] bytes) {
        CRC32 crc = new CRC32();
        crc.update(bytes);
        return (int) crc.getValue();
    }

    public static byte[] crc32b(byte[] bytes) {
        return ByteBuffer.allocate(4).putInt(crc32(bytes)).array();
    }
}
