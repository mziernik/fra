package com.io;

import com.utils.Utils;
import com.utils.Is;
import com.utils.Str;
import java.io.*;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.zip.DeflaterOutputStream;
import java.util.zip.InflaterInputStream;

/**
 *
 * @author milosz
 */
public class IOUtils {

    private final static int defBuffSize = 102400;
    //------------------------------------------------

    public static byte[] copy(InputStream in, boolean closeStreams,
            long maxLength, int buffSize) throws IOException {
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        copy(in, bout, closeStreams, maxLength, buffSize);
        return bout.toByteArray();
    }

    public static long copy(File file, OutputStream out) throws IOException {
        try (BufferedInputStream in = new BufferedInputStream(
                new FileInputStream(file), defBuffSize);) {
            return copy(in, out, -1, false);
        }
    }

    public static long copy(InputStream in, File outFile) throws IOException {
        createFileDir(outFile);
        try (BufferedOutputStream out = new BufferedOutputStream(
                new FileOutputStream(outFile), defBuffSize);) {
            return copy(in, out, -1, false);
        }
    }

    private static void createFileDir(File file) {
        if (file == null)
            return;
        File parent = file.getParentFile();
        if (parent != null
                && !parent.getPath().equals("")
                && !parent.getPath().equals("/"))
            parent.mkdirs();
    }

    public static long copy(File inFile, File outFile) throws IOException {
        createFileDir(outFile);
        try (BufferedInputStream in = new BufferedInputStream(
                new FileInputStream(inFile), defBuffSize);
                BufferedOutputStream out = new BufferedOutputStream(
                        new FileOutputStream(outFile), defBuffSize);) {
            return copy(in, out, -1, false);
        }
    }

    public static long copy(InputStream in, OutputStream out) throws IOException {
        return copy(in, out, false, -1, defBuffSize);
    }

    public static long copy(InputStream in, OutputStream out,
            boolean closeStreams) throws IOException {
        return copy(in, out, closeStreams, -1, defBuffSize);
    }

    public static long copy(InputStream in, OutputStream out, long maxLength,
            boolean closeStreams) throws IOException {
        return copy(in, out, closeStreams, maxLength, defBuffSize);
    }

    public static long copy(InputStream in, OutputStream out,
            boolean closeStreams, long maxLength, int buffSize) throws IOException {
        if (in == null || out == null)
            return -1;

        long total = 0;
        byte[] buff = new byte[maxLength > 0 && maxLength < buffSize
                ? (int) maxLength : buffSize];
        try {
            while (true) {
                int max = buff.length;
                if (maxLength > 0 && total + max > maxLength)
                    max = (int) (maxLength - total);
                int len = in.read(buff, 0, max);
                if (len <= 0)
                    break;
                out.write(buff, 0, len);
                total += len;
                if (maxLength > 0 && total >= maxLength)
                    break;
            }
            return total;
        } finally {
            out.flush();
            if (closeStreams) {
                in.close();
                out.close();
            }
        }
    }

    public static long compress(InputStream in, OutputStream out) throws IOException {
        try (OutputStream dOut = new DeflaterOutputStream(out)) {
            return copy(in, dOut);
        }
    }

    public static long compress(byte[] in, OutputStream out) throws IOException {
        try (OutputStream dOut = new DeflaterOutputStream(out)) {
            dOut.write(in);
        }
        return in.length;
    }

    public static byte[] compress(byte[] in) throws IOException {
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        try (OutputStream dOut = new DeflaterOutputStream(bout)) {
            dOut.write(in);
        }
        return bout.toByteArray();
    }

    public static long decompress(InputStream in, OutputStream out) throws IOException {
        try (InflaterInputStream dIn = new InflaterInputStream(in)) {
            return copy(dIn, out);
        }
    }

    public static byte[] decompress(InputStream in) throws IOException {
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        decompress(in, bout);
        return bout.toByteArray();
    }

    public static long decompress(byte[] data, OutputStream out) throws IOException {
        return decompress(new ByteArrayInputStream(data), out);
    }

    public static byte[] decompress(byte[] data) throws IOException {
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        decompress(new ByteArrayInputStream(data), bout);
        return bout.toByteArray();
    }

    public static void append(String fileName, String text) throws IOException {
        if (text != null && fileName != null)
            append(fileName, text.getBytes(Charset.forName("UTF-8")));
    }

    public static void append(String file, byte[] data) throws IOException {
        append(new File(file), data);
    }

    public static void append(File file, byte[] data) throws IOException {
        if (file != null && data != null)
            try (FileOutputStream fos = new FileOutputStream(file, true);) {
                fos.write(data);
            }
    }

    public static byte[] read(File file) throws IOException {
        try (BufferedInputStream in = new BufferedInputStream(
                new FileInputStream(file), defBuffSize);) {
            return copy(in, false, -1, defBuffSize);
        }
    }

    public static byte[] read(InputStream in) throws IOException {
        return copy(in, false, -1, defBuffSize);
    }

    public static byte[] read(URL url) throws IOException {
        if (url == null)
            return null;
        try (InputStream in = url.openStream()) {
            return read(in);
        }
    }

    public static String readUtf(URL url) throws IOException {
        return read(url, Utils.UTF8);
    }

    public static String readUtf(InputStream in) throws IOException {
        return read(in, Utils.UTF8);
    }

    public static String read(InputStream in, Charset charset) throws IOException {
        byte[] buff = read(in);
        return buff != null ? new String(buff, charset) : null;
    }

    public static String read(URL url, Charset charset) throws IOException {
        byte[] buff = read(url);
        return Str.decode(buff, charset);
    }

    public static String read(File file, Charset charset) throws IOException {
        byte[] buff = IOUtils.read(file);
        return buff != null ? Str.decode(buff, charset) : null;
    }

    public static void write(String value, File file, Charset charset) throws IOException {

        file.getParentFile().mkdirs();

        try (BufferedWriter writer = new BufferedWriter(
                new OutputStreamWriter(
                        new FileOutputStream(file), charset))) {
            writer.append(value);
        }
    }

    public static void write(InputStream in, File file) throws IOException {
        file.getParentFile().mkdirs();
        copy(in, file);
    }

    public static void write(byte[] data, File file) throws IOException {
        file.getParentFile().mkdirs();
        copy(new ByteArrayInputStream(data), file);
    }

    /**
     * Usuń katalog wraz z podkatalogami
     */
    public static boolean deleteDir(File dir, boolean excludeRoot) {
        if (dir.isDirectory()) {
            String[] children = dir.list();
            if (children != null)
                for (int i = 0; i < children.length; i++) {
                    boolean success = deleteDir(new File(dir, children[i]), false);
                    if (!success)
                        return false;
                }
        }

        if (excludeRoot)
            return true;
        return dir.delete();
    }

}
