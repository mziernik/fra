package com.io;

import com.lang.LIo;
import java.io.*;

public class DynValue {

    private long value;
    private int shift;
    private Boolean negative;

    public static byte[] writeSigned(Long value) throws IOException {
        if (value == null)
            return null;
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        writeSigned(value, bout);
        return bout.toByteArray();
    }

    public static byte[] writeUnsigned(Long value) throws IOException {
        if (value == null)
            return null;
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        writeUnsigned(value, bout);
        return bout.toByteArray();
    }

    public static void writeSigned(Long value, OutputStream out) throws IOException {
        if (value == null)
            return;

        boolean negative = value < 0;
        value = value << 1;
        if (negative) {
            value = ~value; // zaneguj całą wartość
            value |= 1; // przenieś bit znaku na koniec wartości
        }
        while (value >= 0x80) {
            out.write((byte) (value | 0x80));
            value = value >> 7;
        }
        out.write((byte) (long) value);
    }

    public static void writeUnsigned(Long value, OutputStream out) throws IOException {
        if (out == null || value == null)
            return;
        if (value < 0)
            throw new IOException(LIo.VALUE_CANT_BE_LESS_THAN_ZERO.toString(value));

        while (value > 0x70) {
            out.write(((byte) (long) value | 0x80) & 0xFF);
            value = value >> 7;
        }
        out.write((byte) (long) value);
    }

    public static int readSignedInt(InputStream in) throws IOException {
        return (int) readSigned(in);
    }

    public static long readSigned(InputStream in) throws IOException {
        DynValue dValue = new DynValue();
        Long val = null;
        while (val == null)
            val = dValue.readSigned(in.read());
        return val;
    }

    public static int readUnsignedInt(InputStream in) throws IOException {
        return (int) readUnsigned(in);
    }

    public static long readUnsigned(InputStream in) throws IOException {
        long value = 0;
        int shift = 0;

        while (shift <= 7 * 9) {
            int v = in.read() & 0xFF;
            if (v == -1)
                break;
            value |= (v & 0x7f) << shift;
            shift += 7;
            if (v < 0x80)
                break;
        }
        if (value < 0)
            throw new IOException(LIo.VALUE_LESS_THAN_ZERO.toString(value));

        return value;
    }

    public Long readSigned(int v) {

        value |= (long) (v & ~0x80) << shift;
        shift += 7;

        if (negative == null)
            negative = (v & 0x01) != 0;

        if (v < 0x80) { // ostatni bajt
            value = value >> 1;
            if (Boolean.TRUE.equals(negative)) {
                value = ~value;
                value |= 0x8000000000000000l;
            }
            long val = value;
            value = 0;
            shift = 0;
            negative = null;
            return val;
        }
        return null;
    }

}
