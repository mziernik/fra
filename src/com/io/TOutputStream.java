package com.io;

import com.utils.Utils;
import com.utils.Is;
import com.mlogger.Log;
import com.utils.hashes.Hashes.Hash;
import java.io.*;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.*;
import java.util.zip.*;
import javax.crypto.*;

public class TOutputStream extends OutputStream implements Closeable, Flushable {

    protected long length = 0;
    protected long outLength = 0;
    private boolean inverseByteOrder = false;
    private long startTime;
    private long endTime;
    private final ProxyOutput pOut;
    private OutputStream currentOutput;
    boolean closed;

    //-----------------------------------
    private Deflater deflater;
    private Inflater inflater;
    private Cipher cipher;
    private Hash inputHash;
    private Hash outputHash;
    private int bufferSize;
    private ByteArrayOutputStream memory;

    public TOutputStream(final OutputStream... mirrors) {
        this(false, mirrors);
    }

    public TOutputStream(File file) throws FileNotFoundException {
        this(false, new BufferedOutputStream(new FileOutputStream(file), 1024 * 100));
    }

    public TOutputStream(boolean memory, final OutputStream... mirrors) {
        List<OutputStream> outs = new LinkedList<>();
        if (memory) {
            this.memory = new ByteArrayOutputStream();
            outs.add(this.memory);
        }
        if (mirrors != null)
            outs.addAll(Arrays.asList(mirrors));

        pOut = new ProxyOutput(outs.toArray(new OutputStream[outs.size()]));
        currentOutput = pOut;
    }

    @Override
    public String toString() {
        return "Length: " + Utils.formatSize(length)
                + (outLength != length ? " / " + Utils.formatSize(outLength) : "" + "");
    }

    public byte[] memory() {
        return memory != null ? memory.toByteArray() : null;
    }

    public boolean isEmpty() {
        return length == 0;
    }

    public TOutputStream inputHash(Hash hash) {
        this.inputHash = hash;
        reorganizeOutputs();
        return this;
    }

    public TOutputStream bufferSize(int bufferSize) {
        this.bufferSize = bufferSize;
        reorganizeOutputs();
        return this;
    }

    public TOutputStream outputHash(Hash hash) {
        this.outputHash = hash;
        reorganizeOutputs();
        return this;
    }

    public TOutputStream cipher(Cipher cipher) {
        this.cipher = cipher;
        reorganizeOutputs();
        return this;
    }

    public TOutputStream compreesion(Deflater deflater) {
        this.inflater = null;
        this.deflater = deflater;
        reorganizeOutputs();
        return this;
    }

    public TOutputStream decompression(Inflater inflater) {
        this.inflater = inflater;
        this.deflater = null;
        reorganizeOutputs();
        return this;
    }

    public long speed() {
        if (endTime == 0)
            endTime = System.currentTimeMillis();
        return (long) ((double) length / ((endTime - startTime) / 1000d));
    }

    public long length() {
        return length;
    }

    public TOutputStream inverseByteOrder(boolean inverseByteOrder) {
        this.inverseByteOrder = inverseByteOrder;
        return this;
    }

    public void copy(InputStream source, int length) throws IOException {
        IOUtils.copy(source, this, length, false);
    }

    public void copy(InputStream source) throws IOException {
        IOUtils.copy(source, this);
    }

    @Override
    public void write(byte[] b, int off, int len) throws IOException {
        if (b == null || len <= 0)
            return;
        if (startTime == 0)
            startTime = System.currentTimeMillis();
        currentOutput.write(b, off, len);
        length += len;
    }

    @Override
    public void write(int b) throws IOException {
        if (startTime == 0)
            startTime = System.currentTimeMillis();
        currentOutput.write(b);
        ++length;
    }

    @Override
    public void flush() throws IOException {
        currentOutput.flush();
        endTime = System.currentTimeMillis();
    }

    public boolean isClosed() {
        return closed;
    }

    @Override
    public void close() throws IOException {
        closed = true;
        currentOutput.close();
        endTime = System.currentTimeMillis();
    }

    public TOutputStream writeByte(Byte value) throws IOException {
        if (value != null)
            write(value);
        return this;
    }

    public TOutputStream writeShort(Short value) throws IOException {
        if (value != null)
            write(ByteBuffer.allocate(2)
                    .putShort(inverseByteOrder
                            ? Short.reverseBytes(value)
                            : value).array());
        return this;
    }

    public TOutputStream writeInt(Integer value) throws IOException {
        if (value != null)
            write(ByteBuffer.allocate(4)
                    .putInt(inverseByteOrder
                            ? Integer.reverseBytes(value)
                            : value).array());
        return this;
    }

    public TOutputStream writeLong(Long value) throws IOException {
        if (value != null)
            write(ByteBuffer.allocate(8)
                    .putLong(inverseByteOrder
                            ? Long.reverseBytes(value)
                            : value).array());
        return this;
    }

    public TOutputStream writeFloat(Float value) throws IOException {
        if (value != null)
            write(ByteBuffer.allocate(4).putFloat(value).array());
        return this;
    }

    public TOutputStream writeDouble(Double value) throws IOException {
        if (value != null)
            write(ByteBuffer.allocate(8).putDouble(value).array());
        return this;
    }

    public TOutputStream writeString(String value, boolean includeLength) throws IOException {
        return writeString(value, Charset.forName("UTF-8"), includeLength);
    }

    public TOutputStream writeStringDyn(String value) throws IOException {
        byte[] buff = value.getBytes(Charset.forName("UTF-8"));
        writeUnsignedDyn(buff.length);
        write(buff);
        return this;
    }

    public TOutputStream writeString(String value, Charset charset, boolean includeLength) throws IOException {
        if (value != null) {
            byte[] buff = value.getBytes(charset);
            if (includeLength)
                writeInt(buff.length);
            write(buff);
        }
        return this;
    }

    public TOutputStream writeSignedDyn(long value) throws IOException {
        DynValue.writeSigned(value, this);
        return this;
    }

    public TOutputStream writeUnsignedDyn(long value) throws IOException {
        DynValue.writeUnsigned(value, this);
        return this;
    }

    private void reorganizeOutputs() {
        currentOutput = pOut;

        if (outputHash != null)
            currentOutput = new HashOutputStream(outputHash, currentOutput);

        if (deflater != null)
            currentOutput = new DeflaterOutputStream(currentOutput, deflater);

        if (inflater != null)
            currentOutput = new InflaterOutputStream(currentOutput, inflater);

        if (cipher != null)
            currentOutput = new CipherOutputStream(currentOutput, cipher);

        if (inputHash != null)
            currentOutput = new HashOutputStream(inputHash, currentOutput);

        if (bufferSize > 0)
            currentOutput = new BufferedOutputStream(currentOutput, bufferSize);
    }

    /**
     * Zamienia wartości enumeraty na podstawie indeksu (pole ordinal) na
     * reprezentację liczbową
     *
     * @param <E>
     * @param items
     * @return
     */
    public static <E extends Enum> int flags(E... items) {
        int result = 0;
        for (E item : items)
            result += Math.pow(2, item.ordinal());
        return result;
    }

    /**
     * Resetuje (czyści) bufor pamięci
     */
    public void reset() {
        if (memory == null)
            return;

        memory.reset();
        length = 0;
        startTime = 0;
    }

    private class ProxyOutput extends OutputStream {

        private final OutputStream[] outputs;

        public ProxyOutput(final OutputStream[] outputs) {
            this.outputs = outputs;
        }

        /* public ProxyOutput(Cipher cipher, Deflater deflater, final OutputStream[] outputs) {
         this.outputs = null;
         }

         public ProxyOutput(Cipher cipher, Inflater inflater, final OutputStream[] outputs) {
         this.outputs = null;

         }
         */
        @Override
        public void write(byte[] b) throws IOException {
            for (OutputStream out : outputs)
                out.write(b);
            outLength += b.length;
        }

        @Override
        public void write(byte[] b, int off, int len) throws IOException {
            for (OutputStream out : outputs)
                out.write(b, off, len);
            outLength += len;
        }

        @Override
        public void write(int b) throws IOException {
            for (OutputStream out : outputs)
                out.write(b);
            ++outLength;
        }

        @Override
        public void flush() throws IOException {
            for (OutputStream out : outputs)
                out.flush();
        }

        @Override
        public void close() throws IOException {
            for (OutputStream out : outputs)
                out.close();
        }
    }

}
