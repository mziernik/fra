package com.io.iobuff;

import com.utils.Utils;
import com.utils.Is;
import com.context.AppContext;
import com.io.IOUtils;
import com.io.TOutputStream;
import com.lang.LIo;
import com.utils.hashes.Hex;
import java.io.*;
import java.security.*;
import java.util.*;

public class IOBuffer extends TOutputStream {

    private int memoryBufferSizeLimit = 1024 * 1024;
    RandomAccessFile raf;
    File file;
    final ByteArrayOutputStream memory = new ByteArrayOutputStream();
    long length;
    boolean isModified = false;
    private final Set<IOBuffListener> listeners = new LinkedHashSet<>();
    final Set<IOBufferInput> oppenedInputs = new HashSet<>();
    private final BufferedOutputStream buffer;
    public Hash hash;
    private MessageDigest digest;
    protected long fileLastModified;

    public static enum Hash {

        MD5,
        SHA1,
        SHA256
    }

    public IOBuffer() throws IOException {
        this(null, 1024 * 100);
    }

    public IOBuffer(int bufferSize) throws IOException {
        this(null, bufferSize);
    }

    /**
     *
     * @param tempFileName Jesli jest null-em, to dane umieszczone będą w
     * pamięci
     * @throws IOException
     */
    public IOBuffer(String tempFileName) throws IOException {
        this(tempFileName, 1024 * 100);
    }

    public IOBuffer(String tempFileName, int bufferSize) throws IOException {
        super(false);
        buffer = new BufferedOutputStream(new BuffOut(), bufferSize);
        if (tempFileName != null)
            assignFile(createFile(tempFileName));
    }

    @Override
    public String toString() {
        return "IOBuffer, " + Utils.formatSize(length);
    }

    public void saveToFile(String file) throws IOException {
        saveToFile(new File(file));
    }

    public void saveToFile(File file) throws IOException {
        if (file == null)
            return;
        try (InputStream in = getInputStream()) {
            IOUtils.copy(in, file);
        }
    }

    public void loadFromFile(File file) throws IOException {
        if (file == null)
            return;
        length = IOUtils.copy(file, this);
        fileLastModified = file.lastModified();
        this.file = file;
        flush();
    }

    /**
     * Przypisuje istniejący plik do strumienia. Od tego momentu plik staje się
     * "własnością" klasy. Plik może być modyfikowany lub usunięty.
     *
     * @param file
     * @return
     * @throws IOException
     */
    public IOBuffer assignFile(File file) throws IOException {
        if (!file.exists())
            throw new FileNotFoundException(file.toString());
        closeOppenedInputs();

        if (raf != null)
            raf.close();
        raf = new RandomAccessFile(file, "rw");
        fileLastModified = file.lastModified();
        this.file = file;
        return this;
    }

    private byte[] bmd5;
    private boolean md5called = false;

    public byte[] getHashB() throws IOException {
        if (bmd5 == null && md5called)
            throw new IOException("Wielokrotne wywołanie generatora sumy kontrolnej");

        if (bmd5 == null) {
            bmd5 = digest.digest();
            md5called = true;
        }

        return bmd5;
    }

    public String getHashS() throws IOException {
        return digest != null ? Hex.toString(getHashB()) : null;
    }

    private void initialize() throws IOException {
        initialized = true;
        try {
            if (hash != null)
                digest = MessageDigest.getInstance(hash.name());
        } catch (NoSuchAlgorithmException ex) {
            throw new IOException(ex);
        }
    }

    public long read(InputStream in) throws IOException {
        long copy;
        try (BufferedInputStream bin = new BufferedInputStream(in)) {
            copy = IOUtils.copy(bin, this);
            flush();
        }
        return copy;
    }

    @Override
    public long length() {
        return length;
    }

    /**
     * Zwalnia dostęp do pliku. Od tego momentu dane strumienia wyjściowego będą
     * zapisywane do ramu
     *
     * @return
     * @throws IOException
     */
    public IOBuffer releaseFile() throws IOException {
        if (raf != null)
            raf.close();
        length = memory.size();
        raf = null;
        file = null;
        return this;
    }

    private boolean initialized = false;

    @Override
    public void write(byte[] buff, int off, int len) throws IOException {
        if (buff == null)
            return;

        if (!initialized)
            initialize();

        bmd5 = null; // musi byc resetowane

        for (IOBuffListener listener : listeners)
            listener.onWrite(buff);

        if (digest != null)
            digest.update(buff, off, len);

        buffer.write(buff, off, len);
        isModified = true;
    }

    @Override
    public void write(int b) throws IOException {
        if (!initialized)
            initialize();

        bmd5 = null; // musi byc resetowane

        for (IOBuffListener listener : listeners)
            listener.onWrite(b);

        if (digest != null)
            digest.update((byte) (b & 0xFF));

        buffer.write(b);
        isModified = true;
    }

    public boolean isFileChanged() {
        if (file == null)
            return false;
        return file.lastModified() != fileLastModified;
    }

    public void reloadFile() throws IOException {
        clear();
        if (raf != null)
            assignFile(file);
        else
            loadFromFile(file);
    }

    protected File createFile(String tempFileName) throws IOException {
        File tFile = AppContext.tempPath.getFile(tempFileName);
        tFile.getParentFile().mkdirs();
        tFile.createNewFile();
        return tFile;
    }

    public boolean isInMemory() {
        return raf == null;
    }

    public boolean isInFile() {
        return raf != null;
    }

    public File getFile() {
        return file;
    }

    @Override
    public void flush() throws IOException {
        if (raf != null)
            length = raf.length();
        buffer.flush();
    }

    @Override
    public void close() throws IOException {
        for (IOBuffListener listener : listeners)
            if (!listener.onClose())
                return;
        buffer.flush();
        buffer.close();
        closeOppenedInputs();
        super.close();
    }

    public IOBuffer clear() throws IOException {
        for (IOBuffListener listener : listeners)
            listener.onClear();

        buffer.flush();

        if (raf != null)
            raf.setLength(0);

        if (memory != null)
            memory.reset();

        isModified = false;
        length = 0;

        return this;
    }

    public IOBuffer delete() throws IOException {
        for (IOBuffListener listener : listeners)
            listener.onDelete();

        buffer.flush();

        if (raf != null) {
            raf.close();
            if (file != null)
                file.delete();
        }

        memory.reset();
        raf = null;
        file = null;
        isModified = false;

        return this;
    }

    public IOBufferInput getInputStream() {
        IOBufferInput in = new IOBufferInput(this);
        synchronized (oppenedInputs) {
            oppenedInputs.add(in);
        }
        return in;
    }

    public IOBuffer closeOppenedInputs() {
        for (IOBufferInput in : getOppenedInputs())
            in.close();
        return this;
    }

    public Set<IOBufferInput> getOppenedInputs() {
        Set<IOBufferInput> set = new LinkedHashSet<>();
        synchronized (oppenedInputs) {
            set.addAll(oppenedInputs);
        }
        return set;
    }

    public boolean isModified() {
        return isModified;
    }

    public byte[] getData() throws IOException {
        try (InputStream is = getInputStream()) {
            return IOUtils.read(is);
        }
    }

    public IOBuffer setMemoryBufferSizeLimit(int memoryBufferSizeLimit) {
        this.memoryBufferSizeLimit = memoryBufferSizeLimit;
        return this;
    }

    public int getMemoryBufferSizeLimit() {
        return memoryBufferSizeLimit;
    }

    static {
        Runtime.getRuntime().addShutdownHook(new Thread() {
            @Override
            public void run() {

            }
        });
    }

    public void moveToFile(String tempFileName) throws IOException {
        if (isInFile())
            return;
        assignFile(createFile(tempFileName));
        IOUtils.copy(new ByteArrayInputStream(memory.toByteArray()),
                new BufferedOutputStream(
                        new FileOutputStream(raf.getFD()), 1024 * 100), false);
        memory.reset();
    }

    private class BuffOut extends OutputStream {

        @Override
        public void write(int b) throws IOException {
            throw new UnsupportedOperationException(LIo.UNSUPPORTED_OPERATION.toString());
        }

        @Override
        public void flush() throws IOException {
            super.flush();
            if (raf != null && file != null)
                fileLastModified = file.lastModified();
        }

        @Override
        public void write(byte[] buff, int off, int len) throws IOException {
            if (buff == null || len <= 0)
                return;

            if (length + buff.length > memoryBufferSizeLimit && raf == null)
                moveToFile("iobuff_" + Utils.randomId(16));

            for (IOBuffListener listener : listeners)
                listener.onFlush(buff);

            if (raf != null)
                raf.write(buff, off, len);
            else
                memory.write(buff, off, len);

            length += (len - off);
            isModified = false;
        }

        @Override
        public void write(byte[] buff) throws IOException {
            if (buff != null)
                write(buff, 0, buff.length);
        }

    }

}
