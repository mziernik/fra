package com.io;

import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.Strings;
import com.utils.collections.TList;
import java.io.*;
import java.lang.reflect.Array;
import java.nio.channels.Channels;
import java.util.LinkedList;
import java.util.List;

public class TInputStream extends InputStream implements DataInput {

    private final DataInputStream in;
    private long position;
    private long limitPos = -1;

    private final List<OutputStream> mirrors = new LinkedList<>();
    private long limit;

    @Override
    public String toString() {
        return new Strings()
                .add("Position: " + Utils.formatSize(position))
                .add(limitPos >= 0 ? "Limit: " + Utils.formatSize(limit) : null)
                .toString(", ");
    }

    public TInputStream(byte[] buffer) throws FileNotFoundException {
        this(new ByteArrayInputStream(buffer));
    }

    public TInputStream(File file, int bufferSize) throws FileNotFoundException {
        this(new BufferedInputStream(new FileInputStream(file), bufferSize));
    }

    /**
     * Ustaw limit odczytanych danych
     *
     * @param pos
     * @return
     */
    public TInputStream setLimit(long limit) {
        limitPos = limit >= 0 ? position + limit : -1;
        this.limit = limit;
        return this;
    }

    public long getLimit() {
        return limit;
    }

    public long getLimitPos() {
        return limitPos;
    }

    public TInputStream(final InputStream src) {
        this.in = new DataInputStream(new BufferedInputStream(src) {
            @Override
            public void close() throws IOException {
                super.close();
                for (OutputStream out : mirrors)
                    out.close();
            }

            @Override
            public final int read() throws IOException {
                int read = limitPos >= 0 && position >= limitPos ? -1
                        : src.read();
                if (read != -1)
                    ++position;
                for (OutputStream out : mirrors)
                    out.write(read);
                return read;
            }

            @Override
            public final int read(byte[] b) throws IOException {
                return b != null ? read(b, 0, b.length) : -1;
            }

            @Override
            public final int read(byte[] b, int off, int len) throws IOException {
                int prevLen = len;
                if (limitPos >= 0 && position + len > limitPos)
                    len = (int) (limitPos - position);

                if (len == 0 && prevLen > 0)
                    return -1; // koniec strumienia

                int read = src.read(b, off, len);
                if (read > 0) {
                    position += read;
                    for (OutputStream out : mirrors)
                        out.write(b, off, read);
                }
                return read;
            }

            @Override
            public final synchronized int available() throws IOException {
                int avail = in.available();

                if (limitPos >= 0 && position + avail > limitPos)
                    avail = (int) (limitPos - position);

                return avail;
            }

            @Override
            public final long skip(long n) throws IOException {
                if (n > 0) {
                    position += n;
                    return src.skip(n);
                }
                return 0;
            }

        });
    }

    public long getReadedBytesCount() {
        return position;
    }

    public TInputStream addMirror(OutputStream out) {
        if (out != null)
            mirrors.add(out);
        return this;
    }

    public TInputStream removeMirror(OutputStream out) {
        if (out != null)
            mirrors.remove(out);
        return this;
    }

    public TInputStream(RandomAccessFile raf) {
        this(Channels.newInputStream(raf.getChannel()));
    }

    @Override
    public void readFully(byte[] b) throws IOException {
        in.readFully(b);
    }

    @Override
    public void readFully(byte[] b, int off, int len) throws IOException {
        in.readFully(b, off, len);
    }

    @Override
    public boolean readBoolean() throws IOException {
        return in.readBoolean();
    }

    @Override
    public byte readByte() throws IOException {
        return in.readByte();
    }

    @Override
    public int readUnsignedByte() throws IOException {
        return in.readUnsignedByte();
    }

    @Override
    public short readShort() throws IOException {
        return in.readShort();
    }

    @Override
    public int readUnsignedShort() throws IOException {
        return in.readUnsignedShort();
    }

    @Override
    public char readChar() throws IOException {
        return in.readChar();
    }

    @Override
    public int readInt() throws IOException {
        return in.readInt();
    }

    @Override
    public long readLong() throws IOException {
        return in.readLong();
    }

    @Override
    public float readFloat() throws IOException {
        return in.readFloat();
    }

    @Override
    public double readDouble() throws IOException {
        return in.readDouble();
    }

    @Override
    public String readLine() throws IOException {
        return in.readLine();
    }

    @Override
    public String readUTF() throws IOException {
        return in.readUTF();
    }

    public String readStringDyn() throws IOException {
        int count = readUnsignedDynInt();
        byte[] data = read(count);
        return new String(data, Utils.UTF8);
    }

    public int readUnsignedDynInt() throws IOException {
        return (int) DynValue.readUnsigned(this);
    }

    public int readSignedDynInt() throws IOException {
        return (int) DynValue.readSigned(this);
    }

    public long readUnsignedDyn() throws IOException {
        return DynValue.readUnsigned(this);
    }

    public long readSignedDyn() throws IOException {
        return DynValue.readSigned(this);
    }

    @Override
    public int available() throws IOException {
        return in.available();
    }

    @Override
    public void close() throws IOException {
        in.close();
    }

    @Override
    public int read() throws IOException {
        return in.read();
    }

    public byte[] read(int count) throws IOException {
        byte[] buff = new byte[count];
        read(buff);
        return buff;
    }

    @Override
    public int read(byte[] b) throws IOException {
        return in.read(b);
    }

    @Override
    public int read(byte[] b, int off, int len) throws IOException {
        return in.read(b, off, len);
    }

    @Override
    public synchronized void reset() throws IOException {
        in.reset();
    }

    @Override
    public synchronized void mark(int readlimit) {
        in.mark(readlimit);
    }

    @Override
    public boolean markSupported() {
        return in.markSupported();
    }

    @Override
    public long skip(long n) throws IOException {
        return in.skip(n);
    }

    @Override
    public int skipBytes(int n) throws IOException {
        return in.skipBytes(n);
    }

    /**
     * Skocz do danej pozycji
     *
     * @param position
     * @return wartość ujemna oznacza, że nie udało się wykonać skoku
     * @throws IOException
     */
    public long skipTo(long position) throws IOException {
        long seek = position - this.position;
        if (seek > 0)
            return skipBytes((int) seek);
        return seek;
    }

    public long position() {
        return position;
    }

    /**
     * Zamienia reprezentację liczbową na wartości enumeraty na podstawie
     * indeksu (pole ordinal)
     *
     * @param <E>
     * @param cls
     * @param flags
     * @return
     */
    public static <E extends Enum> TList<E> flags(Class<E> cls, int flags) {
        TList<E> result = new TList<>();

        for (E item : cls.getEnumConstants())
            if (((int) Math.pow(2, item.ordinal()) & flags) != 0)
                result.add(item);
        return result;
    }
}
