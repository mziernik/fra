package com.io;

import com.utils.Utils;
import com.utils.Is;
import java.io.*;

public class NullOutputStream extends OutputStream implements Closeable {

    public long length = 0;

    @Override
    public String toString() {
        return "Length: " + length + (length > 1000
                ? " (" + Utils.formatSize(length) + ")" : "");
    }

    @Override
    public final void write(int b) throws IOException {
        ++length;
    }
}
