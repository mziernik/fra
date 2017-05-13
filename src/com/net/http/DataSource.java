package com.net.http;

import java.io.IOException;
import java.io.OutputStream;

@FunctionalInterface
public interface DataSource {

    public void write(OutputStream out) throws IOException;
}
