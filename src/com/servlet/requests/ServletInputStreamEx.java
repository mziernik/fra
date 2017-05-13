package com.servlet.requests;

import com.servlet.MainServlet;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import com.mlogger.Log;

public class ServletInputStreamEx extends InputStream {

    public long length;
    public boolean noReturn = false;
    public boolean aborted = false;
    private InputStream input;
    private boolean closed;
    private final HttpRequest request;
    ByteArrayOutputStream inputData;

    ServletInputStreamEx(final HttpRequest request) {
        this.request = request;
        if (inputData == null)
            inputData = new ByteArrayOutputStream();
    }

    @Override
    public void close() throws IOException {
        super.close();
        try {
            closed = true;
            getInput().close();
        } catch (IOException ex) {
            if (!"ClientAbortException".equals(ex.getClass().getSimpleName()))
                throw ex;
            aborted = true;
            Log.warning(ex);
        }
    }

    private InputStream getInput() throws IOException {
        if (input == null)
            input = request.request.getInputStream();
        return input;
    }

    @Override
    public int available() throws IOException {
        return getInput().available();
    }

    @Override
    public int read() throws IOException {
        if (aborted)
            return -1;
        int val = getInput().read();

        if (length < 1024)
            inputData.write(val);

        ++length;
        return val;
    }
    /*
     // wersja dla tomcata 8
     @Override
     public boolean isFinished() {
     return closed;
     }

     @Override
     public boolean isReady() {
     return true;
     }

     @Override
     public void setReadListener(ReadListener readListener) {

     }
     */
}
