package com.servlet.requests;

import com.utils.Utils;
import com.utils.Is;
import com.config.CContent;
import com.context.AppContext;
import java.io.IOException;
import java.io.OutputStream;
import com.mlogger.Log;
import com.utils.Str;
import java.io.*;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.zip.GZIPOutputStream;

public class ServletOutputStreamEx extends OutputStream {

    private OutputStream os;
    public long length;
    public boolean noReturn = false;
    public boolean aborted = false;
    public final ByteArrayOutputStream content = new ByteArrayOutputStream();// poczatek strumienia wyjsciowego (do podgladu)
    private final HttpRequest request;
    //private boolean responseLogged = false;
    private final Set<OutputStream> mirrors = new LinkedHashSet<>();

    ServletOutputStreamEx(final HttpRequest request) throws IOException {
        super();
        this.request = request;
    }

    public ServletOutputStreamEx addMirror(OutputStream out) {
        mirrors.add(out);
        return this;
    }

    @Override
    public void flush() throws IOException {

        for (OutputStream out : mirrors)
            out.flush();

        if (os == null)
            return;
        try {
            os.flush();
        } catch (IOException ex) {
            if (!"ClientAbortException".equals(ex.getClass().getSimpleName()))
                throw ex;
            Log.warning("Response", ex);
        }
    }

    @Override
    public void close() throws IOException {

        for (OutputStream out : mirrors)
            out.close();

        if (os == null)
            return;

        try {
            os.close();
        } catch (IOException ex) {
            aborted = true;
            Log.warning("Response", ex);
        }
    }

    private void checkStream() throws IOException {
        if (os != null)
            return;

        String acceptEncoding = Utils.coalesce(request.getHeader("Accept-Encoding"), "").toLowerCase();
        String ct = Utils.coalesce(request.response.getContentType(), "").toLowerCase();
        if (ct.contains(";"))
            ct = ct.substring(0, ct.indexOf(";"));

        if (request.compression == null) {

            if (!AppContext.devMode && acceptEncoding.contains("gzip"))
                for (String s : CContent.compressionMimeTypes.getValue(null))
                    if (Str.matchesMask(ct, s)) {
                        request.compression = true;
                        break;
                    }

            if (request.compression == null)
                request.compression = false;

        }

        if (request.compression) {
            request.response.setContentLength(-1);
            request.setHeader("Content-Encoding", "gzip");
            os = new GZIPOutputStream(request.response.getOutputStream());

        } else
            os = request.response.getOutputStream();

//            noReturn = request.controller != null /* && request.page.testMode */
//                    && request.getHeader(MainServlet.HttpConst.ajaxTestNoReturnFile) != null;
    }

    @Override
    public void write(int b) throws IOException {

        for (OutputStream out : mirrors)
            out.write(b);

        if (noReturn)
            return;
        checkStream();
        os.write(b);
        if (content.size() < 1024)
            content.write(b);
        ++length;
    }

    // wersja dla tomcata 8
    /* @Override
     public boolean isReady() {
     return true;
     }

     @Override
     public void setWriteListenerWriteListener writeListener) {
     }
     */
    @Override
    public void write(byte[] buff) throws IOException {

        for (OutputStream out : mirrors)
            out.write(buff);

        if (noReturn || buff == null)
            return;
        checkStream();
        os.write(buff);
        if (content.size() < 1024)
            content.write(buff);
        length += buff.length;
    }

    @Override
    public void write(byte[] buff, int off, int len) throws IOException {

        try {
            for (OutputStream out : mirrors)
                out.write(buff, off, len);

            if (noReturn || buff == null)
                return;

            checkStream();

            os.write(buff, off, len);
            if (content.size() < 1024)
                content.write(buff, off, len);

            length += (len - off);
        } catch (IOException e) {
            aborted = true;
            try {
                os.flush();
                os.close(); // spróbuj zamknąć strumień
            } catch (Throwable ex) {
            }
            throw e;
        }
    }

}
