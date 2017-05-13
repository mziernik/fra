package com.servers.http;

import com.io.IOUtils;
import com.utils.collections.Params;
import com.utils.collections.TList;
import java.io.*;
import java.util.Arrays;

public class MultiPartFormData {

    public int sizeLimit = 1024 * 1024 * 10;

    public final TList<Part> parts = new TList<>();
    private byte[] boundary = null;
    private final HttpReq request;

    public class Part {

        public final Params headers = new Params().caseSensitive(false);
        public byte[] data;
        private boolean endOfHeaders = false;

        private void addHeader(String line) {
            if (line.contains(":")) {
                String name = line.substring(0, line.indexOf(":"));
                String value = line.substring(line.indexOf(":") + 1, line.length());
                headers.add(name.trim(), value.trim());
            }
            headers.add(line.trim(), null);
        }
    }

    public MultiPartFormData(HttpReq request) throws IOException {
        this.request = request;
        int contentLength = request.getContentLength();

        if (contentLength > sizeLimit)
            throw new IOException("File has exceeded size limit.");
    }

    public MultiPartFormData process() throws IOException {
        InputStream in = new BufferedInputStream(request.getRequestBody());

        byte[] bytes = IOUtils.read(in);
        Part part = null;

        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        for (int i = 0; i < bytes.length; i++) {
            byte b = bytes[i];

            boolean rn = b == 13 && i < bytes.length - 1 && bytes[i + 1] == 10;

            if (rn)
                i++;

            if (boundary == null && rn) {
                boundary = bout.toByteArray();
                bout = new ByteArrayOutputStream();
                continue;
            }
            if (boundary == null) {
                bout.write(b);
                continue;
            }

            if (part == null)
                parts.add(part = new Part());

            if (rn) {
                if (checkForBoundary(bytes, i + 1)) {
                    part.data = bout.toByteArray();
                    bout = new ByteArrayOutputStream();
                    part = null;
                    continue;
                }

                if (part.endOfHeaders) {
                    --i;
                    bout.write(b);
                    continue;
                }

                byte[] line = bout.toByteArray();

                if (line.length == 0) {
                    part.endOfHeaders = true;
                    continue;
                }

                part.addHeader(new String(line));
                bout = new ByteArrayOutputStream();
                continue;
            }

            bout.write(b);

        }
        return this;
    }

    /**
     * Sprawdza czy kolejna sekwencja bajtów jest separatorem
     *
     * @param bytes
     * @param i
     * @return
     */
    private boolean checkForBoundary(byte[] bytes, int i) {

        if (bytes.length - i < boundary.length)
            return false;

        return Arrays.equals(boundary, Arrays.copyOfRange(bytes, i, i + boundary.length));

    }

}
