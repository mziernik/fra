package com.net.http;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

@FunctionalInterface
public interface HttpResponseCallback {

    void onResponse(HttpResponse response, IOException exception);
}
