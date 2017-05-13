package com.net.http;

import com.utils.Url;
import java.io.Closeable;
import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class HttpClient implements Closeable {

    int connectTimeout = -1;
    int readTimeout = -1;

    private ThreadPoolExecutor defExecutor;
    private ExecutorService userExecutor;

    public HttpRequest request(Url url) {

        return new HttpRequest(this, url, true);
    }

    ExecutorService getExecutor() {
        if (userExecutor != null)
            return userExecutor;

        if (defExecutor == null)
            defExecutor = new ThreadPoolExecutor(100, 300, 500000L, TimeUnit.MILLISECONDS,
                    new LinkedBlockingQueue<>());
        return defExecutor;
    }

    public HttpClient connectTimeout(int connectTimeout) {
        this.connectTimeout = connectTimeout;
        return this;
    }

    public HttpClient readTimeout(int readTimeout) {
        this.readTimeout = readTimeout;
        return this;
    }

    @Override
    public void close() throws IOException {
        if (defExecutor != null)
            defExecutor.shutdown();
    }

}
