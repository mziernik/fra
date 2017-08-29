package com.resources.core;

import com.cache.CachedData;
import com.utils.Path;
import com.utils.Utils;
import com.utils.Is;
import com.context.AppContext;
import com.config.CContent;
import com.config.CResources;
import com.context.fra.Framework;
import com.context.unit_test.FraUnitTestContext;
import com.html.core.Scss;
import com.io.IOUtils;
import com.mlogger.Log;
import com.servlet.Handlers;
import com.servlet.interfaces.HttpMethod;
import com.servlet.requests.HttpRequest;
import com.servlet.requests.ServletInputStreamEx;
import com.utils.collections.Pair;
import com.utils.hashes.Hashes;
import java.io.*;
import java.net.*;
import java.util.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class Resources {

    protected int cahceExpireTime = 24 * 60 * 60;
    protected int maxCachedFileSize = 1024 * 1024;
    protected int maxCachedFilesCount = 300;
    protected int maxCachedFilesTotalSize = 100 * 1024 * 1024;
    protected int expireTime = 60 * 60 * 1000; // 1 godz

    public static Resources instance() {
        return Handlers.resources.getInstance();
    }

    public static byte[] getData(Class<?> referenceClass, String path) throws IOException {
        ResData res = get(referenceClass, path);
        return res != null ? res.getData() : null;
    }

    public static byte[] getDataF(Class<?> referenceClass, String path) throws IOException {
        return getF(referenceClass, path, null).getData();
    }

    public static String getStrF(Class<?> referenceClass, String path) throws IOException {
        return new String(getDataF(referenceClass, path), Utils.UTF8);
    }

    public static String getStr(Class<?> referenceClass, String path) throws IOException {
        byte[] data = getData(referenceClass, path);
        return data != null ? new String(data, Utils.UTF8) : null;
    }

    protected boolean returnCachedData(HttpRequest request, CachedData data) throws IOException {
        request.contentDisposition.inline = Utils.coalesce(data.inline, Boolean.FALSE);
        request.returnFile(data);
        return true;
    }

    public boolean processRequest(HttpRequest http) throws Exception {

        String path = http.relativePath;

        String id = http.getQueryString();

        if (id != null) {
            String[] split = id.split("\\.");
            if (split.length != 2
                    || split[0].length() != 6
                    || split[1].length() != 8)
                id = null;
        }

        if (path.endsWith("/"))
            path = path.substring(0, path.length() - 1);

        if (path.isEmpty())
            return false;

        CachedData cd = CachedData.get(path);

        if (cd != null) {

            if (http.method == HttpMethod.OPTIONS) {
                http.addCorsHeaders();
                return true;
            }
            
            // upload pliku CacheData
            if (http.method == HttpMethod.POST && cd.uploadable && http.contentLength() > 0) {

                try (ServletInputStreamEx in = http.getInputStream()) {
                    cd.reset();
                    http.addCorsHeaders();
                    IOUtils.copy(in, cd);
                    cd.flush();
                }

                return true;
            }

            if (returnCachedData(http, cd))
                return true;

        }
        ResData res = getResource(null, id, path, null);

        if (res == null)
            return false;

        if (res.isIndexHtml && http.relativePath.length() > 1 && !http.relativePath.endsWith("/")) {
            http.redirect(http.toAbsoluteUrl(true));
            return true;
        }

        http.contentDisposition.inline = true;
        http.setLogsEnabled(AppContext.devMode);

        returnFile(http, res);

        return true;
    }

    public static ResData get(String path) {
        return get(null, path, null);
    }

    public static ResData get(Class<?> referenceClass, String path) {
        return get(referenceClass, path, null);
    }

    public static ResData get(String path, Boolean canBeCached) {
        return get(null, path, canBeCached);
    }

    public static ResData get(Class<?> referenceClass, String path, Boolean canBeCached) {
        try {
            return instance().getResource(referenceClass, null, path, canBeCached);
        } catch (IOException ex) {
            Log.warning(ex);
            return null;
        }
    }

    public static ResData getF(String path, Boolean canBeCached) throws IOException {
        return getF(null, path, canBeCached);
    }

    public static ResData getF(Class<?> referenceClass, String path, Boolean canBeCached) throws IOException {
        ResData res = instance().getResource(referenceClass, null, path, canBeCached);
        if (res == null)
            throw new FileNotFoundException(path);
        return res;
    }

    public static URL getUrl(Class<?> referenceClass, String path) {
        Pair<URL, ResSourceType> pair = new ResData(referenceClass, path).urls.first();
        return pair != null ? pair.first : null;
    }

    /**
     *
     * @param referenceClass
     * @param path Może być w formacie url: plik.js?id
     * @param canBeCached
     * @return
     * @throws IOException
     */
    protected ResData getResource(Class<?> referenceClass, String id, String path, Boolean canBeCached) throws IOException {

        if (Is.empty(path) || path.endsWith("/"))
            throw new FileNotFoundException();

        ResData res = null;

        if (canBeCached == null ? CResources.useCache.value() : canBeCached)
            synchronized (all) {

                for (ResData r : all)
                    if (((r.idxRes != null && r.idxRes.id.equals(id))
                            || (id == null && r.path.equals(path)))
                            && (referenceClass == null || referenceClass == r.referenceClass)) {
                        res = r;
                        break;
                    }

                if (res != null && CResources.checkFileChanges.value() && res.isFileChanged()) {
                    Log.debug("Resource", "Zmodyfikowano zasób " + res.getFileName())
                            .comment(Utils.formatSize(res.getSize()))
                            .attribute("Type", res.getSourceType())
                            .attribute("File", res.getSourceURL());

                    res.data = null;
                }

            }

        if (res != null)
            return res;

        res = new ResData(referenceClass, id != null ? id : path);

        if (res.urls.isEmpty() && path.toLowerCase().endsWith(".css")) {
            String p = path.substring(0, path.length() - ".css".length()) + ".scss";
            res = new ResData(referenceClass, id != null ? id : p) {
                @Override
                public InputStream getInputStream() throws IOException {
                    String data;
                    try (InputStream in = super.getInputStream()) {
                        data = IOUtils.readUtf(in);
                    }
                    data = Scss.process(path, data, false);
                    byte[] buff = data.getBytes(Utils.UTF8);
                    addToCache(this, buff);
                    return new ByteArrayInputStream(buff);

                }
            };

            res.path = path;
        }

        if (res.urls.isEmpty())
            return null;

        if (!Boolean.FALSE.equals(canBeCached))
            synchronized (all) {

                all.add(res);
            }

        return res;
    }

    protected void returnFile(HttpRequest http, ResData item) throws IOException {
        http.contentDisposition.inline = true;
        http.setHeader("ETag", item.eTag);
        try (InputStream in = item.getInputStream()) {
            http.returnFile(in, item.getFileName());
        }
    }

    public boolean processETag(HttpServletRequest req, HttpServletResponse resp, String eTag) {
        if (!CContent.eTags.value(false))
            return false;

        for (ResData res : getList())
            if (eTag.equals(res.eTag) && !res.isFileChanged()) {
                resp.setStatus(304); // brak zmian  

                if (AppContext.devMode)
                    Log.event("Request|ETag|NoChange", req.getRequestURL()
                            + " -> " + res.getFileName());

                return true;
            }

        return false;
    }

    public static LinkedList<ResData> getList() {
        synchronized (all) {
            return Utils.asList(all);
        }
    }

    protected void addToCache(ResData res, byte[] data) {
        res.data = data;
        clenup();
    }

    private final static long CREATE_TIME = System.currentTimeMillis();

    public static void clenup() {

        int totalSize = 0;
        LinkedList<ResData> list = new LinkedList<>();
        for (ResData res : getList())
            if (res.data != null) {
                totalSize += res.data.length;
                list.add(res);
            }

        if (list.isEmpty() || totalSize == 0)
            return;

        Collections.sort(list, (ResData r1, ResData r2) -> {
            long a1 = r1.accessCount * (r1.lastAccessed - CREATE_TIME);
            long a2 = r2.accessCount * (r2.lastAccessed - CREATE_TIME);
            return (int) (a1 - a2);
        });

        //ToDo: Dopisać czyszczenie
/*
         while (list.size() > instance.maxCachedFilesCount)
         list.pollFirst().data = null;

         while (totalSize > instance.maxCachedFileSize) {
         ResData res = list.pollFirst();
         totalSize -= res.data.length;
         res.data = null;
         } */
    }

    private final static LinkedList<ResData> all = new LinkedList<>();

    final static Map<String, URL> mapping = new HashMap<>();

    public static String addMapping(URL resource) {
        String hash = Hashes.idHash6(resource.toString()) + "."
                + new Path(resource.getFile()).getFileExt();
        addMapping(hash, resource);
        return hash;
    }

    public static void addMapping(String hash, URL resource) {
        synchronized (mapping) {
            mapping.put(hash, resource);
        }
    }

    public boolean canIndex(File classPath, String fName) {
        return !fName.startsWith("META-INF/") && !fName.startsWith(".");
    }

    // zwraca ścieżkę katalogu META-INF w zalezności czy uruchomiony jest framework czy plikacja na nim bazująca 
    public static String getMetaInfDir() {
        return "/META-INF/" + (Framework.isRunning() || FraUnitTestContext.isRunning() ? "fra/" : "");
    }

}
