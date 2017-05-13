package com.resources.core;

import com.config.CContent;
import com.context.AppContext;
import com.dev.Dev;
import com.exceptions.ServiceException;
import com.utils.Utils;
import com.utils.Is;
import com.io.IOUtils;
import com.io.TInputStream;
import com.mlogger.Log;
import com.servers.WebAppServer;
import com.servlet.Handlers;
import com.utils.Path;
import com.utils.Str;
import com.utils.collections.Pair;
import com.utils.collections.Pairs;
import java.io.*;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.*;

/**
 * @author Miłosz Ziernik
 * @date 15 października 2015
 * @encoding UTF-8
 */
public class ResData implements Iterable<Pair<URL, ResSourceType>> {

    // public final IdxRes idxRes;
    String path;
    String contentDisposition;
    public final IdxRes idxRes;
    byte[] data;
    public final Map<String, Object> properties = new LinkedHashMap<>();
    public final Class<?> referenceClass;
    protected int accessCount;
    protected long lastAccessed;
    final Pairs<URL, ResSourceType> urls = new Pairs<>();
    public String eTag = com.cache.CachedData.generateETag();
    private Integer size;
    private Long lastModified;
    private File file;
    boolean isIndexHtml;

    /**
     *
     * @param referenceClass
     * @param path może być również identyfikatorem zasobu (xxxxx.xxxxx)
     * @throws IOException
     */
    public ResData(Class<?> referenceClass, String path) {

        String[] split = path.split("\\.");
        IdxRes idxRes = null;

        try {
            // przykład id: uNC77w.2018001D
            if (split.length == 2
                    && split[0].length() == 6
                    && split[1].length() == 8) {

                idxRes = IdxRes.getById(path);
                if (idxRes == null)
                    throw new FileNotFoundException(path);

                path = idxRes.path;
            }

            synchronized (Resources.mapping) {
                URL url = Resources.mapping.get(path);
                if (url != null)
                    urls.add(url, ResSourceType.CLASSES);
            }

            if (idxRes == null)
                idxRes = IdxRes.getById(path);

            if (idxRes != null) {
                path = idxRes.path;
                if (!path.startsWith("/"))
                    path = "/" + path;
            }

            this.contentDisposition = path;

            if (!AppContext.sourcesPath.isEmpty())
                addFile(AppContext.sourcesPath.getFile(path), ResSourceType.SOURCES);

            for (Path p : Dev.sources)
                addFile(p.getFile(path), ResSourceType.SOURCES);

            // ----------------- usluga --------------------------------------------
            if (Package.getPackage(path.substring(1).replace("/", ".")) == null) {
                Class<?> cls = referenceClass != null
                        ? referenceClass
                        : AppContext.isInitialized()
                        ? AppContext.instance().getClass()
                        : AppContext.class;

                URL url = cls.getResource(path);
                boolean isJar = url != null && url.toString().toLowerCase().contains(".jar!");

                if (isJar)
                    urls.add(url, ResSourceType.JAR);
                else if (url != null)
                    addFile(new File(url.toURI()), ResSourceType.CLASSES);

                Enumeration<URL> resources = cls.getClassLoader().getResources(path);

                while (resources.hasMoreElements()) {
                    url = resources.nextElement();
                    isJar = url != null && url.toString().toLowerCase().contains(".jar!");

                    if (isJar)
                        urls.add(url, ResSourceType.JAR);
                    else if (url != null)
                        addFile(new File(url.toURI()), ResSourceType.CLASSES);
                }
            }

            if (referenceClass == null) {

                if (!AppContext.webPath.isEmpty())
                    addFile(AppContext.webPath.getFile(path), ResSourceType.EXTERNAL);

                if (urls.isEmpty())
                    addFile(new File(path), ResSourceType.EXTERNAL);

                if (path.startsWith("/") && WebAppServer.context != null) {
                    URL url = WebAppServer.context.getResource(path);
                    boolean isJar = url != null && url.toString().toLowerCase().contains(".jar!");
                    if (isJar)
                        urls.add(url, ResSourceType.JAR);
                    else if (url != null)
                        addFile(new File(url.toURI()), ResSourceType.CLASSES);

                }
            }

        } catch (Error | RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new ServiceException(e);
        }

        this.path = path;
        this.idxRes = idxRes;
        this.referenceClass = referenceClass;
    }

    private boolean addFile(File file, ResSourceType type) {

        if (file == null || !file.exists())
            return false;

        try {
            if (file.isFile()) {
                urls.add(file.toURI().toURL(), type);
                return true;
            }

            if (file.isDirectory() && CContent.searchIndexHtml.value()) {
                File f = new File(file, "index.html");
                if (f.exists() && f.isFile()) {
                    isIndexHtml = true;
                    contentDisposition += "index.html";
                    urls.add(f.toURI().toURL(), type);
                    return true;
                }
            }

        } catch (Throwable e) {
            Log.warning(e);
        }
        return false;
    }

    public URL getSourceURL() {
        Pair<URL, ResSourceType> res = urls.iterator().next();
        return res != null ? res.first : null;
    }

    public ResSourceType getSourceType() {
        Pair<URL, ResSourceType> res = urls.iterator().next();
        return res != null ? res.second : null;
    }

    public String getFileName() {
        return new File(contentDisposition).getName();
    }

    public InputStream getInputStream() throws IOException {

        Resources resources = Handlers.resources.getInstance();

        synchronized (this) {
            ++accessCount;
            lastAccessed = System.currentTimeMillis();

            if (data != null)
                return new ByteArrayInputStream(data);

            Pair<URL, ResSourceType> res = urls.last();

            if (res.second != ResSourceType.JAR && "file".equalsIgnoreCase(res.first.getProtocol()))
                try {
                    file = new File(res.first.toURI());
                    lastModified = file.lastModified();
                } catch (URISyntaxException ex) {
                    throw new IOException(ex);
                }

            InputStream in = res.first.openStream();
            this.size = in.available();;

            return new TInputStream(in).addMirror(new ByteArrayOutputStream() {

                @Override
                public void write(int b) {
                    if (size() <= resources.maxCachedFileSize)
                        super.write(b);
                }

                @Override
                public void write(byte[] b) throws IOException {
                    if (size() <= resources.maxCachedFileSize)
                        super.write(b);

                }

                @Override
                public void write(byte[] b, int off, int len) {
                    if (size() <= resources.maxCachedFileSize)
                        super.write(b, off, len);
                }

                @Override
                public void close() throws IOException {
                    if (size() == size)
                        resources.addToCache(ResData.this, toByteArray());
                }
            });
        }
    }

    public byte[] getData() throws IOException {
        if (data != null)
            return data;
        InputStream in = getInputStream();
        if (in == null)
            return null;
        try {
            return IOUtils.read(in);
        } finally {
            in.close();
        }
    }

    public File getFile() {
        return file;
    }

    public String getStringUtf8() throws IOException {
        return Str.decode(getData(), Utils.UTF8);
    }

    public boolean isFileChanged() {
        synchronized (this) {
            if (file == null
                    || lastModified == null
                    || size == null
                    || getSourceType() == ResSourceType.JAR)
                return false;

            boolean changed = file.lastModified() != lastModified || file.length() != size;

            // wyczyść zawartość jeśli uległa zmianie
            if (changed)
                data = null;

            return changed;
        }
    }

    @Override
    public String toString() {
        return "Resource: " + path + ", ETag: " + eTag + ", accessed: " + accessCount
                + (data != null ? ", cached: " + Utils.formatSize(data.length) : "");
    }

    public boolean isCached() {
        return data != null;
    }

    public int getAccessCount() {
        return accessCount;
    }

    public long getLastAccess() {
        return lastAccessed;
    }

    public Integer getSize() {
        return size;
    }

    @Override
    public Iterator<Pair<URL, ResSourceType>> iterator() {
        return urls.iterator();
    }

    public String getPath() {
        return path;
    }

}
