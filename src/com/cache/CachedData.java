package com.cache;

import com.utils.StrUtils;
import com.utils.Path;
import com.utils.Utils;
import com.utils.Is;
import com.config.CCache;
import com.context.AppContext;
import com.resources.dict.MimeMappings;
import com.exceptions.EError;
import com.io.SearchFiles;
import com.io.iobuff.IOBuffer;
import com.io.iobuff.IOBufferInput;
import com.lang.LCache;
import com.mlogger.Log;
import com.servlet.handlers.HRequests;
import com.servlet.controller.BaseSession;
import com.servlet.requests.HttpRequest;
import com.utils.Size.SizeUnit;
import com.utils.collections.Strings;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import java.io.*;
import java.util.*;

public class CachedData extends IOBuffer {

    public static int defaultExpire = 60 * 60;

    public final String key;
    public final String source;
    public String name;
    public String type;
    public final long created = System.currentTimeMillis();
    protected long lastAccessed = System.currentTimeMillis();
    protected int accessCount;
    protected int expireSeconds;
    public final Map<String, Object> attributes = new LinkedHashMap<>();
    final Set<CachedData> children = new LinkedHashSet<>();
    public Boolean inline; //ContentDisposition
    //-----------------
    public String mimeType;
    public String url;
    public String referer;
    public String username;
    public String sourceIP;
    public String requestId;
    public String retrunName;
    public boolean deleted;
    public final Set<String> tags = new LinkedHashSet<>();
    public CachedData parent;
    public String eTag = generateETag();
    /**
     * Czy można uploadować (zapisywać) dane
     */
    public boolean uploadable = false;

    HttpRequest requestLock;
    BaseSession sessionLock;

    /**
     * Metoda uniemozliwia usunęcia zawartości z innego wątku niż tego, który
     * realizuje żądanie, Gdy żądanie się zakończy, blokada już nie obowiązuje
     *
     * @param http
     * @return
     */
    public CachedData lockRequest(HttpRequest http) {
        this.requestLock = http;
        return this;
    }

    /**
     * Metoda uniemozliwia usunęcia zawartości z innego wątku niż tego, który
     * realizuje żądanie danej sesji, Gdy sesja wygaśnie, blokada już nie
     * obowiązuje
     *
     * @param session
     * @return
     */
    public CachedData lockSession(BaseSession session) {
        this.sessionLock = session;
        return this;
    }

    public static Iterable<CachedData> getList() {
        return manager().getList();
    }

    public static CacheManager manager() {
        return CacheManager.instance;
    }

    public static CachedData get(String key) {
        return CacheManager.instance.get(key);
    }

    public static CachedData getF(String key) throws FileNotFoundException {
        CachedData item = get(key);
        if (item == null)
            throw new FileNotFoundException(key);
        return item;
    }

    public static CachedData getByName(String source, String type, String name) {
        for (CachedData cd : manager().getList())
            if (cd.source != null && cd.source.equals(source)
                    && cd.type != null && cd.type.equals(type)
                    && cd.name != null && cd.name.equals(name))
                return cd;
        return null;
    }

    public static CachedData getByAttribute(String name, Object value) {

        for (CachedData cd : manager().getList()) {
            Object attr = cd.attributes.get(name);
            if (attr != null && attr.equals(value))
                return cd;
        }
        return null;
    }

    public CachedData(String source, String type, String name, boolean saveToFile) throws IOException {
        this(source, type, name, defaultExpire, saveToFile);
    }

    public CachedData(String source, String type, String name) throws IOException {
        this(source, type, name, defaultExpire);
    }

    public CachedData(String source, String type, String name, int expireSeconds) throws IOException {
        this(source, type, name, expireSeconds, false);
    }

    public CachedData(String source, String type, String name, int expireSeconds, byte[] in) throws IOException {
        this(source, type, name, expireSeconds, in.length > CCache.maxMemorySize.getValue(null).getValue(SizeUnit.B));
        read(new ByteArrayInputStream(in));
    }

    public CachedData(String source, String type, String name, int expireSeconds, InputStream in) throws IOException {
        this(source, type, name, expireSeconds, in.available() > CCache.maxMemorySize.getValue(null).getValue(SizeUnit.B));
        read(in);
    }

    private static String generateTempName(String key, String source, String type, String name) {
        String path = "cache/";
        if (source != null)
            path += StrUtils.formatFileName(source) + "/";
        if (type != null)
            path += StrUtils.formatFileName(type) + "_";
        path += key;
        if (name != null && name.contains("."))
            path += name.substring(name.lastIndexOf(".")).trim();
        else
            path += ".tmp";
        return path;
    }

    public CachedData(String source, String type, String name, int expireSeconds, boolean saveToFile)
            throws IOException {
        this(Utils.randomId(10), source, type, name, expireSeconds, saveToFile);
    }

    private CachedData(String key, String source, String type, String name, int expireSeconds, boolean saveToFile)
            throws IOException {
        super(saveToFile ? generateTempName(key, source, type, name) : null);
        this.source = Utils.coalesce(source, "").trim();
        this.name = Utils.coalesce(name, "").trim();
        this.expireSeconds = expireSeconds;
        this.key = key;
        this.type = Utils.coalesce(type, "").trim();
        CacheManager.instance.add(this);

        retrunName = getFileName();
        mimeType = MimeMappings.get(retrunName);
        if (mimeType == null)
            mimeType = "application/octetstream";

        HttpRequest req = HttpRequest.getInstance();
        if (req != null) {
            url = req.url.toString();
            sourceIP = req.request.getRemoteHost();
            requestId = req.id;
            referer = req.getHeader("referer");
            requestLock = req;
            sessionLock = req.session;
            if (req.session != null && req.session.user != null)
                username = req.session.user.username;
        }
    }

    public void moveToFile() throws IOException {
        super.moveToFile(generateTempName(key, source, type, name));
    }

    public CachedData type(String type) {
        this.type = type;
        return this;
    }

    @Override
    public IOBufferInput getInputStream() {
        peak();
        return super.getInputStream();
    }

    @Override
    public byte[] getData() throws IOException {
        peak();
        return super.getData();
    }

    @Override
    public String toString() {
        return source + ", " + name + ", " + Utils.formatSize(length());
    }

    public CachedData attribute(String key, Object value) {
        attributes.put(key, value);
        return this;
    }

    public CachedData tag(String tag) {
        tags.add(tag);
        return this;
    }

    public long getLastAccessed() {
        return lastAccessed;
    }

    public long getLastModified() {
        return fileLastModified;
    }

    /**
     * Podbij (wyzeruj) licznik wygasania
     */
    public void peak() {
        lastAccessed = System.currentTimeMillis();
        ++accessCount;
    }

    public long getLeaveTime() {
        long diff = lastAccessed + (expireSeconds * 1000) - System.currentTimeMillis();
        if (diff < 0)
            diff = 0;
        return diff;
    }

    public int getAccessCount() {
        return accessCount;
    }

    public int getExpireTime() {
        return expireSeconds;
    }

    public void setExpireTime(int expireSeconds) {
        this.expireSeconds = expireSeconds;
        peak();
    }

    @Override
    public CachedData delete() throws IOException {

        HttpRequest current = HttpRequest.getInstance();

        if (getRequestLock(current) != null)
            throw new IOException(
                    LCache.CANT_DELETE_FILE_UNTIL_REQUEST_IS_PROCESSING.toString(key, name));

        BaseSession sLock = getSessionLock(current != null ? current.session
                : null);

        if (sLock != null)
            throw EError.addDetails(
                    EError.addDetails(new Error(LCache.CANT_DELETE_FILE_UNTIL_SESSION_IS_ACTIVE.toString(key, name, sLock.id)),
                            LCache.REMAINING_TIME.toString(), new Interval(sLock.getRemainingTime(), Unit.MILLISECONDS).toStringFrmtS()),
                    LCache.SESSION.toString(), sLock.toString());

        if (deleted)
            return this;

        deleted = true;

        for (CachedData cd : getChildren())
            cd.delete();

        sessionLock = null;

        if (!CacheManager.instance.doRemove(this))
            return this;

        if (parent != null)
            synchronized (parent.children) {
                parent.children.remove(this);
            }

        super.delete();
        return this;
    }

    public CachedData mimeType(String mimeType) {
        this.mimeType = mimeType;
        return this;
    }

    public CachedData url(String url) {
        this.url = url;
        return this;
    }

    public String getMimeType() {
        if (mimeType == null)
            mimeType = MimeMappings.get(name);
        return mimeType;
    }

    public static void clearCache() {
        for (CachedData cd : manager().getList())
            try {
                cd.delete();
            } catch (IOException e) {
                Log.warning(e);
            }
    }

    public String getName() {
        return name;
    }

    /**
     * Zwraca nazwę sformatowaną do nazwy pliku (np z postaci URI)
     *
     * @return
     */
    public String getFileName() {
        String fileName = Utils.coalesce(name, "").replace("\\", "/").trim();

        if (fileName.contains("?"))
            fileName = fileName.substring(0, fileName.indexOf("?"));

        if (fileName.endsWith("/"))
            fileName = fileName.substring(0, fileName.length() - 1);

        if (fileName.contains("/"))
            fileName = fileName.substring(fileName.lastIndexOf("/") + 1);

        return fileName;
    }

    public HttpRequest getRequestLock(HttpRequest current) {
        if (requestLock != null && requestLock != current)
            synchronized (HRequests.requests) {
                for (HttpRequest req : HRequests.requests.values())
                    if (req == requestLock)
                        return req;
                requestLock = null;
            }
        return null;
    }

    public BaseSession getSessionLock(BaseSession current) {
        if (sessionLock != null && sessionLock != current) {
            for (BaseSession ses : BaseSession.getSessions())
                if (ses == sessionLock)
                    return ses;
            sessionLock = null;

        }
        return null;
    }

    public Object getAttribute(String name) {
        return attributes.get(name);
    }

    public String getAttribute(String name, String def) {
        Object attr = attributes.get(name);
        return attr instanceof String ? (String) attr : attr != null
                ? Utils.coalesce(attr.toString(), def) : def;
    }

    public Integer getAttribute(String name, Integer def) {
        Object attr = attributes.get(name);
        return attr instanceof Integer ? (Integer) attr : attr != null
                ? Utils.strInt(attr.toString(), def) : def;
    }

    public Strings getTags() {
        return new Strings(tags);
    }

    public CachedData setName(String name) {
        this.name = name;
        return this;
    }

    @Override
    public boolean isFileChanged() {
        boolean changed = super.isFileChanged();
        if (changed)
            eTag = generateETag();
        return changed;
    }

    public CachedData addChild(String name) throws IOException {
        CachedData cd = new CachedData(source, type, name, expireSeconds);
        synchronized (children) {
            children.add(cd);
        }
        cd.parent = this;
        return cd;
    }

    public CachedData addChild(CachedData file) {
        synchronized (children) {
            children.add(file);
        }
        file.parent = this;
        return this;
    }

    public LinkedList<CachedData> getChildren() {
        LinkedList<CachedData> list = new LinkedList<>();
        synchronized (children) {
            list.addAll(children);
        }
        return list;
    }

    public static void clearTempFiles() throws IOException {
        File dir = AppContext.tempPath.getFile("cache");
        if (!dir.exists())
            return;

        Log.debug("Cache", "Czyszczenie zawartości " + dir);

        SearchFiles search = new SearchFiles(dir.getAbsolutePath(), true);

        Set<String> current = new HashSet<>();

        for (CachedData cd : getList()) {
            File file = cd.getFile();
            if (file != null)
                current.add(file.getAbsolutePath());
        }

        for (Path p : search) {
            File f = p.toFile();
            if (!current.contains(f.getAbsolutePath()))
                f.delete();
        }

        for (Path p : search.getDirs())
            p.toFile().delete();

    }

    @Override
    public void close() throws IOException {
        super.close();
    }

    public CachedData changeFileExtension(String ext) throws IOException {
        if (ext == null || getFile() == null)
            return this;
        File file = getFile();
        String _ext = new Path(file).getFileExt();
        if (ext.equals(_ext))
            return this;
        releaseFile();
        File newFile = new Path(file).changeExtension(ext).toFile();
        file.renameTo(newFile);
        assignFile(newFile);
        return this;
    }

    public static String generateETag() {
        return "E'" + Utils.randomId(10);
    }

}
