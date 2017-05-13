package com.resources.core;

import com.exceptions.ServiceException;
import com.lang.LResource;
import com.utils.Url;
import com.utils.hashes.Hashes;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.Map;

/**
 * Klasa reprezentująca pliki znajdujące się w katalugu źródeł oraz web
 * (wszystko z rozszerzeniem innym niż .java i .class)
 *
 * @author milosz
 */
public class IdxRes {

    private final static Map<String, IdxRes> paths = new HashMap<>(); // Url, IdxRes
    private final static Map<String, IdxRes> ids = new HashMap<>(); // Url, IdxRes

    public final String path;
    public final String hash;
    public final String crc;
    public final String id;

    private final boolean reindexed;

    public IdxRes(File root, String fileName) throws IOException {
        reindexed = true;
        this.hash = Hashes.idHash6(fileName);
        this.crc = Hashes.hash(Hashes.Hash.CRC32, Files.readAllBytes(
                new File(root, fileName).toPath()));
        this.path = fileName;
        paths.put(path, this);
        id = hash + "." + crc;
        ids.put(id, this);
    }

    public IdxRes(String name) {
        reindexed = false;
        String[] split = name.split(":");
        if (split.length != 3)
            throw new ServiceException(LResource.INVALID_INDEX);
        this.hash = split[0];
        this.crc = split[1];
        this.path = split[2];

        paths.put(path, this);
        id = hash + "." + crc;
        ids.put(id, this);
    }

    public boolean isReindexed() {
        return reindexed;
    }

    public Url getUrl() {
        String s = path;
        if (s.contains("/"))
            s = s.substring(s.lastIndexOf("/") + 1);

        return new Url("/" + s).param(id);
    }

    @Override
    public String toString() {
        // Nie modyfikować! Wykorzystywane w pliku indeksu
        return hash + ":" + crc + ":" + path;
    }

    public static IdxRes getByPath(String name) {
        if (name == null)
            return null;
        if (name.startsWith("/"))
            name = name.substring(1);
        return paths.get(name);
    }

    public static IdxRes getById(String id) {
        return ids.get(id);
    }

}
