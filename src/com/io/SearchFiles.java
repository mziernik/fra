package com.io;

import java.io.*;
import java.nio.file.*;
import java.nio.file.attribute.*;
import java.util.*;

/**
 * Szybkie wyszukiwanie plików
 *
 * @author User
 */
public class SearchFiles extends SimpleFileVisitor<Path>
        implements Iterable<com.utils.Path> {

    private final List<com.utils.Path> files = new LinkedList<>();
    private final List<com.utils.Path> dirs = new LinkedList<>();
    private final LinkedHashMap<com.utils.Path, IOException> errors = new LinkedHashMap<>();

    private final boolean includeSubDirs;
    private int dirsVisited = 0;
    private final String root;

    public SearchFiles(String path, boolean includeSubDirs) throws IOException {
        this.includeSubDirs = includeSubDirs;
        this.root = path;
        Files.walkFileTree(Paths.get(path), this);
    }

    public List<com.utils.Path> getDirs() {
        return dirs;
    }

    public LinkedList<File> getFiles() {
        LinkedList<File> list = new LinkedList<>();
        for (com.utils.Path p : files)
            list.add(p.toFile());
        return list;
    }

    @Override
    public FileVisitResult preVisitDirectory(final Path dir,
            final BasicFileAttributes attrs) {

        if (dirsVisited > 0)
            dirs.add(new com.utils.Path(dir.toString())
                    .setAttributes(attrs)
                    .setRoot(new com.utils.Path(root)));

        return dirsVisited++ == 0 || includeSubDirs
                ? FileVisitResult.CONTINUE
                : FileVisitResult.SKIP_SUBTREE;
    }

    @Override
    public FileVisitResult visitFile(final Path file,
            final BasicFileAttributes attrs) {
        files.add(new com.utils.Path(file.toString())
                .setAttributes(attrs)
                .setRoot(new com.utils.Path(root)));
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult visitFileFailed(final Path file, IOException exc)
            throws IOException {
        errors.put(new com.utils.Path(file.toString())
                .setRoot(new com.utils.Path(root)), exc);
        return FileVisitResult.CONTINUE;
    }

    @Override
    public Iterator<com.utils.Path> iterator() {
        return files.iterator();
    }

    public LinkedHashMap<com.utils.Path, IOException> getErrors() {
        return errors;
    }

}
