package com.io;

import com.thread.TThread;
import com.utils.collections.*;
import com.utils.date.TDate;
import java.io.*;
import java.nio.file.*;
import java.nio.file.WatchEvent.Kind;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.TimeUnit;

import static com.context.index.Index.entries;

public abstract class DirWatcher extends TThread {

    private final File dir;
    private final boolean includeSubDirs;
    protected Kind<Path>[] kinds = new Kind[]{
        StandardWatchEventKinds.ENTRY_DELETE,
        StandardWatchEventKinds.ENTRY_MODIFY,
        StandardWatchEventKinds.ENTRY_CREATE};

    public DirWatcher(File dir) {
        this(dir, true);
    }

    public DirWatcher(File dir, boolean includeSubDirs) {
        super("Dir watcher: " + dir);
        this.dir = dir;
        this.includeSubDirs = includeSubDirs;
    }

    protected FileVisitResult preVisitDirectory(WatchService watcher, Path dir)
            throws IOException {
        dir.register(watcher, kinds);
        return FileVisitResult.CONTINUE;
    }

    @Override
    protected void run() throws Exception {

        try (WatchService watcher = FileSystems.getDefault().newWatchService()) {

            if (includeSubDirs)
                Files.walkFileTree(dir.toPath(), new SimpleFileVisitor<Path>() {
                    @Override
                    public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs)
                            throws IOException {
                        return DirWatcher.this.preVisitDirectory(watcher, dir);
                    }
                });
            else
                DirWatcher.this.preVisitDirectory(watcher, dir.toPath());

            while (!isInterrupted())
                try {
                    WatchKey key = watcher.poll(100, TimeUnit.MILLISECONDS);

                    long now = System.currentTimeMillis();

                    Map<Path, Pair<Long, WatchEntry>> map = new HashMap<>();
                    synchronized (delayed) {
                        for (Entry<Path, Pair<Long, WatchEntry>> en : delayed.entrySet())
                            if (now > en.getValue().first)
                                map.put(en.getKey(), en.getValue());

                        for (Path p : map.keySet())
                            delayed.remove(p);
                    }

                    for (Pair<Long, WatchEntry> p : map.values())
                        try {
                            onDelayed(p.second);

                        } catch (Throwable e) {
                            onException(e);
                        }

                    if (key == null)
                        continue;

                    for (WatchEvent<?> e : key.pollEvents()) {

                        Path path = ((WatchEvent<Path>) e).context();

                        WatchEntry entry = new WatchEntry(path, new File(dir, path.toString()));

                        Kind<Path> kind = ((WatchEvent<Path>) e).kind();

                        if (kind == StandardWatchEventKinds.ENTRY_CREATE)
                            onCreate(entry);
                        if (kind == StandardWatchEventKinds.ENTRY_MODIFY)
                            onModify(entry);
                        if (kind == StandardWatchEventKinds.ENTRY_DELETE)
                            onDelete(entry);
                    }

                    key.reset();
                } catch (InterruptedException ex) {
                    return;
                } catch (Throwable e) {
                    onException(e);
                }
        }
    }

    public abstract void onCreate(WatchEntry entry) throws Exception;

    public abstract void onDelete(WatchEntry entry) throws Exception;

    public abstract void onModify(WatchEntry entry) throws Exception;

    public abstract void onDelayed(WatchEntry entry) throws Exception;

    private Map<Path, Pair<Long, WatchEntry>> delayed = new HashMap<>();

    public class WatchEntry {

        public final Path path;
        public final File file;
        public final TDate created = new TDate();

        private WatchEntry(Path path, File file) {
            this.path = path;
            this.file = file;
        }

        public void delay(int milliseconds) {
            synchronized (delayed) {
                delayed.put(path, new Pair<>(System.currentTimeMillis() + milliseconds, this));
            }
        }

        @Override
        public int hashCode() {
            return path.hashCode() + file.hashCode();
        }

    }

}
