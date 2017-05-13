package com.context.index;

import com.utils.Path;
import com.context.*;
import com.io.SearchFiles;
import com.mlogger.Log;
import com.utils.Ready;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import com.utils.reflections.TClass;
import java.io.File;
import java.util.*;

/**
 * @author Miłosz Ziernik
 * @date 07 września 2015
 * @encoding UTF-8
 */
public class ClassScanner extends Thread {

    private final IndexFile idxFile;
    private final File classPath;

    public ClassScanner(IndexFile idxFile, File classPath) {
        this.idxFile = idxFile;
        this.classPath = classPath;
        setPriority(Thread.MIN_PRIORITY);
        setName("Background file indexer");
    }

    public String process() throws Exception {
        IndexFile local;

        local = scanFiles(classPath);

        if (local.compare(idxFile))
            return null;

        File file = AppContext.sourcesPath.getFile(idxFile.indexFile);
        local.toJson().write(file, false);
        Log.info("Index", "Zaktualizowano plik " + file.toString());
        return "Zaktualizowano plik " + file.toString();
    }

    @Override
    public void run() {
        try {
            Ready.waitFor(new Interval(30, Unit.SECONDS), AppContextInitializer.class);
            process();
        } catch (Throwable e) {
            AppContextInitializer.addInitError(e);
        }

    }

    private IndexFile scanFiles(final File srcPath) throws Exception {
        final Set<String> fileNames = new LinkedHashSet<>();

        if (srcPath.isFile())
            return null;

        Log.info("ClassScanner", "Skanowanie katalogu " + srcPath);

        String sRoot = srcPath.getAbsolutePath();
        sRoot = sRoot.replace("\\", "/");

        File fRoot = new File(sRoot);
        if (!fRoot.exists())

            if (Environment.isWindows && sRoot.startsWith("/"))
                sRoot = sRoot.substring(1);

        if (!sRoot.endsWith("/"))
            sRoot += "/";

        for (Path path : new SearchFiles(sRoot, true)) {
            if (!path.toFile().isFile())
                continue;
            fileNames.add(path.getRelativePath().toString());
        }

        final IndexFile local = new IndexFile(null);

        for (String fName : fileNames) {
            if (!fName.endsWith(".class")
                    || fName.endsWith("package-info.class")) {
                local.addResourceMapping(classPath, fName);
                continue;
            }
            fName = fName.substring(0,
                    fName.length() - 6).replaceAll("/", ".");

            try {
                local.addClass(new TClass<>(fName));
            } catch (NoClassDefFoundError e) {
                Log.warning(e);
            } catch (Throwable e) {
                AppContextInitializer.addInitError(new RuntimeException(
                        "Błąd inicjalizacji klasy " + fName, e));
            }
        }

        Index.resources.removeIf(idx -> !idx.isReindexed());

        Log.debug("ClassScanner", "Skanowanie klas zakończone");

        return local;
    }

}
