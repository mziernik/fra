package com.io.iochannel.intfs;

import java.net.URI;
import java.nio.file.Path;
import java.nio.file.Paths;

public class IOPath {

    private Path root;
    Path current;

    public IOPath(URI uri) {
        this(Paths.get(uri.getPath()));
    }

    public IOPath(Path root) {
        this.root = root;
        this.current = root;
    }

    public Path moveToPath(Path dst) {
        current = getPathTo(dst);
        return current;
    }

    public Path getPathTo(Path dst) {
        Path tmpPath = isAbsolute(dst) ? getAbsolute(dst) : current.resolve(dst).normalize();
        return tmpPath.startsWith(root) ? tmpPath : root;
    }

    private boolean isAbsolute(Path path) {
        return !current.resolve(path).startsWith(root);
    }

    private Path getAbsolute(Path path) {
        if (path.getRoot().toString().length() == 1) // tylko '/' (pomijanie 'C:/' itp)
            return Paths.get(root.toString(), path.toString()).normalize();
        return root;
    }

    public Path getCurrentPath(PathType pathType) {
        if (pathType == PathType.ABSOLUTE)
            return current;
        return Paths.get("/").resolve(root.relativize(current));
    }

    public Path getRootPath() {
        return root;
    }

    public void setRootPath(Path path) {
        this.root = path;
        this.current = path;
    }

    public static enum PathType {
        ABSOLUTE, RELATIVE
    }

    @Override
    public String toString() {
        return current.toString();
    }
}
