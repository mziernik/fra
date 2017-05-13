package com.io.iochannel.file;

import com.io.iochannel.intfs.Attribute;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Date;

public class FileIoAttribute extends Attribute {

    public final boolean isFile;
    public final boolean isDirectory;
    public final boolean isLink;
    public final Date modified;
    public final long size;

    public FileIoAttribute(Path path) throws IOException {
        super(path.getFileName().toString());

        BasicFileAttributes attr = Files.readAttributes(path, BasicFileAttributes.class);

        this.isDirectory = attr.isDirectory();
        this.isFile = attr.isRegularFile();
        this.isLink = attr.isSymbolicLink();
        this.modified = new Date(attr.lastModifiedTime().toMillis());
        this.size = attr.size();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(name + " ");

        if (isFile)
            sb.append("file ");
        else if (isDirectory)
            sb.append("directory ");
        else if (isLink)
            sb.append("link ");

        sb.append(modified + " ");
        sb.append(size + "B");

        return sb.toString();
    }
}
