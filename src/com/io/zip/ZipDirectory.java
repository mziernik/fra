package com.io.zip;

import com.utils.Path;
import com.io.IOUtils;
import com.io.SearchFiles;
import java.io.*;
import java.nio.charset.Charset;
import java.util.zip.*;

public class ZipDirectory extends ZipOutputStream {

    public ZipDirectory(OutputStream out, Charset charset) {
        super(out, charset);
    }

    public ZipDirectory(OutputStream out) {
        super(out);
    }

    public ZipDirectory addFile(Path path, String name,
            boolean uncompressed, String comment, byte[] extra) throws IOException {

        ZipEntry ze = new ZipEntry(name);
        File file = path.toFile();

        InputStream in = null;
        if (uncompressed) {
            ze.setMethod(ZipEntry.STORED);
            byte[] buffer = IOUtils.read(file);
            ze.setCompressedSize(buffer.length);
            CRC32 crc = new CRC32();
            crc.update(buffer);
            ze.setCrc(crc.getValue());
            in = new ByteArrayInputStream(buf);
        }

        if (in == null)
            in = new BufferedInputStream(new FileInputStream(file));

        ze.setCreationTime(path.getAttributes().creationTime());
        ze.setLastModifiedTime(path.getAttributes().lastModifiedTime());
        ze.setLastAccessTime(path.getAttributes().lastAccessTime());
        ze.setSize(in.available());
        if (comment != null)
            ze.setComment(comment);
        if (extra != null)
            ze.setExtra(extra);
        putNextEntry(ze);
        IOUtils.copy(in, this);

        closeEntry();

        return this;
    }

    public ZipDirectory addDir(File dir, boolean includeSubDirs) throws IOException {
        for (Path file : new SearchFiles(dir.getAbsolutePath(), true))
            addFile(file, file.getRelativePath().toString(), false, null, null);
        return this;
    }

}
