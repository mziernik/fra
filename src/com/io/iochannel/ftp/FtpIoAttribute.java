package com.io.iochannel.ftp;

import com.io.iochannel.intfs.Attribute;
import it.sauronsoftware.ftp4j.FTPFile;
import java.util.Date;

public class FtpIoAttribute extends Attribute {

    public final boolean isFile;
    public final boolean isDirectory;
    public final boolean isLink;
    public final Date modified;
    public final long size;

    public FtpIoAttribute(FTPFile file) {
        super(file.getName());
        this.isFile = file.getType() == FTPFile.TYPE_FILE;
        this.isDirectory = file.getType() == FTPFile.TYPE_DIRECTORY;
        this.isLink = file.getType() == FTPFile.TYPE_LINK;

        modified = file.getModifiedDate();
        size = file.getSize();
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
