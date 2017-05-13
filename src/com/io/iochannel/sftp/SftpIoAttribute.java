package com.io.iochannel.sftp;

import com.jcraft.jsch.ChannelSftp.LsEntry;
import com.jcraft.jsch.SftpATTRS;
import com.io.iochannel.intfs.Attribute;
import java.util.Date;

public class SftpIoAttribute extends Attribute {

    public final boolean isFile;
    public final boolean isDirectory;
    public final boolean isLink;
    public final Date modified;
    public final long size;
    public final String permissions;

    public SftpIoAttribute(LsEntry file) {
        super(file.getFilename());

        SftpATTRS attrs = file.getAttrs();

        isDirectory = attrs.isDir();
        isLink = attrs.isLink();
        isFile = (!attrs.isDir() && !attrs.isLink());

        permissions = attrs.getPermissionsString();
        modified = new Date(attrs.getMTime() * 1000L);
        size = attrs.getSize();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(name + " ");
        sb.append(permissions + " ");

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
