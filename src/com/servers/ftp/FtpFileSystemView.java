package com.servers.ftp;

import org.apache.ftpserver.ftplet.FileSystemView;
import org.apache.ftpserver.ftplet.FtpException;
import org.apache.ftpserver.ftplet.User;

public class FtpFileSystemView implements FileSystemView {

    private final User user;

    public FtpFileSystemView(User user) {
        this.user = user;
    }

    @Override
    public FFtpFile getHomeDirectory() throws FtpException {
        return new FFtpFile();
    }

    @Override
    public FFtpFile getWorkingDirectory() throws FtpException {
        return new FFtpFile();
    }

    @Override
    public boolean changeWorkingDirectory(String string) throws FtpException {
        return true;
    }

    @Override
    public FFtpFile getFile(String string) throws FtpException {
        return new FFtpFile();
    }

    @Override
    public boolean isRandomAccessible() throws FtpException {
        return false;
    }

    @Override
    public void dispose() {
    }

}
