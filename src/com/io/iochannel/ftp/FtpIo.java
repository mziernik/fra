package com.io.iochannel.ftp;

import com.io.TInputStream;
import com.io.TOutputStream;
import com.io.iochannel.intfs.IOChannel;
import com.io.iochannel.intfs.IOPath;
import com.lang.LIo;
import com.utils.Utils;
import com.utils.Is;
import it.sauronsoftware.ftp4j.FTPClient;
import it.sauronsoftware.ftp4j.FTPDataTransferListener;
import it.sauronsoftware.ftp4j.FTPFile;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

public class FtpIo extends IOChannel<FtpIo, FtpIoOptions, FtpIoAttribute> {

    public final FTPClient ftp = new FTPClient();
    private FTPDataTransferListener listener;

    public FtpIo(URI uri) {
        super(uri, new FtpIoOptions());

        if (login == null || Is.empty(login)) {
            login = "anonymous";
            password = "";
        }

        ftp.setPassive(true);
        ftp.setType(FTPClient.TYPE_BINARY);
    }

    @Override
    protected void doConnect() throws IOException {
        try {
            if (ftp.isConnected() && ftp.isAuthenticated())
                return;

            if (!ftp.isConnected())
                ftp.connect(uri.getHost(), uri.getPort() > 0 ? uri.getPort() : 21);
            ftp.login(login, password);

            if (uri.getPath() != null && !uri.getPath().equals(""))
                ftp.changeDirectory(uri.getPath());
        } catch (Exception ex) {
            throw new IOException(ex);
        }
    }

    @Override
    protected void doClose() throws IOException {
        try {
            ftp.disconnect(true);
        } catch (Exception ex) {
            throw new IOException(ex);
        }
    }

    @Override
    protected boolean doIsConnected() throws IOException {
        try {
            return ftp.isConnected();
        } catch (Exception ex) {
            throw new IOException(ex);
        }
    }

    @Override
    protected String doGetCurrentDirectory() throws IOException {
        try {
            return ftp.currentDirectory();
        } catch (Exception ex) {
            throw new IOException(ex);
        }
    }

    @Override
    protected void doOpen(String directory) throws IOException {
        try {
            ftp.changeDirectory(directory);
        } catch (Exception ex) {
            throw new IOException(ex);
        }
    }

    @Override
    protected boolean doDirectoryExists(String dirName) throws IOException {
        try {
            open(dirName);
            open(getCurrentPath(IOPath.PathType.ABSOLUTE));
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    @Override
    protected boolean doFileExists(String fileName) throws IOException {
        try {
            ftp.fileSize(fileName);
            open(getCurrentPath(IOPath.PathType.ABSOLUTE));
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    @Override
    protected boolean doCreateDir(String dirName) throws IOException {
        try {
            ftp.createDirectory(dirName);
        } catch (Exception ex) {
            throw new IOException(ex);
        }
        return true;
    }

    @Override
    protected boolean doCreateFile(String fileName) throws IOException {
        throw new UnsupportedOperationException(LIo.UNSUPPORTED_OPERATION.toString()); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    protected boolean doDeleteDir(String dirName) throws IOException {
        try {
            ftp.deleteDirectory(dirName);
        } catch (Exception ex) {
            throw new IOException(ex);
        }
        return true;
    }

    @Override
    protected boolean doDeleteFile(String fileName) throws IOException {
        try {
            ftp.deleteFile(fileName);
        } catch (Exception ex) {
            throw new IOException(ex);
        }
        return true;
    }

    @Override
    protected boolean doRename(String source, String destination) throws IOException {
        try {
            ftp.rename(source, destination);
        } catch (Exception ex) {
            throw new IOException(ex);
        }
        return true;
    }

    @Override
    protected boolean doCopy(String source, String destination) throws IOException {
        throw new UnsupportedOperationException(LIo.UNSUPPORTED_OPERATION.toString()); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    protected boolean doMove(String source, String destination) throws IOException {
        throw new UnsupportedOperationException(LIo.UNSUPPORTED_OPERATION.toString()); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    protected List<String> doListNames() throws IOException {
        List<String> list = new LinkedList<>();
        try {
            list.addAll(Arrays.asList(ftp.listNames()));
        } catch (Exception ex) {
            throw new IOException(ex);
        }
        return list;
    }

    @Override
    protected List<FtpIoAttribute> doListFiles(String mask) throws IOException {
        List<FtpIoAttribute> result = new LinkedList<>();
        try {
            FTPFile[] list = mask != null && !mask.isEmpty() ? ftp.list(mask) : ftp.list();
            if (list != null)
                for (FTPFile file : list)
                    result.add(new FtpIoAttribute(file));
        } catch (Exception ex) {
            throw new IOException(ex);
        }
        return result;
    }

    @Override
    protected TInputStream doRead(String fileName) throws IOException {
        throw new UnsupportedOperationException(LIo.UNSUPPORTED_OPERATION.toString()); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    protected TOutputStream doWrite(String fileName, boolean append) throws IOException {
        throw new UnsupportedOperationException(LIo.UNSUPPORTED_OPERATION.toString()); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    protected void doDownload(String fileName, OutputStream out) throws IOException {
        try {
            ftp.download(fileName, out, 0, listener);
        } catch (Exception ex) {
            throw new IOException(ex);
        }
    }

    @Override
    protected void doUpload(String fileName, InputStream in, boolean append) throws IOException {
        try {
            if (append)
                ftp.append(fileName, in, 0, listener);
            else
                ftp.upload(fileName, in, 0, 0, listener);
        } catch (Exception ex) {
            throw new IOException(ex);
        }
    }

    public FtpIo setPassive(boolean passive) {
        ftp.setPassive(passive);
        return this;
    }

    public boolean isPassive() {
        return ftp.isPassive();
    }
}
