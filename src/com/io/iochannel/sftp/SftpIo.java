package com.io.iochannel.sftp;

import com.io.TInputStream;
import com.io.TOutputStream;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpATTRS;
import com.jcraft.jsch.SftpException;
import com.io.iochannel.intfs.IOChannel;
import com.io.iochannel.intfs.IOPath;
import com.lang.LIo;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.util.LinkedList;
import java.util.List;
import java.util.Vector;

public class SftpIo extends IOChannel<SftpIo, SftpIoOptions, SftpIoAttribute> {

    private Session session = null;
    private ChannelSftp channelSftp = null;

    public SftpIo(URI uri) {
        super(uri, new SftpIoOptions());
    }

    @Override
    protected void doConnect() throws IOException {
        try {
            JSch jsch = new JSch();
            session = jsch.getSession(this.login, uri.getHost(), uri.getPort() > 0 ? uri.getPort() : 22);
            session.setConfig("StrictHostKeyChecking", "no"); // Wyłączenie restrykcyjnego sprawdzania klucza hosta
            session.setPassword(password);
            session.connect(); // Możliwość ustawienia timeout-u

            channelSftp = (ChannelSftp) session.openChannel("sftp");
            channelSftp.connect();

            // Przejście do określonej ścieżki i ustawienie jej jako katalog główny
            if (uri.getPath() != null && !uri.getPath().equals(""))
                channelSftp.cd(uri.getPath());

        } catch (Exception ex) {
            throw new IOException(ex);
        }
    }

    @Override
    protected void doClose() throws IOException {
        try {
            channelSftp.exit();
        } catch (Exception ex) {
            throw new IOException(ex);
        } finally {
            try {
                session.disconnect();
            } catch (Exception ex) {
                throw new IOException(ex);
            }
        }
    }

    @Override
    protected void doOpen(String directory) throws IOException {
        try {
            channelSftp.cd(directory);
        } catch (Exception ex) {
            throw new IOException(ex);
        }
    }

    @Override
    protected boolean doIsConnected() throws IOException {
        try {
            return channelSftp.isConnected();
        } catch (Exception ex) {
            throw new IOException(ex);
        }
    }

    @Override
    protected String doGetCurrentDirectory() throws IOException {
        try {
            return channelSftp.pwd();
        } catch (SftpException ex) {
            throw new IOException(ex);
        }
    }
//

    @Override
    protected boolean doDirectoryExists(String dirName) throws IOException {
        try {
            SftpATTRS dirAttr = channelSftp.stat(dirName);
            return dirAttr.isDir();
        } catch (Exception ex) {
            return false;
        }

    }

    @Override
    protected boolean doFileExists(String fileName) throws IOException {
        try {
            SftpATTRS fileAttr = channelSftp.stat(fileName);
            return !fileAttr.isDir() && !fileAttr.isLink();
        } catch (Exception ex) {
            return false;
        }
    }

    @Override
    protected boolean doCreateDir(String dirName) throws IOException {
        try {
            channelSftp.mkdir(dirName);
        } catch (Exception ex) {
            throw new IOException(ex);
        }
        return true;
    }

    @Override
    protected boolean doCreateFile(String fileName) throws IOException {
        try {
            try (OutputStream put = channelSftp.put(fileName)) {
            }
        } catch (Exception ex) {
            throw new IOException(ex);
        }
        return true;
    }

    @Override
    protected boolean doDeleteDir(String dirName) throws IOException {
        try {
            channelSftp.rmdir(dirName);
        } catch (Exception ex) {
            throw new IOException(ex);
        }
        return true;
    }

    @Override
    protected boolean doDeleteFile(String fileName) throws IOException {
        try {
            channelSftp.rm(fileName);
        } catch (Exception ex) {
            throw new IOException(ex);
        }
        return true;
    }

    @Override
    protected boolean doRename(String source, String destination) throws IOException {
        try {
            channelSftp.rename(source, destination);
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
            Vector<ChannelSftp.LsEntry> files = channelSftp.ls(getCurrentPath(IOPath.PathType.ABSOLUTE));

            for (ChannelSftp.LsEntry file : files)
                if (!file.getFilename().equals(".") && !file.getFilename().equals(".."))
                    list.add(file.getFilename());
        } catch (Exception ex) {
            throw new IOException(ex);
        }
        return list;
    }

    @Override
    protected List<SftpIoAttribute> doListFiles(String mask) throws IOException {
        // Maska jest nieobsługiwana!
        List<SftpIoAttribute> result = new LinkedList<>();
        try {
            Vector<ChannelSftp.LsEntry> files = channelSftp.ls(getCurrentPath(IOPath.PathType.ABSOLUTE));

            for (ChannelSftp.LsEntry file : files)
                if (!file.getFilename().equals(".") && !file.getFilename().equals(".."))
                    result.add(new SftpIoAttribute(file));

        } catch (Exception ex) {
            throw new IOException(ex);
        }
        return result;
    }

    @Override
    protected TInputStream doRead(String fileName) throws IOException {
        TInputStream in = null;
        try {
            in = new TInputStream(channelSftp.get(fileName));
        } catch (Exception ex) {
            throw new IOException(ex);
        }
        return in;
    }

    @Override
    protected TOutputStream doWrite(String fileName, boolean append) throws IOException {
        try {
            return new TOutputStream(channelSftp.put(fileName,
                    append ? ChannelSftp.APPEND : ChannelSftp.OVERWRITE));
        } catch (Exception ex) {
            throw new IOException(ex);
        }
    }

    @Override
    protected void doDownload(String fileName, OutputStream out) throws IOException {
        try {
            channelSftp.get(fileName, out);
        } catch (Exception ex) {
            throw new IOException(ex);
        }
    }

    @Override
    protected void doUpload(String fileName, InputStream in, boolean append) throws IOException {
        try {
            channelSftp.put(in, fileName, append ? ChannelSftp.APPEND : ChannelSftp.OVERWRITE);
        } catch (Exception ex) {
            throw new IOException(ex);
        }
    }
}
