package com.io.iochannel.file;

import com.context.Environment;
import com.io.TInputStream;
import com.io.TOutputStream;
import com.io.iochannel.intfs.IOChannel;
import com.io.iochannel.intfs.IOPath;
import com.io.iochannel.intfs.SlashType;
import com.lang.LIo;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

public class FileIo extends IOChannel<FileIo, FileIoOptions, FileIoAttribute> {

    public FileIo(URI uri) {
        super(uri, new FileIoOptions());

        if (Environment.isWindows)
            this.slashType = SlashType.BACKSLASH;
    }

    @Override
    protected void doConnect() throws IOException {
    }

    @Override
    protected void doClose() throws IOException {
    }

    @Override
    protected boolean doIsConnected() throws IOException {
        return true;
    }

    @Override
    protected String doGetCurrentDirectory() throws IOException {
        return getCurrentPath(IOPath.PathType.ABSOLUTE);
    }

    @Override
    protected void doOpen(String dirName) throws IOException {
        if (!doDirectoryExists(dirName))
            throw new IOException(LIo.DIRECTORY_NOT_EXIST.toString(dirName));
    }

    @Override
    protected boolean doDirectoryExists(String dirName) throws IOException {
        return Files.isDirectory(Paths.get(dirName));
    }

    @Override
    protected boolean doFileExists(String fileName) throws IOException {
        return Files.isRegularFile(Paths.get(fileName));
    }

    @Override
    protected boolean doCreateDir(String dirName) throws IOException {
        Files.createDirectory(Paths.get(dirName));
        return true;
    }

    @Override
    protected boolean doCreateFile(String fileName) throws IOException {
        Files.createFile(Paths.get(fileName));
        return true;
    }

    @Override
    protected boolean doDeleteDir(String dirName) throws IOException {
        Files.delete(Paths.get(dirName));
        return true;
    }

    @Override
    protected boolean doDeleteFile(String fileName) throws IOException {
        Files.delete(Paths.get(fileName));
        return true;
    }

    @Override
    protected boolean doRename(String source, String destination) throws IOException {
        Files.move(Paths.get(source), Paths.get(destination));
        return true;
    }

    @Override
    protected boolean doCopy(String source, String destination) throws IOException {
        Files.copy(Paths.get(source), Paths.get(destination));
        return true;
    }

    @Override
    protected boolean doMove(String source, String destination) throws IOException {
        Files.move(Paths.get(source), Paths.get(destination));
        return true;
    }

    @Override
    protected List<String> doListNames() throws IOException {
        return Files.list(Paths.get(getCurrentPath(IOPath.PathType.ABSOLUTE)))
                .map((Path p) -> p.getFileName().toString())
                .collect(Collectors.toList());
    }

    @Override
    protected List<FileIoAttribute> doListFiles(String mask) throws IOException {
        List<Path> files = Files.list(Paths.get(getCurrentPath(IOPath.PathType.ABSOLUTE)))
                .collect(Collectors.toList());

        List<FileIoAttribute> result = new LinkedList<>();

        for (Path path : files)
            result.add(new FileIoAttribute(path));

        return result;
    }

    @Override
    protected TInputStream doRead(String fileName) throws IOException {
        return new TInputStream(Files.newInputStream(Paths.get(fileName)));
    }

    @Override
    protected TOutputStream doWrite(String fileName, boolean append) throws IOException {
        if (append)
            return new TOutputStream(Files.newOutputStream(Paths.get(fileName), StandardOpenOption.APPEND));
        return new TOutputStream(Files.newOutputStream(Paths.get(fileName)));
    }

    @Override
    protected void doDownload(String fileName, OutputStream out) throws IOException {
        Files.copy(Paths.get(fileName), out);
    }

    @Override
    protected void doUpload(String fileName, InputStream in, boolean append) throws IOException {
        if (append)
            throw new UnsupportedOperationException(LIo.UNSUPPORTED_OPERATION.toString()); //To change body of generated methods, choose Tools | Templates.
        Files.copy(in, Paths.get(fileName), StandardCopyOption.REPLACE_EXISTING);
    }
}
