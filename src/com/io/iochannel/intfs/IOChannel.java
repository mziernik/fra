package com.io.iochannel.intfs;

import com.utils.Path;
import com.utils.Utils;
import com.utils.Is;
import com.io.TInputStream;
import com.io.TOutputStream;
import com.io.iochannel.file.FileIo;
import com.io.iochannel.ftp.FtpIo;
import com.io.iochannel.sftp.SftpIo;
import com.lang.LIo;
import com.utils.collections.Params;
import com.utils.collections.Params.Param;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.nio.file.Paths;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public abstract class IOChannel<Self extends IOChannel, TOptions extends Options, TAttrib extends Attribute>
        implements Closeable {

    protected final IOPath ioPath;
    protected final URI uri;
    protected String login;
    protected String password;
    protected final Map<String, String> parameters = new LinkedHashMap<>();
    private final Self self;
    protected SlashType slashType = SlashType.SLASH;

    public final TOptions options;
    public final Permission[] permissions;

    protected IOChannel(URI uri, TOptions options, Permission... permissions) {
        this.self = (Self) this;
        this.uri = uri;
        this.ioPath = new IOPath(uri);
        this.options = options;
        this.permissions = Objects.nonNull(permissions) ? Permission.values() : permissions;

        Path utilPath = new Path(uri);
        login = utilPath.uri.username;
        password = utilPath.uri.password;

        for (Param p : new Params().parseQuery(uri.getQuery()))
            parameters.put(p.name, Utils.toString(p.value));
    }

    public static IOChannel getChannel(URI uri) throws IOException {
        switch (uri.getScheme()) {
            case "file":
                return new FileIo(uri);
            case "ftp":
                return new FtpIo(uri);
            case "sftp":
                return new SftpIo(uri);
            case "ssh":
            case "scp":
            case "smb":
            case "nfs":
            case "cifs":
                throw new UnsupportedOperationException();
        }
        throw new IOException(LIo.UNSUPPORTED_PROTOCOL.toString(uri));

    }

    public URI getUri() {
        return uri;
    }

    public Self setPassword(String password) {
        this.password = password;
        return self;
    }

    public String getLogin() {
        return login;
    }

    public String getPassword() {
        return password;
    }

    public Self setLogin(String login) {
        this.login = login;
        return self;
    }

    public Self connect() throws IOException {
        try {
            doConnect();
            ioPath.setRootPath(Paths.get(doGetCurrentDirectory()));
            return self;

        } catch (Exception ex) {
            throw new IOException(ex);
        }
    }

    @Override
    public void close() throws IOException {
        doClose();
    }

    public Self open(String dirName) throws IOException {
        connectIfNotConnected();

        doOpen(getPathTo(dirName));
        movePathTo(dirName);

        return self;
    }

    public boolean directoryExists(String dirName) throws IOException {
        connectIfNotConnected();
        return doDirectoryExists(getPathTo(dirName));
    }

    public boolean fileExists(String fileName) throws IOException {
        connectIfNotConnected();
        checkFileName(fileName);
        return doFileExists(getPathTo(fileName));
    }

    public boolean createDir(String dirName) throws IOException {
        connectIfNotConnected();

        checkFileName(dirName);
        checkPathExistance(dirName);
        if (directoryExists(dirName))
            throw new IOException(LIo.DIRECTORY_ALREADY_EXISTS.toString(getPathTo(dirName)));

        return doCreateDir(getPathTo(dirName));
    }

    public boolean createFile(String fileName) throws IOException {
        connectIfNotConnected();

        checkFileName(fileName);
        checkPathExistance(fileName);
        if (fileExists(fileName))
            throw new IOException(LIo.FILE_ALREADY_EXISTS.toString(getPathTo(fileName)));

        return doCreateFile(getPathTo(fileName));
    }

    public boolean deleteDir(String dirName) throws IOException {
        connectIfNotConnected();

        if (!directoryExists(dirName))
            throw new IOException(LIo.DIRECTORY_NOT_EXIST.toString(getPathTo(dirName)));

        return doDeleteDir(getPathTo(dirName));
    }

    // TODO => do obłsużenia
//    public boolean deleteDirRecursive(String dirName) throws IOException {
//        connectIfNotConnected();
//        if (!directoryExists(dirName))
//            throw new IOException("Katalog \"" + getPathToFile(dirName) + "\" nie istnieje!");
//
//        try {
//            Path pathCopy = new Path(this.getCurrentDir());
//            open(dirName);
//
//            List<String> listNames = listNames();
//            for (String file : listNames) {
//                if (directoryExists(file)) {
//                    deleteDirRecursive(file);
//                }
//                else
//                    deleteFile(file);
//            }
//            open(pathCopy.toString());
//            deleteDir(dirName);
//        } catch (Exception ex) {
//            throw new IOException(ex);
//        }
//        return true;
//    }
    public boolean deleteFile(String fileName) throws IOException {
        connectIfNotConnected();

        checkFileName(fileName);
        if (!fileExists(fileName))
            throw new IOException(LIo.FILE_NOT_EXIST.toString(getPathTo(fileName)));

        return doDeleteFile(getPathTo(fileName));
    }

    public boolean rename(String source, String destination) throws IOException {
        connectIfNotConnected();
        checkFileName(destination);
        checkPathExistance(destination);
        if (!fileExists(source) && !directoryExists(source))
            throw new IOException(LIo.SRC_FILE_NOT_EXIST.toString(getPathTo(source)));
        if (fileExists(destination) || directoryExists(destination))
            throw new IOException(LIo.DEST_FILE_ALREADY_EXISTS.toString(getPathTo(destination)));

        return doRename(getPathTo(source), getPathTo(destination));
    }

    public boolean copy(String source, String destination) throws IOException {
        connectIfNotConnected();
        // TODO
        return doCopy(getPathTo(source), getPathTo(destination));
    }

    public boolean move(String source, String destination) throws IOException {
        connectIfNotConnected();
        // TODO
        return doMove(getPathTo(source), getPathTo(destination));
    }

    public List<String> listNames() throws IOException {
        connectIfNotConnected();
        return doListNames();
    }

    public List<TAttrib> listFiles(String mask) throws IOException {
        connectIfNotConnected();
        return doListFiles(mask);
    }

    public TInputStream read(String fileName) throws IOException {
        connectIfNotConnected();
        if (!fileExists(fileName))
            throw new IOException(LIo.SRC_FILE_NOT_EXIST.toString(getPathTo(fileName)));

        return doRead(getPathTo(fileName));
    }

    public TOutputStream write(String fileName, boolean append) throws IOException {
        connectIfNotConnected();
        checkPathExistance(fileName);
        return doWrite(getPathTo(fileName), append);
    }

    public Self download(String fileName, OutputStream out) throws IOException {
        connectIfNotConnected();
        if (!fileExists(fileName))
            throw new IOException(LIo.FILE_NOT_EXIST.toString(getPathTo(fileName)));

        doDownload(getPathTo(fileName), out);
        return self;
    }

    public Self upload(String fileName, InputStream in, boolean append) throws IOException {
        connectIfNotConnected();
        checkPathExistance(fileName);
        doUpload(getPathTo(fileName), in, append);
        return self;
    }

    //**************************************************************************
    protected abstract void doConnect() throws IOException;

    protected abstract void doClose() throws IOException;

    protected abstract boolean doIsConnected() throws IOException;

    protected abstract String doGetCurrentDirectory() throws IOException;

    protected abstract void doOpen(String directory) throws IOException;

    protected abstract boolean doDirectoryExists(String dirName) throws IOException;

    protected abstract boolean doFileExists(String fileName) throws IOException;

    protected abstract boolean doCreateDir(String dirName) throws IOException;

    protected abstract boolean doCreateFile(String fileName) throws IOException;

    protected abstract boolean doDeleteDir(String dirName) throws IOException;

    protected abstract boolean doDeleteFile(String fileName) throws IOException;

    protected abstract boolean doRename(String source, String destination) throws IOException;

    protected abstract boolean doCopy(String source, String destination) throws IOException;

    protected abstract boolean doMove(String source, String destination) throws IOException;

    protected abstract List<String> doListNames() throws IOException;

    protected abstract List<TAttrib> doListFiles(String mask) throws IOException;

    protected abstract TInputStream doRead(String fileName) throws IOException;

    protected abstract TOutputStream doWrite(String fileName, boolean append) throws IOException;

    protected abstract void doDownload(String fileName, OutputStream out) throws IOException;

    protected abstract void doUpload(String fileName, InputStream in, boolean append) throws IOException;

    //**************************************************************************
    protected String getPathTo(String path) {
        if (slashType == SlashType.SLASH)
            return ioPath.getPathTo(Paths.get(path)).toString().replace('\\', '/');
        return ioPath.getPathTo(Paths.get(path)).toString();

    }

    protected String movePathTo(String path) {
        if (slashType == SlashType.SLASH)
            return ioPath.moveToPath(Paths.get(path)).toString().replace('\\', '/');
        return ioPath.moveToPath(Paths.get(path)).toString();
    }

    public String getCurrentPath(IOPath.PathType type) {
        if (slashType == SlashType.SLASH)
            return ioPath.getCurrentPath(type).toString().replace('\\', '/');
        return ioPath.getCurrentPath(type).toString();
    }

    public void setPathSlashType(SlashType slash) {
        this.slashType = slash;
    }

    //**************************************************************************
    private void connectIfNotConnected() throws IOException {
        if (!isConnected())
            connect();
    }

    public boolean isConnected() throws IOException {
        return doIsConnected();
    }

    /**
     * Funkcja sprawdza czy wprowadzona nazwa pliku jest dozwolona. Ścieżka do
     * pliku jest pomijana podczas sprawdzania.
     *
     * @param fileName nazwa pliku (może być podana ze ścieżką)
     * @throws IOException niedozwolona nazwa pliku
     */
    private void checkFileName(String fileName) throws IOException {
        String last = new Path(fileName).last(false); // Pobranie ostatniego elementu ze ścieżki

        Pattern pattern = Pattern.compile(
                "# Match a valid Windows filename (unspecified file system).          \n"
                + "^                                # Anchor to start of string.        \n"
                + "(?!                              # Assert filename is not: CON, PRN, \n"
                + "  (?:                            # AUX, NUL, COM1, COM2, COM3, COM4, \n"
                + "    CON|PRN|AUX|NUL|             # COM5, COM6, COM7, COM8, COM9,     \n"
                + "    COM[1-9]|LPT[1-9]            # LPT1, LPT2, LPT3, LPT4, LPT5,     \n"
                + "  )                              # LPT6, LPT7, LPT8, and LPT9...     \n"
                + "  (?:\\.[^.]*)?                  # followed by optional extension    \n"
                + "  $                              # and end of string                 \n"
                + ")                                # End negative lookahead assertion. \n"
                + "[^<>:\"/\\\\|?*\\x00-\\x1F]*     # Zero or more valid filename chars.\n"
                + "[^<>:\"/\\\\|?*\\x00-\\x1F\\ .]  # Last char is not a space or dot.  \n"
                + "$                                # Anchor to end of string.            ",
                Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE | Pattern.COMMENTS);

        Matcher matcher = pattern.matcher(last);
        boolean isMatch = matcher.matches();

        if (!isMatch)
            throw new IOException(LIo.INVALID_FILE_NAME.toString(fileName));
    }

    /**
     * Funkcja sprawdza czy ścieżka do podanego pliku istnieje. Ostatni element
     * jest uznawany za nazwę pliku zaś pozostała cześć za ścieżkę.
     *
     * @param path ścieżka do pliku
     * @throws IOException ścieżka do pliku nie istnieje
     */
    private void checkPathExistance(String path) throws IOException {
        Path tmpPath = new Path(path);
        String last = tmpPath.last(true);
        if (!tmpPath.toString().equals("") && !directoryExists(tmpPath.toString()))
            throw new IOException(LIo.INVALID_FILE_PATH.toString(tmpPath.toString(), last));

    }
}
