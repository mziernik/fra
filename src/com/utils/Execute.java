package com.utils;

import com.config.CDebug;
import com.config.engine.field.CfBoolStringList;
import com.context.Environment;
import com.exceptions.EError;
import com.intf.runnable.RunnableEx1;
import com.intf.runnable.RunnableEx2;
import com.io.IOUtils;
import com.mlogger.Log;
import com.utils.collections.Strings;
import java.io.*;
import java.nio.charset.Charset;
import java.util.*;
import java.util.concurrent.TimeUnit;
import com.config.engine.interfaces.Cfg;
import com.utils.collections.Pair;
import static com.lang.LConfig.*;

public class Execute {

    private RunnableEx1<Process> onStart;
    private RunnableEx2<Boolean, String> onLineReaded;

    //ToDo dodać redirectErrorStream
    public static String windowsCmd(String command) throws IOException {
        try (InputStream in = Runtime.getRuntime().exec(
                new String[]{"cmd", "/c", command}
        ).getInputStream()) {
            return IOUtils.read(in, Charset.defaultCharset());
        }
    }

    public static class ExecResult {

        public final Throwable exception;
        public final String buffer;
        public final String error;
        public final int exitValue;

        public ExecResult(String buffer, String error, int exitValue, Throwable exception) {
            this.buffer = buffer;
            this.error = error;
            this.exitValue = exitValue;
            this.exception = exception;
        }

    }

    private int timeout;
    private String encoding = Environment.isWindows ? "CP852" : "UTF-8";
    private final Strings command = new Strings().separator(" ");

    java.lang.Process exec = null;

    public Execute(File file) throws IOException {
        this("cmd", "/c " + file.getAbsolutePath());
    }

    public Execute(String... command) {
        this.command.addAll(command);
    }

    public Execute timeout(int timeout) {
        this.timeout = timeout;
        return this;
    }

    public void destroy() {
        if (exec != null)
            exec.destroy();
    }

    public Execute onLineReaded(RunnableEx2<Boolean, String> onLineReaded) {
        this.onLineReaded = onLineReaded;
        return this;
    }

    public Execute onStart(RunnableEx1<Process> onStart) throws IOException {
        this.onStart = onStart;
        if (exec != null && onStart != null)
            try {
                onStart.run(exec);
            } catch (IOException | RuntimeException | Error ex) {
                throw ex;
            } catch (Exception ex) {
                throw new IOException(ex);
            }
        return this;
    }

    public Execute encoding(String encoding) {
        this.encoding = encoding;
        return this;
    }

    public Execute param(String param) {
        if (param != null && !param.isEmpty())
            command.add(param);
        return this;
    }

    public Execute param(Collection<String> params) {
        if (params != null)
            command.addAll(params);
        return this;
    }

    public Execute param(String... params) {
        if (params != null)
            command.addAll(params);
        return this;
    }

    @FunctionalInterface
    public static interface ProcessDone {

        public void onDone(Execute process, ExecResult result);
    }

    public ExecResult run() throws IOException {
        return run(null, false);
    }

    public void run(ProcessDone done) throws IOException {
        run(done, true);
    }

    private ExecResult run(ProcessDone done, boolean async) throws IOException {

        if (command.isEmpty())
            throw new IOException("No command");

        Log.debug("Execute", new Strings(command).toString(" "));

        ProcessBuilder builder = new ProcessBuilder();
        builder.command().addAll(command.astList());

        String sname = command.first();

        ProcessHandler ph = new ProcessHandler(done, builder, sname);

        if (async) {
            ph.start();
            //  Utils.sleep(1);
            return null;
        }

        ph.run();
        ExecResult res = ph.result;

        if (res.exception != null) {
            if (res.exception instanceof IOException)
                throw (IOException) res.exception;
            throw new IOException(res.exception);
        }

        return res;
    }

    private void onDone(ProcessDone done, ExecResult res, String sname) {

        boolean log = false;

        String sCmd = command.toString().toLowerCase();

        for (Pair<Boolean, String> pair : CStdoutLogging.getValue(null))
            log |= (pair != null
                    && pair.first != null
                    && pair.first
                    && pair.second != null
                    && sCmd.contains(pair.second.toLowerCase()));

        if (log && !res.buffer.isEmpty())
            Log.debug(sname, "Return value: " + res.exitValue, res.buffer);

        if (log && !res.error.isEmpty() && res.exitValue != 0)
            Log.warning(sname, "Return value: " + res.exitValue + "\n" + res.error, null);

        if (log && res.buffer.isEmpty()
                && res.error.isEmpty())
            Log.debug(sname, "Return value: " + res.exitValue);

        if (done != null)
            done.onDone(this, res);
    }

    public OutputStream getOutputStream() {
        return exec == null ? null : exec.getOutputStream();
    }

    @Cfg(parent = CDebug.class)
    public final static CfBoolStringList CStdoutLogging = new CfBoolStringList("process.stdout",
            DEBUG__PROCESS_STDOUT,
            ACTIVE, PROCESS)
            .description(DEBUG__PROCESS_STDOUT__DESCRIPTION);

    private class ProcessHandler extends Thread {

        private final ProcessBuilder builder;
        final StringWriter data = new StringWriter();
        final StringWriter error = new StringWriter();
        private final String name;
        public Throwable exception;
        public int exitValue;
        private final ProcessDone done;

        private ExecResult result;

        private ProcessHandler(ProcessDone done, ProcessBuilder builder, String name) {
            this.builder = builder;
            this.name = name;
            this.done = done;
        }

        @Override
        public void run() {
            try {
                exec = builder.start();
                if (onStart != null)
                    onStart.run(exec);

                InputStream in = exec.getInputStream();
                InputStream err = exec.getErrorStream();
                exec.getOutputStream();

                long time = new Date().getTime();

                int emptyCounter = 0;

                while (emptyCounter < 5) {

                    if (in.available() == 0 && err.available() == 0 && !exec.isAlive()) {
                        ++emptyCounter;
                        exec.waitFor(1, TimeUnit.MILLISECONDS);
                        continue;
                    }
                    emptyCounter = 0;

                    if (timeout > 0 && new Date().getTime() - time > timeout) {
                        exec.destroy();
                        Log.error(name, "Timeout");
                        return;
                    }
                    byte[] buff = new byte[10240];
                    int cnt = in.read(buff, 0, in.available());
                    if (cnt > 0) {
                        String str = new String(buff, 0, cnt, encoding);
                        data.write(str);
                        if (onLineReaded != null)
                            onLineReaded.run(false, str);
                    }
                    cnt = err.read(buff, 0, err.available());
                    if (cnt > 0) {
                        String str = new String(buff, 0, cnt, encoding);
                        error.write(str);
                        if (onLineReaded != null)
                            onLineReaded.run(false, str);
                    }
                    exec.waitFor(1, TimeUnit.MILLISECONDS);
                }
            } catch (Throwable ex) {
                exception = ex;
                EError.addDetails(exception, "Command", command.toString());
                Log.error(ex);
                if (exec != null)
                    exec.destroy();
            } finally {
                if (exec != null)
                    exitValue = exec.exitValue();
                result = new ExecResult(data.toString(), error.toString(), exitValue, exception);
                onDone(done, result, name);
            }
        }
    }

}
