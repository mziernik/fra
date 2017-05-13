package com.mlogger.handlers;

import com.utils.console.TConsole;
import com.mlogger.*;
import com.utils.text.StrWriter;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.security.MessageDigest;
import java.util.LinkedList;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;
import com.mlogger.interfaces.LogException;
import com.mlogger.utils._Internal;
import java.util.UUID;

/**
 * Miłosz Ziernik 2014/06/13
 */
public abstract class LogHandler extends Handler {

    public abstract void publish(LogElement element,
            LinkedList<Handler> handlers, LogRecord record) throws Exception;

    @Override
    public void publish(LogRecord record) {
        try {
            MLogger.MLogRec rec = null;
            for (Object o : record.getParameters())
                if (o instanceof MLogger.MLogRec)
                    rec = (MLogger.MLogRec) o;

            if (rec == null)
                return;

            publish(rec.log, rec.handlers, record);
        } catch (Exception ex) {
            TConsole.printErr(ex);
        }
    }

    @Override
    public void flush() {

    }

    @Override
    public void close() throws SecurityException {

    }

    protected byte[] prepareData(LogElement log) throws IOException {

        byte[] data = log.toString(this).getBytes(Charset.forName("UTF-8"));

        LogFlag.LogFlags flags = new LogFlag.LogFlags(log.options);
        if (log.options.priority)
            flags.add(LogFlag.priority);

        if (log.options.token != null)
            flags.add(LogFlag.token);
        /*
         if ((log.options.compressed != null && log.options.compressed)
         || (log.options.compressed == null && data.length > 500)) {
         flags.add(LogFlag.compressed);

         ByteArrayOutputStream bout = new ByteArrayOutputStream();

         DeflaterOutputStream def = new DeflaterOutputStream(bout);
         try {
         def.write(data);
         def.flush();
         } finally {
         def.close();
         }

         data = bout.toByteArray();
         }
         */
        if (log.options.encryptionKey != null && log.options.token != null)
            try {
                flags.add(LogFlag.encrypted);
                Cipher cipher = Cipher.getInstance("AES/ECB/PKCS5Padding");
                SecretKeySpec secretKey = new SecretKeySpec(
                        MessageDigest.getInstance("MD5")
                                .digest(log.options.encryptionKey
                                        .getBytes(Charset.forName("UTF-8"))
                                ), "AES");
                cipher.init(Cipher.ENCRYPT_MODE, secretKey);
                data = cipher.doFinal(data);
            } catch (Exception ex) {
                throw new LogException(this, log, ex);
            }

        if (data.length > log.options.maxDatagramSize)
            flags.add(LogFlag.multipart);

        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        DataOutputStream dout = new DataOutputStream(bout);
        dout.write(MHandler.signature);
        dout.writeByte(1); // wersja
        dout.writeInt(data.length + 1
                + (log.options.token != null ? 16 : 0)
                + (log.options.priority ? 16 : 0)
        );
        dout.write(flags.toByte());
        // rozmiar całkowity

        if (log.options.token != null) {
            ByteBuffer bb = ByteBuffer.wrap(new byte[16]);
            bb.putLong(log.options.token.getMostSignificantBits());
            bb.putLong(log.options.token.getLeastSignificantBits());
            bout.write(bb.array());
        }

        if (log.options.priority) {
            ByteBuffer bb = ByteBuffer.wrap(new byte[16]);
            UUID uuid = ((Log) log).uid.value();
            bb.putLong(uuid.getMostSignificantBits());
            bb.putLong(uuid.getLeastSignificantBits());
            bout.write(bb.array());
        }

        dout.write(data);
        return bout.toByteArray();
    }

    protected void processResponse(LogElement log, String line) throws ServerException {

        _Internal.addInfo(log, this, "Response", line);

        if (line == null || line.isEmpty())
            throw new ServerException("Brak odpowiedzi");

        if (line.startsWith("error: "))
            throw new ServerException(unescape(line.substring("error: ".length())));

//        if (line.startsWith("confirm: "))
//            if (!line.substring("confirm: ".length()).equals(log.uid.toString()))
//                throw new ServerException("Nieprawidłowa odpowiedź");
    }

    public static String unescape(String st) {

        StrWriter sb = new StrWriter();

        for (int i = 0; i < st.length(); i++) {
            char ch = st.charAt(i);
            if (ch == '\\') {
                char nextChar = (i == st.length() - 1) ? '\\' : st
                        .charAt(i + 1);
                // Octal escape?
                if (nextChar >= '0' && nextChar <= '7') {
                    String code = "" + nextChar;
                    i++;
                    if ((i < st.length() - 1) && st.charAt(i + 1) >= '0'
                            && st.charAt(i + 1) <= '7') {
                        code += st.charAt(i + 1);
                        i++;
                        if ((i < st.length() - 1) && st.charAt(i + 1) >= '0'
                                && st.charAt(i + 1) <= '7') {
                            code += st.charAt(i + 1);
                            i++;
                        }
                    }
                    sb.append((char) Integer.parseInt(code, 8));
                    continue;
                }
                switch (nextChar) {
                    case '\\':
                        ch = '\\';
                        break;
                    case 'b':
                        ch = '\b';
                        break;
                    case 'f':
                        ch = '\f';
                        break;
                    case 'n':
                        ch = '\n';
                        break;
                    case 'r':
                        ch = '\r';
                        break;
                    case 't':
                        ch = '\t';
                        break;
                    case '\"':
                        ch = '\"';
                        break;
                    case '\'':
                        ch = '\'';
                        break;
                    // Hex Unicode: u????
                    case 'u':
                        if (i >= st.length() - 5) {
                            ch = 'u';
                            break;
                        }
                        int code = Integer.parseInt(
                                "" + st.charAt(i + 2) + st.charAt(i + 3)
                                + st.charAt(i + 4) + st.charAt(i + 5), 16);
                        sb.append(Character.toChars(code));
                        i += 5;
                        continue;
                }
                i++;
            }
            sb.append(ch);
        }
        return sb.toString();
    }

    public void start() throws IOException {

    }

    public void stop() {

    }

}
