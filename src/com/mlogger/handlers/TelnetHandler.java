package com.mlogger.handlers;

import com.utils.text.StrWriter;
import com.net.tcp.TcpServer;
import java.io.*;
import java.net.*;
import java.util.*;
import java.util.logging.*;
import com.mlogger.Log;
import com.mlogger.LogElement;
import com.utils.Utils;
import com.utils.Is;
import com.net.tcp.*;
import com.utils.collections.Strings;
import com.utils.date.TDate;

public class TelnetHandler extends LogHandler {

    //VT100
    private final TcpServer<TelnetClientsocket> server;
    private final InetSocketAddress address;

    @Override
    public void start() throws IOException {
        server.start(address);
    }

    @Override
    public void stop() {
        try {
            server.close();
        } catch (IOException ex) {
            server.onException(ex, null);
        }
    }

    public static class TelnetClientsocket extends TcpClientSocket {

        private BufferedWriter writer;
        private BufferedReader reader;
        InputStream in;
        OutputStream out;

        public TelnetClientsocket(TcpServer server, Socket socket) throws Exception {
            super(server, socket);
            in = socket.getInputStream();
            reader = new BufferedReader(new InputStreamReader(in));
            out = socket.getOutputStream();
        }

        void command(String command) throws IOException {
            out.write(27);
            out.write(command.getBytes());
            // out.write("\r\n".getBytes());
            out.flush();
        }

        @Override
        public void execute() throws Exception {
            while (in.available() > 0)
                in.read();

            Thread.sleep(1); // czekaj chwilę

            command("[2J"); // wyczysc ekran
            command("c"); // wyczysc ekran

            // to wysyła putty 
            //ff fb 1f ff fb 20 ff fb 18 ff fb 27 ff fd 01 ff fb 03 ff fd 03 00
            out.write("Wybierz kodowanie:\r\n\r\n".getBytes());

            out.write("1. IBM 852\r\n".getBytes());
            out.write("2. UTF-8\r\n".getBytes());
            out.write("3. ISO 8859-2\r\n".getBytes());
            out.write("\r\n[1]:".getBytes());

            reader = new BufferedReader(new InputStreamReader(in));

            String encoding = null;

            while (encoding == null) {
                int value = reader.read();
                switch (value) {
                    case '1':
                        encoding = "IBM852";
                        break;
                    case '2':
                        encoding = "UTF-8";
                        break;
                    case '3':
                        encoding = "ISO-8859-2";
                        break;
                    case '\r':
                        encoding = "IBM852";
                        break;
                    case -1:
                    case 27:
                        return;
                }
            }
            command("[2J"); // wyczysc ekran
            command("c"); // wyczysc ekran
            command("[12h"); // wylacz echo
            command("[?25l"); // wylacz kursor

            writer = new BufferedWriter(new OutputStreamWriter(out, encoding));

            while (isRunning()) {
                int value = reader.read();
                if (value == 27 || value == -1)
                    return;
            }
        }

        private void print(int color, String msg) throws IOException {
            writer.append("\033[")
                    .append(Integer.toString(30 + color))
                    .append("m")
                    .append(msg).append("\033[0m");
        }

        public void publish(LogElement element, LinkedList<Handler> handlers,
                LogRecord record) throws Exception {

            if (writer == null)
                return;

            if (element instanceof Log) {
                Log log = (Log) element;

                /*

                 col = c.equals("black") ? 0
                 : c.equals("red") ? 1
                 : c.equals("green") ? 2
                 : c.equals("yellow") ? 3
                 : c.equals("blue") ? 4
                 : c.equals("magenta") ? 5
                 : c.equals("cyan") ? 6
                 : c.equals("white") ? 7
                 : -1;
                 */
                print(3, log.date.value().toString(
                        (new TDate().isSameDay(log.date.value()) ? ""
                        : "yyyy-MM-dd ")
                        + "HH:mm:ss.SSS"));

                writer.append(" ");

                if (!log.tag.isEmpty())
                    print(6, "[" + new Strings().addAll(log.tag) + "] ");

                if (!log.value.isEmpty())
                    writer.append(log.value.value().value.toString()
                            .replace("\r", "").replace("\n", "\r\n"));

                if (!log.comment.isEmpty()) {
                    writer.write(" ");
                    print(5, log.comment.value());
                }

                writer.write("\r\n");

                if (!log.errorStack.isEmpty()) {
//                    StrWriter sb = new StrWriter();
//                    for (String s : log.errorStack)
//                        sb.append("    ").append(s).append("\r\n");
//                    print(1, sb.toString());
                }

                writer.flush();

            }

        }
    }

    public TelnetHandler(URI address) throws IOException {
        super();
        this.address = new InetSocketAddress(address.getHost() != null
                ? address.getHost() : "localhost",
                address.getPort() > 0 ? address.getPort() : 23);

        server = new TcpServer<>("MLogger Telnet", TelnetClientsocket.class);
    }

    @Override
    public void publish(LogElement element, LinkedList<Handler> handlers, LogRecord record) throws Exception {
        for (TelnetClientsocket socket : server.getClients())
            socket.publish(element, handlers, record);
    }

    @Override
    public void close() throws SecurityException {
        try {
            server.close();
        } catch (IOException ex) {
            server.onException(ex, null);
        }
        super.close();
    }

}
