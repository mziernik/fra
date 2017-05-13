package com.net.tcp;

import com.exceptions.EError;
import com.exceptions.ThrowableException;
import java.io.Closeable;
import java.io.IOException;
import java.net.*;
import java.util.LinkedList;
import com.mlogger.Log;
import com.utils.reflections.TClass;

public class TcpServer<T extends TcpClientSocket> implements Closeable {

    private final static LinkedList<TcpServer> servers = new LinkedList<>();
    public final ServerSocket server;
    private Thread thread;
    final LinkedList<T> clients = new LinkedList<T>();
    public final String name;
    private final TClass<T> socketClass;

    public static void onContextDestroyed() {
        for (TcpServer s : getServers())
            try {
                s.close();
            } catch (IOException ex) {
            }
    }

    public static LinkedList<TcpServer> getServers() {
        LinkedList<TcpServer> list = new LinkedList<>();
        synchronized (servers) {
            list.addAll(servers);
        }
        return list;
    }

    public TcpServer(String name, Class<T> socketClass) {
        try {
            this.server = new ServerSocket();
            this.name = name;
            this.socketClass = new TClass<>(socketClass);
        } catch (IOException e) {
            Log.warning(e);
            throw new ThrowableException(e);
        }
    }

    public void onException(Throwable e, TcpClient socket) {
        Log.error(e);
    }

    public LinkedList<T> getClients() {
        LinkedList<T> list = new LinkedList<>();
        synchronized (clients) {
            list.addAll(clients);
        }
        return list;
    }

    private class ClientThread extends Thread {

        private final Socket socket;

        public ClientThread(Socket socket) {
            this.socket = socket;
            setPriority(4);
        }

        @Override
        public void run() {
            T client = null;
            try {
                client = socketClass.newInstance(null, TcpServer.this, socket);
                try {
                    if (!client.accept())
                        return;

                } catch (Throwable e) {
                    onException(e, client);
                    return;
                }
                synchronized (clients) {
                    clients.add(client);
                }

                try {
                    client.execute();
                } catch (Throwable ex) {
                    onException(ex, client);
                } finally {
                    try {
                        socket.close();
                    } catch (IOException ex) {
                        onException(ex, client);
                    } finally {
                        synchronized (clients) {
                            clients.remove(client);
                        }
                    }
                }

            } catch (Throwable ex) {
                onException(ex, client);
            } finally {
                if (client != null)
                    try {
                        client.close();
                    } catch (IOException ex) {
                        onException(ex, client);
                    }

                try {
                    socket.close(); // na wysrost
                } catch (IOException ex) {
                    onException(ex, client);
                }
            }
        }

    }

    public void start(final SocketAddress address) throws IOException {

        try {
            server.bind(address);
        } catch (BindException e) {
            throw EError.processSocketException(e, address);
        }

        thread = new Thread(() -> {
            try {

                while (!thread.isInterrupted())
                    try {
                        new ClientThread(server.accept()).start();
                    } catch (Throwable ex) {
                        onException(ex, null);
                    }
            } finally {
                try {
                    server.close();
                } catch (IOException ex) {
                    onException(ex, null);
                }
            }

        });

        thread.setPriority(Thread.MIN_PRIORITY);
        thread.setName(name + " " + address.toString());
        thread.start();

        synchronized (servers) {
            servers.add(this);
        }

    }

    @Override
    public void close() throws IOException {
        for (T client : getClients())
            try {
                client.close();
            } catch (Throwable e) {
                onException(e, client);
            }

        try {
            server.close();
        } catch (Throwable ex) {
            onException(ex, null);
        }
        if (thread != null)
            thread.interrupt();

        synchronized (servers) {
            servers.remove(this);
        }
    }

}
