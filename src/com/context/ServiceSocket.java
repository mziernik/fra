package com.context;

import com.net.tcp.TcpClient;
import com.net.tcp.TcpClientSocket;
import com.net.tcp.TcpServer;
import com.utils.Utils;
import com.utils.Is;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.InetSocketAddress;
import java.net.Socket;

public class ServiceSocket {

    static boolean init(int servicePort) throws IOException {

        try {
            TcpServer<Sck> server = new TcpServer<>("ServiceSocket", Sck.class);
            server.start(new InetSocketAddress(servicePort));
            return true;

        } catch (IOException e) {
            e.printStackTrace();
        }

        TcpClient client = new TcpClient();
        client.connect(new InetSocketAddress("127.0.0.1", servicePort), 10);

        BufferedReader reader = new BufferedReader(new InputStreamReader(client.getInputStream(), Utils.UTF8));
        BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(client.getOutputStream(), Utils.UTF8));

        String line = reader.readLine();

        writer.write("command:shutdown\n");
        writer.flush();

        return false;
    }

    static class Sck extends TcpClientSocket {

        Sck(TcpServer<TcpClientSocket> server, Socket socket) {
            super(server, socket);
        }

        @Override
        protected void execute() throws Exception {

            DataOutputStream out = new DataOutputStream(getOutputStream());

            out.writeUTF("service::" + AppConfig.getServiceName() + "\n");
            out.flush();

            BufferedReader reader = new BufferedReader(new InputStreamReader(getInputStream(), Utils.UTF8));
            String line = reader.readLine();

        }

    }

}
