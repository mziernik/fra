package service;

import com.utils.console.TConsole;
import com.utils.Utils;
import com.context.index.Index;
import com.context.unit_test.WebAppServerRequired;
import com.servers.websocket.client.WebSocketClient;
import com.servers.websocket.handshake.ServerHandshake;
import com.servlet.websocket.WebSocketController;
import com.servlet.websocket.WebSocketEndpoint;
import context.ServiceTest;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.ByteBuffer;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.junit.*;

/**
 * @author Miłosz Ziernik
 * @date 17 grudnia 2015
 * @encoding UTF-8
 */
public class WebSockets extends ServiceTest implements WebAppServerRequired {

    static {
        Index.webSockets.put(WsCtrl.class, WsCtrl.class.getAnnotation(WebSocketEndpoint.class));
    }
    final static LinkedHashSet<String> set = new LinkedHashSet<>();

    @Test(timeout = 30000)
    public void checkAuthRequired() throws Exception {

        String url = getUrl("test").toString();

        final LinkedHashSet<String> set = new LinkedHashSet<>();

        final WebSocketClient client = new WebSocketClient(URI.create(url)) {
            @Override
            public void onOpen(ServerHandshake handshakedata) {
                synchronized (WebSockets.set) {
                    WebSockets.set.add("S-Hello");
                }
                send("Hello");
            }

            @Override
            public void onMessage(String message) {
                synchronized (WebSockets.set) {
                    WebSockets.set.add("S-" + message);
                }
                send("arr".getBytes());
            }

            @Override
            public void onMessage(ByteBuffer bytes) {
                synchronized (WebSockets.set) {
                    WebSockets.set.add("S-" + new String(bytes.array()));
                }
                close();
            }

            @Override
            public void onClose(int code, String reason, boolean remote) {
                synchronized (WebSockets.set) {
                    WebSockets.set.add("S-Close-" + code + "-" + reason);
                }
            }

            @Override
            public void onError(Exception ex) {
                TConsole.printErr(ex);
                close();
            }

        };

        client.connectBlocking();
        while (!client.isClosed())
            Thread.sleep(1);

        Iterator<String> itr = WebSockets.set.iterator();

        assertEquals("S-Hello", itr.next());
        assertEquals("R-Hello", itr.next());
        assertEquals("S-Re: Hello", itr.next());
        assertEquals("R-arr", itr.next());
        assertEquals("S-abcarr", itr.next());
        assertEquals("S-Close-1000-", itr.next());
    }

}

@WebSocketEndpoint(url = "test")
class WsCtrl extends WebSocketController {

    public WsCtrl() {

    }

    @Override
    public void onMessage(String data) {
        synchronized (WebSockets.set) {
            WebSockets.set.add("R-" + data);
        }
        send("Re: " + data);
    }

    @Override
    public void onMessage(byte[] data) {

        synchronized (WebSockets.set) {
            WebSockets.set.add("R-" + new String(data));
        }

        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        try {
            bout.write("abc".getBytes());
            bout.write(data);
            send(bout.toByteArray());

        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

    }

}
