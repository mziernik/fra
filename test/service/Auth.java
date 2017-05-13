package service;

import com.utils.Utils;
import com.context.unit_test.WebAppServerRequired;
import com.io.IOUtils;
import com.net.HttpClient;
import com.servlet.controller.Controller;
import com.servlet.interfaces.Endpoint;
import com.servlet.requests.HttpRequest;
import context.ServiceTest;
import org.junit.*;

/**
 * @author Miłosz Ziernik
 * @date 17 grudnia 2015
 * @encoding UTF-8
 */
public class Auth extends ServiceTest implements WebAppServerRequired {

    static boolean authEnabled;

    static {
        registerController(AuthCtrl.class);
    }

    @Test()
    public void checkAuthRequired() throws Exception {
        authEnabled = true;
        /* try (HttpClient http = new HttpClient(getUrl("auth"))) {
            Assert.assertEquals(401, http.getResponseCode());
            Assert.assertEquals("Unauthorized", http.getResponseMessage());
            String response = IOUtils.read(http.getInputStream(), Utils.UTF8);
            Assert.assertTrue(response.contains("<title>Logowanie</title>"));
        }*/
    }

    @Test()
    public void checkNoAuthRequired() throws Exception {
        authEnabled = false;
        try (HttpClient http = new HttpClient(getUrl("auth"))) {
            Assert.assertEquals(200, http.getResponseCode());
            Assert.assertEquals("OK", http.getResponseMessage());
            String response = IOUtils.read(http.getInputStream(), Utils.UTF8);
            Assert.assertEquals("AUTH OK", response);
        }
    }

    @Test()
    public void authAsRoot() throws Exception {
        authEnabled = true;
        try (HttpClient http = new HttpClient(getUrl("auth"))) {
            http.setAuthorization("root", "1234");

            Assert.assertEquals(200, http.getResponseCode());
            Assert.assertEquals("OK", http.getResponseMessage());
            String response = IOUtils.read(http.getInputStream(), Utils.UTF8);
            Assert.assertEquals("AUTH OK", response);
        }
    }

    @Endpoint(url = "auth", title = "Bla")
    static class AuthCtrl implements Controller {

        public AuthCtrl() {
            endpoint().auth(authEnabled);
        }

        @Override
        public void onRequest(HttpRequest http) throws Exception {
            http.returnPlainText("AUTH OK");
        }

    }
}
