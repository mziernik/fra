package service;

import com.context.unit_test.WebAppServerRequired;
import com.net.HttpClient;
import com.servlet.controller.Controller;
import com.servlet.interfaces.Endpoint;
import com.servlet.requests.HttpRequest;
import com.utils.date.Timestamp;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import context.ServiceTest;
import org.junit.*;

/**
 * @author Miłosz Ziernik
 * @date 17 grudnia 2015
 * @encoding UTF-8
 */
public class Requests extends ServiceTest implements WebAppServerRequired {

    static boolean authEnabled;

    static {
        registerController(Ctrl.class);
    }

    @Test()
    public void test1() throws Exception {

        int count = 0;
        int sum = 0;

        for (int i = 0; i < 150; i++) {

            Timestamp ts = new Timestamp();
            try (HttpClient http = new HttpClient(getUrl("ctrl"))) {
                read(http);
                if (i > 5) {
                    sum += ts.diff().getTime(Unit.MILLISECONDS);
                    ++count;
                }
            }

        }

        double mid = (double) (sum) / (double) count;

        System.out.println(new Interval((long) mid, Unit.MILLISECONDS));

    }

    @Endpoint(url = "ctrl", title = "Bla", auth = false)
    static class Ctrl implements Controller {

        @Override
        public void onRequest(HttpRequest http) throws Exception {
            http.returnPlainText("OK");
        }

    }
}
