/*
 */
package service;

import com.context.unit_test.WebAppServerRequired;
import com.exceptions.http.HttpException;
import com.net.HttpClient;
import com.servlet.controller.Controller;
import com.servlet.interfaces.Arg;
import com.servlet.interfaces.Endpoint;
import com.servlet.requests.HttpRequest;
import context.ServiceTest;
import java.io.IOException;
import java.net.*;
import java.sql.SQLException;
import java.util.*;
import org.junit.*;

/**
 *
 * @author milosz
 */
public class Controllers extends ServiceTest implements WebAppServerRequired {

    static {
        registerController(CArguments.class);
    }

    @Test
    public void range() throws InterruptedException, SQLException, MalformedURLException, IOException, HttpException {
        /*
         try (HttpClient http = getHttp("evals")) {
         String read = http.readStr(Utils.UTF8);
         Assert.assertEquals("abc", read);
         }
         */
        try (HttpClient http = new HttpClient(
                getUrl("evals")
                        //                .param("number", 11)
                        //                .param("number", 22)
                        //                .param("number", 99999)
                        .param("rec", "{name: 'bla', count: 1, date: '2012-05-11 10:10:10.111'}")
                        .param("rec", "{name: 'abc', count: 331, date: '2011-01-11 22:22:22.33'}")
                        .param("fruit", "cherry")
                        .param("fruit", "apple")
                        .param("fruit", "banana")
        )) {

            http.setReadTimeout(60000);
            Assert.assertEquals("abc", read(http));
        }
    }
}

@Endpoint(url = "evals", auth = false, title = "Bla")
class CArguments implements Controller {

    @Arg(name = "number")
    Integer int1 = null;

    @Arg(name = "number")
    int[] arrNumbers;

    @Arg(name = "number")
    HashSet<Double> setNumbers;

    @Arg(name = "number")
    ArrayList<Integer> listNumbers;

    @Arg(name = "number")
    ArrayList<String> listStr;

    @Arg(name = "rec")
    Record record;

    @Arg(name = "fruit")
    Fruit fruit;

    @Arg(name = "fruit")
    Fruit[] fruits;

    @Arg(name = "rec")
    Record[] records;

    @Endpoint
    public void args(
            @Arg(name = "number") Integer int1,
            @Arg(name = "number") int[] arrNumbers,
            @Arg(name = "number") HashSet<Double> setNumbers,
            @Arg(name = "number") ArrayList<Integer> listNumbers,
            @Arg(name = "number") ArrayList<String> listStr,
            @Arg(name = "rec") Record record,
            @Arg(name = "fruit") Fruit fruit,
            @Arg(name = "fruit") Fruit[] fruits,
            @Arg(name = "rec") Record[] records
    ) {
        http().returnPlainText("abc");

    }

    @Override
    public void onRequest(HttpRequest http) throws Exception {
        http.returnPlainText("abc");

    }

}

class Record {

    public String name;
    public int count;
    public Date date;
}

enum Fruit {
    apple,
    orange,
    banana,
    strawberry,
    cherry
}
