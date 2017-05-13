
import com.utils.date.time.TTime;
import java.io.IOException;
import java.util.Date;
import org.junit.Test;

public class Ant {

    @Test
    public void hello() {
        System.out.println(new TTime().intValue());

        TTime tTime = new TTime(new Date());
        System.out.println(tTime.toString());

        System.out.println(tTime.toStringFrmtS());
    }
}
