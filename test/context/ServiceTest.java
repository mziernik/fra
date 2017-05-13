package context;

import com.context.unit_test.AbstractServiceTest;
import com.context.unit_test.FraUnitTestContext;
import org.junit.Ignore;

@Ignore
public abstract class ServiceTest extends AbstractServiceTest<FraUnitTestContext> {

    protected ServiceTest() {
        super(FraUnitTestContext.class);
    }

}
