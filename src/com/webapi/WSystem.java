package com.webapi;

import com.context.Environment;
import com.json.JElement;
import com.json.JValue;
import com.lang.core.LStr;
import com.utils.Execute;
import com.utils.TObject;
import com.webapi.core.*;
import java.io.*;
import javax.websocket.CloseReason;

public class WSystem implements WebApi {

    public WSystem() {

    }

    @WebApiEndpoint(async = true, description = "cmd / bash")
    public Integer shell(WebApiRequest request) throws IOException {

        Execute exec = new Execute(Environment.isWindows ? "cmd" : "bash");

        final TObject<BufferedWriter> writer = new TObject<>();

        request.webSocket.onClose.listen(this, (CloseReason arg) -> exec.destroy());

        request.onEvent((String event, JElement data) -> {
            if (!writer.isNull()) {
                BufferedWriter wr = writer.get();
                wr.write(data.asString());
                if (Environment.isWindows)
                    wr.write("\r");
                wr.write("\n");
                wr.flush();
            }
        });

        exec.onStart((Process arg) -> {
            BufferedWriter wr = new BufferedWriter(new OutputStreamWriter(exec.getOutputStream()));
            writer.set(wr);
        });

        exec.onLineReaded((Boolean error, String line) -> {
            request.event("shell", error ? "error" : "line", new JValue(line));
        });

        exec.run((Execute process, Execute.ExecResult result) -> {
            request.responseCommit(result.exitValue);
        });

        return null;
    }

}
