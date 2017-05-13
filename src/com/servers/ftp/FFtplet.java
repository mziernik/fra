package com.servers.ftp;

import com.utils.console.TConsole;
import java.io.IOException;
import org.apache.ftpserver.ftplet.*;

/**
 *
 * @author milosz
 */
public class FFtplet implements Ftplet {

    @Override
    public void init(FtpletContext ftpletContext) throws FtpException {
        TConsole.print("init");
        TConsole.print("Thread #" + Thread.currentThread().getId());
    }

    @Override
    public void destroy() {
        TConsole.print("destroy");
        TConsole.print("Thread #" + Thread.currentThread().getId());
    }

    @Override
    public FtpletResult beforeCommand(FtpSession session, FtpRequest request) throws FtpException, IOException {
        TConsole.print("beforeCommand " + session.getUserArgument() + " : "
                + session.getSessionId() + " | " + request.getArgument() + " : "
                + request.getCommand() + " : " + request.getRequestLine());
        //   TConsole.print("Thread #" + Thread.currentThread().getId());

        //do something
        return FtpletResult.DEFAULT;//...or return accordingly
    }

    @Override
    public FtpletResult afterCommand(FtpSession session, FtpRequest request, FtpReply reply) throws FtpException, IOException {
        TConsole.print("afterCommand " + session.getUserArgument() + " : "
                + session.getSessionId() + " | " + request.getArgument() + " : " + request.getCommand() + " : " + request.getRequestLine() + " | " + reply.getMessage() + " : " + reply.toString());
        //TConsole.print("Thread #" + Thread.currentThread().getId());

        //do something
        return FtpletResult.DEFAULT;//...or return accordingly
    }

    @Override
    public FtpletResult onConnect(FtpSession session) throws FtpException, IOException {
        TConsole.print("onConnect " + session.getUserArgument() + " : " + session.getSessionId());
        //TConsole.print("Thread #" + Thread.currentThread().getId());

        //do something
        return FtpletResult.DEFAULT;//...or return accordingly
    }

    @Override
    public FtpletResult onDisconnect(FtpSession session) throws FtpException, IOException {
        TConsole.print("onDisconnect " + session.getUserArgument() + " : " + session.getSessionId());
        //TConsole.print("Thread #" + Thread.currentThread().getId());

        //do something
        return FtpletResult.DEFAULT;//...or return accordingly
    }

}
