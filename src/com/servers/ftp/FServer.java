package com.servers.ftp;

import java.util.HashMap;
import java.util.Map;
import org.apache.ftpserver.FtpServer;
import org.apache.ftpserver.FtpServerFactory;
import org.apache.ftpserver.command.Command;
import org.apache.ftpserver.command.CommandFactory;
import org.apache.ftpserver.ftplet.*;
import org.apache.ftpserver.ipfilter.SessionFilter;
import org.apache.ftpserver.listener.ListenerFactory;
import org.apache.mina.core.session.IoSession;

public class FServer {

    public FServer() throws FtpException {
        FtpServerFactory server = new FtpServerFactory();
        ListenerFactory listener = new ListenerFactory();

        listener.setSessionFilter((IoSession is) -> {
            return true;
        });

// set the port of the listener 
        listener.setPort(2121);
        /*
        SslConfigurationFactory ssl = new SslConfigurationFactory();
        ssl.setKeystoreFile(new File("src/test/resources/ftpserver.jks"));
        ssl.setKeystorePassword("password");
// set the SSL configuration for the listener
        factory.setSslConfiguration(ssl.createSslConfiguration());
        factory.setImplicitSsl(true);
        
         */

// replace the default listener 
        server.addListener("default", listener.createListener());

        UserManager userManager = new FtpUserManager();

        server.setUserManager(userManager);

        Map<String, Ftplet> m = new HashMap<>();
        m.put("miaFtplet", new FFtplet());

        server.setFtplets(m);

        server.setFileSystem(FtpFileSystemView::new);

        CommandFactory fact = server.getCommandFactory();
        server.setCommandFactory((String string) -> {
            Command cmd = fact.getCommand(string);
            return cmd;
        });

// start the server 
        server.createServer().start();

    }

}
