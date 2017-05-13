package com.html.core;

import com.mlogger.Log;
import com.vaadin.sass.internal.ScssStylesheet;
import com.vaadin.sass.internal.handler.SCSSDocumentHandlerImpl;
import com.vaadin.sass.internal.handler.SCSSErrorHandler;
import com.vaadin.sass.internal.parser.Parser;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import org.w3c.css.sac.InputSource;

public class Scss {

    public static String process(String fileName, String source, boolean minify) throws IOException {

        Log.trace("SCSS", "Kompilacja pliku " + fileName);

        SCSSErrorHandler errorHandler = new SCSSErrorHandler();
        //  errorHandler.setWarningsAreErrors(!ignoreWarnings);

        SCSSDocumentHandlerImpl handler = new SCSSDocumentHandlerImpl();
        Parser parser = new Parser();
        parser.setDocumentHandler(handler);

        parser.parseStyleSheet(new InputSource(new StringReader(source)));

        ScssStylesheet styleSheet = handler.getStyleSheet();
        styleSheet.setCharset("UTF-8");

        try {
            styleSheet.compile();
        } catch (Error | RuntimeException e) {
            throw e;
        } catch (Exception ex) {
            throw new IOException(fileName, ex);
        }
        StringWriter sw = new StringWriter();
        sw.append("/*# sourceURL=file:///").append(fileName).append("*/\n");
        styleSheet.write(sw, minify);

        return sw.toString();
    }

}
