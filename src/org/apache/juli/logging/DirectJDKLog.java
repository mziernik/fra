package org.apache.juli.logging;

import com.servers.tomcat.Juli;

class DirectJDKLog implements Log {

    public Juli logger;

    public DirectJDKLog(String name) {
        logger = new Juli(name);
    }

    @Override
    public final boolean isErrorEnabled() {
        return true;
    }

    @Override
    public final boolean isWarnEnabled() {
        return true;
    }

    @Override
    public final boolean isInfoEnabled() {
        return true;
    }

    @Override
    public final boolean isDebugEnabled() {
        return true;
    }

    @Override
    public final boolean isFatalEnabled() {
        return true;
    }

    @Override
    public final boolean isTraceEnabled() {
        return true;
    }

    @Override
    public final void debug(Object message) {
        logger.debug(message, null);
    }

    @Override
    public final void debug(Object message, Throwable t) {
        logger.debug(message, t);
    }

    @Override
    public final void trace(Object message) {
        logger.trace(message, null);
    }

    @Override
    public final void trace(Object message, Throwable t) {
        logger.trace(message, t);
    }

    @Override
    public final void info(Object message) {
        logger.info(message, null);
    }

    @Override
    public final void info(Object message, Throwable t) {
        logger.info(message, t);
    }

    @Override
    public final void warn(Object message) {
        logger.warn(message, null);
    }

    @Override
    public final void warn(Object message, Throwable t) {
        logger.warn(message, t);
    }

    @Override
    public final void error(Object message) {
        logger.error(message, null);
    }

    @Override
    public final void error(Object message, Throwable t) {
        logger.error(message, null);
    }

    @Override
    public final void fatal(Object message) {
        logger.fatal(message, null);
    }

    @Override
    public final void fatal(Object message, Throwable t) {
        logger.fatal(message, t);
    }

    // for LogFactory
    static void release() {

    }

    static Log getInstance(String name) {
        return new DirectJDKLog(name);
    }
}
