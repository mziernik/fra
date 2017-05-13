package com.servlet;

import com.config.engine.HConfig;
import com.user.BaseUserConfig;
import com.user.BaseAuthHandler;
import com.user.BaseUsersHandler;
import com.user.BaseUserData;
import com.user.LocalUsersHandler;
import com.user.BaseLdap;
import com.resources.core.Resources;
import com.servlet.controller.BaseSession;
import com.utils.reflections.Reflections;
import com.database.DbHandler;
import com.exceptions.CoreException;
import com.context.EventsHandler;
import com.exceptions.ErrorHandler;
import com.lang.LServlet;
import com.lang.core.HLanguage;
import com.servlet.handlers.*;
import com.servlet.requests.RequestFilter;
import com.user.pages.PLogin;
import com.utils.reflections.TClass;
import java.lang.reflect.*;

public class Handlers {

    public final static IHnd<Resources> resources = new IHnd<>(new Resources());
    public final static IHnd<BaseUsersHandler> users = new IHnd<>(new LocalUsersHandler());
    public final static CHnd<BaseUserData> userData = new CHnd<>(BaseUserData.class);
    public final static CHnd<BaseUserConfig> userConfig = new CHnd<>(BaseUserConfig.class);
    public final static CHnd<BaseSession> session = new CHnd<>(BaseSession.class);
    public final static CHnd<BaseLdap> ldap = new CHnd<>(BaseLdap.class);
    public final static CHnd<BaseAuthHandler> auth = new CHnd<>(BaseAuthHandler.class);
    public final static IHnd<HRequests> requests = new IHnd<>(new HRequests());
    public final static CHnd<PLogin> loginPage = new CHnd<>(PLogin.class);
    public final static IHnd<EventsHandler> events = new IHnd<>(new EventsHandler());
    public final static IHnd<DbHandler> database = new IHnd<>(new DbHandler());
    public final static IHnd<ErrorHandler> errors = new IHnd<>(new ErrorHandler());
    public final static IHnd<RequestFilter> requestFilter = new IHnd<>(new RequestFilter());
    public final static IHnd<HConfig> config = new IHnd<>(HConfig.class);
    public final static IHnd<HLanguage> language = new IHnd<>(HLanguage.class);

    // handler o wielu instancjach
    public static class CHnd<H> extends Hnd<H> {

        public final Class<? extends H> defaultClass;

        protected Class<? extends H> handlerClass;

        public CHnd(Class<? extends H> defaultClass) {
            this.defaultClass = defaultClass;
        }

        public Hnd<H> setHandler(Class<? extends H> handlerClass) {
            this.handlerClass = handlerClass;
            return this;
        }

        public H newInstance(Object... parameters) {
            return new TClass<H>(getHandlerClass()).newInstance(null, parameters);
        }

        public Constructor<? extends H> getConstructor(Class<?>... parameterTypes) {
            try {
                return getHandlerClass().getConstructor(parameterTypes);
            } catch (NoSuchMethodException | SecurityException ex) {
                throw new CoreException(ex);
            }
        }

        public Class<? extends H> getHandlerClass() {

            if (handlerClass == null && defaultClass == null)
                throw new CoreException(LServlet.HANDLER_DEF_NOT_FOUND.toString(
                        getFieldName()));

            return handlerClass != null ? handlerClass : defaultClass;
        }

        public H getInstance(Object... args) {
            if (handlerClass != null)
                return new TClass<H>(handlerClass).newInstance(null, args);

            if (defaultClass != null)
                return new TClass<H>(defaultClass).newInstance(null, args);

            throw new CoreException(LServlet.HANDLER_DEF_NOT_FOUND.toString(
                    getFieldName()));
        }
    }

    // handler typu singleton
    public static class IHnd<H> extends Hnd<H> {

        public final H defaultInstance;
        public final Class<? extends H> defaultClass;
        protected H handlerInstance;

        public IHnd(H defaultInstance) {
            this.defaultInstance = defaultInstance;
            this.defaultClass = null;
        }

        public IHnd(Class<? extends H> defaultClass) {
            this.defaultInstance = null;
            this.defaultClass = defaultClass;
        }

        public Hnd<H> setHandler(H handlerInstance) {
            this.handlerInstance = handlerInstance;
            return this;
        }

        public H getInstance() {
            if (handlerInstance != null)
                return handlerInstance;

            if (defaultInstance != null)
                return defaultInstance;

            if (defaultClass != null)
                return handlerInstance = new TClass<>(defaultClass).newInstance(null);

            throw new CoreException(LServlet.HANDLER_DEF_NOT_FOUND.toString(
                    getFieldName()));
        }
    }

    public abstract static class Hnd<H> {

        protected String getFieldName() {
            for (Field f : Handlers.class.getFields()) {
                if (!Reflections.hasModifires(f.getModifiers(),
                        Modifier.PUBLIC, Modifier.STATIC, Modifier.FINAL))
                    continue;

                try {
                    Object obj = f.get(null);

                    if (obj == this)
                        return Handlers.class.getName() + "." + f.getName();
                } catch (Exception ex) {
                }

            }
            throw new RuntimeException();
        }

    }
}
