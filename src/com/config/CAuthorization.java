package com.config;

import com.config.engine.ConfigNode;
import com.config.engine.field.*;
import com.config.engine.cell.CcString;
import com.config.engine.ValueSource;
import com.servers.Connector;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import com.config.engine.interfaces.Cfg;
import static com.lang.LConfig.*;

public class CAuthorization extends ConfigNode {

    public CAuthorization() {
        super(CService.class, null, AUTH__AUTH);
    }

    @Cfg
    public final static CfBool ssl = new CfBool("ssl", AUTH__SSL, null)
            .required(false)
            .onGetValue((CfBool item, Boolean value, ValueSource source)
                    -> value == null ? Connector.get(true) != null : value);

    @Cfg
    public final static CfInterval cookieExpire = new CfInterval("cookie_expire",
            AUTH__COOKIE_EXPIRE,
            new Interval(48, Unit.HOURS));

    @Cfg
    public final static CfBool htAccess = new CfBool("use_htaccess",
            AUTH__USE_HTACCESS,
            false);

    @Cfg
    public final static CfPairList<String, String> autoLogin = new CfPairList<>("auto_login",
            AUTH__AUTO_LOGIN,
            new CcString(IP),
            new CcString(LOGIN));

    public static class CLDAP extends ConfigNode {

        public CLDAP() {
            super(CAuthorization.class, "ldap", LDAP__LDAP);
        }
        @Cfg
        public final static CfBool enabled = new CfBool("active",
                ACTIVE, false);

        @Cfg
        public final static CfString url = new CfString("url",
                URL, "ldap://localhost:389")
                .description(LDAP__URL__DESCRIPTION); // o=kolporter,c=pl

        @Cfg
        public final static CfString domain = new CfString("domain",
                DOMAIN, "domain"); // o=kolporter,c=pl

        @Cfg
        public final static CfString adminUsername = new CfString("admin_username",
                LDAP__ADMIN_LOGIN, null)
                .required(false);
        @Cfg
        public final static CfPassword adminPassword = new CfPassword("admin_password",
                LDAP__ADMIN_PASS, null)
                .required(false);

        @Cfg
        public final static CfInterval timeout = new CfInterval("timeout",
                TIMEOUT, new Interval(2, Unit.SECONDS))
                .required(true);

        @Cfg
        public final static CfString listUsersFilter = new CfString("list_users_filter",
                LDAP__LIST_USERS_FILTER, "(objectClass=person)")
                .description(LDAP__LIST_USERS_FILTER__DESCRIPTION);

    }
}
