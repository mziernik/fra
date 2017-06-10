package com.config;

import com.config.engine.ConfigElement;
import com.config.engine.ConfigNode;
import com.config.engine.field.*;
import com.context.AppConfig;
import com.extensions.email.EmailAddress;
import com.config.engine.interfaces.Cfg;
import com.utils.Is;
import static com.lang.LConfig.*;

public class CAdministration extends ConfigNode {

    public CAdministration() {
        super(CService.class, null, ADMINISTRATION__ADMINISTRATION);
    }

    @Cfg
    public final static CfEmailRecipients emailList = new CfEmailRecipients("email_list",
            ADMINISTRATION__EMAIL_RECIPIENTS)
            .required(false);

    public static class CSMTP extends ConfigNode {

        private static CSMTP instance;

        public CSMTP() {
            super(CAdministration.class, "smtp", SMTP__SMTP);
            instance = this;
            enabled(false);
            description(SMTP__SMTP__DESCRIPTION);

            onAfterChange(this, (item, isUserValue, newValue) -> onInitialize());
        }

        @Override
        protected void onInitialize() {
            instance.enabled(!host.isEmpty() && !port.isEmpty() && !username.isEmpty());
            for (ConfigElement el : this)
                el.enabled(instance.enabled());
        }

        public static boolean isEnabled() {
            return instance != null && instance.enabled();
        }

        @Cfg
        public final static CfString host = new CfString("host",
                HOST, null)
                .required(false)
                .onBeforeChange(CAdministration.class, (item, isUserValue, oldValue, newValue) -> {
                    boolean req = !Is.empty(newValue);
                    CSMTP.username.required(req).enabled(!req);
                    CSMTP.password.required(req).enabled(!req);
                    CSMTP.senderEmail.required(req).enabled(!req);
                    CSMTP.senderName.required(req).enabled(!req);
                    return true;
                });

        @Cfg
        public final static CfInt port = new CfInt("port", PORT, 25);

        @Cfg
        public final static CfString username = new CfString("username", USER, null)
                .required(false);

        @Cfg
        public final static CfPassword password = new CfPassword("password", PASS, null)
                .required(false);

        @Cfg
        public final static CfString senderEmail = new CfString("sender.email", SMTP__SENDER, null)
                .required(false);

        @Cfg
        public final static CfString senderName = new CfString("sender.name", SMTP__NAME,
                null).required(false);

        public static String getSenderName() {
            String sender = senderName.value();
            if (sender == null || sender.isEmpty())
                return AppConfig.getServiceTitle();
            return sender;
        }

        public static EmailAddress getSender() {
            return new EmailAddress(null, senderEmail.value(), getSenderName());
        }

    }

    public static class CRegistration extends ConfigNode {

        public CRegistration() {
            super(CAdministration.class, "registration", REGISTRATION__REGISTRATION);
        }
        @Cfg
        public final static CfBool enabled = new CfBool("enabled",
                ENABLED,
                false);

        @Cfg
        public final static CfBool emailVerification = new CfBool("email_verification",
                REGISTRATION__EMAIL_VERIFICATION,
                true);

        @Cfg
        public final static CfBool adminVerification = new CfBool("admin_verification",
                REGISTRATION__ADMIN_VERIFICATION,
                true);

        @Cfg
        public final static CfBool passwordRequired = new CfBool("pass_required",
                REGISTRATION__PASSWORD_REQUIRED, null)
                .required(false);

        @Cfg
        public final static CfBoolStringList groups = new CfBoolStringList("groups",
                REGISTRATION__GROUPS,
                ENABLED, GROUP
        ).required(false);

        public static boolean passwordRequired() {
            return passwordRequired.value(!CAuthorization.CLDAP.enabled.value(false));
        }
    }
}
