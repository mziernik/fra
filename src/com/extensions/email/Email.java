package com.extensions.email;

import com.resources.dict.MimeMappings;
import com.config.CAdministration;
import com.config.CAdministration.CSMTP;
import com.config.engine.field.CfEmailRecipients;
import com.context.*;
import com.exceptions.EError;
import com.html.core.Html;
import com.html.core.tag.formatting.Code;
import com.lang.LExtensions;
import com.mlogger.Log;
import com.utils.collections.*;
import com.utils.text.StrWriter;
import java.io.*;
import java.util.*;

public class Email {

    public static class EmailException extends Error {

        public EmailException(Throwable cause) {
            super(cause);
        }

        public EmailException(String message) {
            super(message);
        }

    }

    public final javax.mail.internet.MimeMessage message;

    private final Strings recipientsGroupsNames = new Strings().unique(true);

    public Email() {

        Properties props = new Properties();
        props.setProperty("mail.transport.protocol", "smtp");
        props.setProperty("mail.host", CSMTP.host.value());
        props.setProperty("mail.port", Integer.toString(CSMTP.port.value()));
        props.setProperty("mail.smtp.auth", "true");

        javax.mail.Session mailSession = javax.mail.Session.getDefaultInstance(props,
                new javax.mail.Authenticator() {
            @Override
            public javax.mail.PasswordAuthentication getPasswordAuthentication() {
                return new javax.mail.PasswordAuthentication(CSMTP.username.value(),
                        CSMTP.password.value());
            }
        });

        message = new javax.mail.internet.MimeMessage(mailSession);
        setFrom(CSMTP.getSender());
        try {
            message.setSentDate(new Date());
        } catch (javax.mail.MessagingException ex) {
            throw new EmailException(ex);
        }
    }

    public Email setFrom(EmailAddress address) {
        try {
            message.setFrom(address.getAddr());
        } catch (javax.mail.MessagingException ex) {
            throw new EmailException(ex);
        }
        return this;
    }

    public Email addRecipient(EmailAddress address) {

        try {
            if (address != null && address.type != null)
                switch (address.type) {
                    case TO:
                        message.addRecipient(javax.mail.Message.RecipientType.TO,
                                address.getAddr());
                        break;
                    case CC:
                        message.addRecipient(javax.mail.Message.RecipientType.CC,
                                address.getAddr());
                        break;
                    case BCC:
                        message.addRecipient(javax.mail.Message.RecipientType.BCC,
                                address.getAddr());
                        break;
                }
            return this;
        } catch (javax.mail.MessagingException ex) {
            throw new EmailException(ex);
        }
    }

    public Email addRecipient(EmailType type, String address) {
        return addRecipient(type, address, null);
    }

    public Email addRecipient(EmailType type, String address, String recipientsGroupName) {

        if (type == null || address == null || address.trim().isEmpty())
            return this;

        addRecipient(new EmailAddress(type, address));

        if (recipientsGroupName != null && !recipientsGroupName.isEmpty())
            recipientsGroupsNames.add(recipientsGroupName);
        return this;
    }

    public Email addRecipients(CfEmailRecipients... recipients) {
        if (recipients != null)
            for (CfEmailRecipients recs : recipients)
                if (recs != null)
                    for (Pair<EmailType, String> rec : recs.getValue(null))
                        addRecipient(rec.first, rec.second); // recs.getNodeNamePath().toString(" / ")
        return this;
    }

    public Email(CfEmailRecipients... recipients) {
        this();
        addRecipients(recipients);
    }

    private final javax.mail.Multipart multiPart = new javax.mail.internet.MimeMultipart();

    public void send() {
        try {
            message.setContent(multiPart);
            if (message.getAllRecipients() == null || message.getAllRecipients().length == 0) {
                Log.warning("Brak zdefiniowanych adresów e-mail"
                        + (recipientsGroupsNames.isEmpty() ? ""
                        : " grupy " + recipientsGroupsNames.toString()));
                return;
            }

            Strings recps = new Strings();
            if (message.getAllRecipients() != null)
                for (javax.mail.Address a : message.getAllRecipients())
                    recps.add(a.toString());

            Log.debug("Email", "Wysyłam e-maila \"" + message.getSubject() + "\" do " + recps);

            javax.mail.Transport.send(message);
        } catch (javax.mail.SendFailedException e) {

            if (e.getNextException() != null)
                Log.warning(e.getNextException());

            javax.mail.Address[] invalidAddresses = e.getInvalidAddresses();
            if (invalidAddresses == null)
                throw new EmailError(e);

            String addr = "";
            for (javax.mail.Address a : invalidAddresses) {
                if (!addr.isEmpty())
                    addr += ", ";
                addr += a.toString();
            }

            throw new EmailError(LExtensions.INVALID_ADDRESS.toString(addr), e);
        } catch (javax.mail.MessagingException e) {
            throw new EmailError(e);
        }
    }

    public void send(String title, Html html) {
        setTitile(title);
        send(html);
    }

    public void send(Html html) {
        try {
            javax.mail.internet.MimeBodyPart part = newAttachment(true);
            StrWriter str = new StrWriter();
            html.getContent(str, false, true);
            part.setContent(str.toString(), "text/html; charset=UTF-8");
            send();
        } catch (javax.mail.MessagingException ex) {
            throw new EmailError(ex);
        }
    }

    public void send(String plainText) {
        try {
            javax.mail.Part part = multiPart.getCount() == 0
                    ? new javax.mail.internet.MimeMessage(message)
                    : newAttachment();
            part.setContent(plainText, "text/plain; charset=UTF-8");
        } catch (javax.mail.MessagingException ex) {
            throw new EmailError(ex);
        }
    }

    public javax.mail.internet.MimeBodyPart newAttachment() {
        return newAttachment(false);
    }

    public javax.mail.internet.MimeBodyPart newAttachment(boolean first) {
        try {
            javax.mail.internet.MimeBodyPart part = new javax.mail.internet.MimeBodyPart();
            if (first)
                multiPart.addBodyPart(part, 0);
            else
                multiPart.addBodyPart(part);
            return part;
        } catch (javax.mail.MessagingException e) {
            throw new EmailError(e);
        }
    }

    public Email setTitile(String title) {
        try {
            message.setSubject(title);
            return this;
        } catch (javax.mail.MessagingException e) {
            throw new EmailError(e);
        }
    }

    public static void sendError(Throwable error) {
        try {
            Html html = new Html();
            html.body.h4(EError.toString(error, true)).style().fontSize("12pt");
            Code code = html.body.code();
            code.style().fontSize("8pt").color("#888").marginTop("30px");
            code.ul().addIems(EError.getStackTraceStr(error));
            Email email = new Email();
            email.setTitile(AppConfig.getServiceTitle() + ": " + EError.toString(error, false));
            email.addRecipients(CAdministration.emailList);
            email.send(html);
        } catch (Throwable e) {
            Log.error(e);
        }
    }

    public Email addAttachment(String fileName, InputStream in) throws IOException {
        return addAttachment(fileName, in, null);
    }

    public Email addAttachment(String fileName, InputStream in, String contentType) throws IOException {
        if (contentType == null)
            contentType = MimeMappings.get(fileName);
        javax.mail.internet.MimeBodyPart part;
        try {
            part = newAttachment();
            javax.mail.util.ByteArrayDataSource ds = new javax.mail.util.ByteArrayDataSource(in, contentType);
            part.setDataHandler(new javax.activation.DataHandler(ds));
            part.setFileName(fileName);
        } catch (javax.mail.MessagingException ex) {
            throw new EmailError(ex);
        }
        return this;
    }

    public Email addAttachment(String fileName, byte[] in) {
        return addAttachment(fileName, in, null);
    }

    public Email addAttachment(String fileName, byte[] in, String contentType) {
        if (contentType == null)
            contentType = MimeMappings.get(fileName);
        try {
            javax.mail.internet.MimeBodyPart part = newAttachment();
            javax.mail.util.ByteArrayDataSource ds = new javax.mail.util.ByteArrayDataSource(in, contentType);
            part.setDataHandler(new javax.activation.DataHandler(ds));
            part.setFileName(fileName);
        } catch (javax.mail.MessagingException ex) {
            throw new EmailError(ex);
        }
        return this;
    }

}
