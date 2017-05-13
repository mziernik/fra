package com.extensions.email;

import java.util.Objects;

/**
 * @author Miłosz Ziernik
 * @date 18 września 2015
 * @encoding UTF-8
 */
public class EmailAddress {

    String name;
    String address;
    EmailType type;

    public EmailAddress(EmailType type, String address) {
        this(type, address, null);
    }

    public EmailAddress(EmailType type, String address, String name) {
        this.name = name;
        this.address = address;
        this.type = type;
    }

    javax.mail.internet.InternetAddress getAddr() {
        try {
            if (name != null)
                return new javax.mail.internet.InternetAddress(address, name);
            return new javax.mail.internet.InternetAddress(address);
        } catch (Exception ex) {
            throw new EmailError(ex);
        }
    }

    @Override
    public boolean equals(Object obj) {
        return super.equals(obj);
    }

    @Override
    public int hashCode() {
        int hash = 5;
        hash = 29 * hash + Objects.hashCode(this.name);
        hash = 29 * hash + Objects.hashCode(this.address);
        hash = 29 * hash + Objects.hashCode(this.type);
        return hash;
    }

}
