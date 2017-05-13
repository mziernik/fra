package com.config.engine.field;

import com.config.engine.cell.select.CcEnum;
import com.config.engine.cell.CcString;
import com.config.engine.MultipleConfigFields;
import com.extensions.email.*;

import com.utils.collections.Pair;
import static com.lang.LConfig.*;

public class CfEmailRecipients extends MultipleConfigFields<CfEmailRecipients, Pair<EmailType, String>> {

    public CfEmailRecipients(String key, CharSequence name) {
        super(key, name, new Pair[0],
                new CcEnum<>(EmailType.class, TYPE).defaultValue(EmailType.TO),
                new CcString(RECIPIENT));
        unique = true;

    }

    /*
    public CEmailRecipients(final String id) {
        super(id, true,
                new EEnum(EmailType.class, "Typ", ItemsMode.combo, EmailType.TO),
                new EString("E-Mail", null)
        );

        ((EEnum) elements[0]).setFields(EmailType.class, "caption", EmailType.values());
    }

    public void checkNonEmpty() {
        boolean hasRecipients = false;
        for (Pair<EmailType, String> r : values())
            hasRecipients |= r.first != EmailType.NONE && r.second != null && !r.second.isEmpty();

        if (!hasRecipients)
            throw new EmailError("Pusta grupa mailowa: " + getNodeNamePath().toString(" / "));
    }
     */
}
