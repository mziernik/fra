package com.lang.core;

import com.config.CService;
import com.dev.Dev;
import com.lang.core.Language.LangEntry;
import com.utils.Utils;
import com.utils.Is;
import com.servlet.controller.Controller;
import com.servlet.requests.HttpRequest;
import com.thread.ThreadObject;
import com.webapi.core.WebApiRequest;
import java.util.stream.IntStream;

public interface LString extends CharSequence {

    /**
     *
     * @param message: String lub LString
     * @return
     */
    public static LString get(CharSequence message) {
        return null;
        /*
        return message instanceof LString
                ? (LString) message
                : new LStr(message != null ? message.toString() : null);*/
    }

    static String format(String message, Object... params) {
        if (message == null || params == null || params.length == 0)
            return message;

        message = message.replace("%%", "%");
        for (int i = 0; i < params.length; i++)
            message = message.replace("%" + (i + 1), Utils.coalesce(Utils.toString(params[i]), "<null>"));

        return message;
    }

    default String toString(Object... params) {
        return toStringS((Language) null, params);
    }

    default String toStringS(Language language, Object... params) {
        return format(getValue(language), params);
    }

    default String toStringS(HttpRequest request, Object... params) {
        return toStringS(request != null && request.session != null
                ? request.session.language.get() : null, params);
    }

    default String toStringS(Controller controller, Object... params) {
        return toStringS(controller.http(), params);
    }

    default String toStringS(WebApiRequest req, Object... params) {
        return format(getValue(req.controller.language.get()), params);
    }

    default String getValue(Language lang) {

        if (lang == null)
            lang = ThreadObject.language.get();

        if (lang == null)
            lang = CService.language.value();

        LanguageItem item = null;
        if (this instanceof LStr) {
            LStr lstr = (LStr) this;
            item = LStr.items.get(lstr.value);
            if (item == null || item.isEmpty())
                return lstr.value.toString();
        } else
            item = Languages.lstrItems.get(this);

        if (item == null)
            throw new RuntimeException("Language entry not found, class " + getClass().getName());

        LangEntry en = item.get(lang);

        if (en == null)
            Dev.warning(String.format("Language entry \"%s\" not found for language %s",
                    item.toString(), lang.key));

        if (en != null)
            return en.value;

        for (LangEntry e : item)
            if (e.language == Languages.en)
                return e.value;

        if (this instanceof Enum)
            return ((Enum) this).name();

        return null;
    }

    @Override
    default IntStream chars() {
        return this.toString().chars();
    }

    @Override
    default IntStream codePoints() {
        return this.toString().codePoints();
    }

    @Override
    default int length() {
        return this.toString().length();
    }

    @Override
    default char charAt(int index) {
        return this.toString().charAt(index);
    }

    @Override
    default CharSequence subSequence(int start, int end) {
        return this.toString().subSequence(start, end);
    }

}
