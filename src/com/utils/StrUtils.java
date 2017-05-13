package com.utils;

import com.utils.text.StrWriter;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.Random;

/**
 *
 * @author Miłosz Ziernik
 */
public final class StrUtils {

    private StrUtils() {
    }

    private static final String[][] htmlEscape = {{"&lt;", "<"}, {"&gt;", ">"},
    {"&amp;", "&"}, {"&quot;", "\""},
    {"&agrave;", "à"}, {"&Agrave;", "À"},
    {"&acirc;", "â"}, {"&auml;", "ä"},
    {"&Auml;", "Ä"}, {"&Acirc;", "Â"},
    {"&aring;", "å"}, {"&Aring;", "Å"},
    {"&aelig;", "æ"}, {"&AElig;", "Æ"},
    {"&ccedil;", "ç"}, {"&Ccedil;", "Ç"},
    {"&eacute;", "é"}, {"&Eacute;", "É"},
    {"&egrave;", "è"}, {"&Egrave;", "È"},
    {"&ecirc;", "ê"}, {"&Ecirc;", "Ê"},
    {"&euml;", "ë"}, {"&Euml;", "Ë"},
    {"&iuml;", "ï"}, {"&Iuml;", "Ï"},
    {"&ocirc;", "ô"}, {"&Ocirc;", "Ô"},
    {"&ouml;", "ö"}, {"&Ouml;", "Ö"},
    {"&oslash;", "ø"}, {"&Oslash;", "Ø"},
    {"&szlig;", "ß"}, {"&ugrave;", "ù"},
    {"&Ugrave;", "Ù"}, {"&ucirc;", "û"},
    {"&Ucirc;", "Û"}, {"&uuml;", "ü"},
    {"&Uuml;", "Ü"}, {"&nbsp;", " "},
    {"&copy;", "\u00a9"},
    {"&reg;", "\u00ae"},
    {"&euro;", "\u20a0"}
    };

    /**
     * Konwertuje tekst na standard zapisu metody camelCase, np: "To
     * jest_przykładowa-procedura" -> toJestPrzykladowaProcedura
     */
    public static String formatMethodName(String name) {
        return formatMethodName(name, false);
    }

    public static String formatMethodName(String name, boolean upper) {
        if (name == null)
            return null;

        final String[] keywords = {"abstract", "continue", "for", "new", "switch",
            "assert", "default", "goto", "package", "synchronized",
            "boolean", "do", "if", "private", "this", "break", "double",
            "implements", "protected", "throw", "byte", "else", "import",
            "public", "throws", "case", "enum", "instanceof", "return", "transient",
            "catch", "extends", "int", "short", "try", "char", "final",
            "interface", "static", "void", "class", "finally", "long", "strictfp",
            "volatile", "const", "float", "native", "super", "while"};

        name = convertPolishChars(name);

        StrWriter out = new StrWriter();

        for (int i = 0; i < name.length(); i++) {
            char c = name.charAt(i);

            if (c == '_' || c == '-' || c == ' ' || c == '.' || c == ',' || c == ';') {
                upper = true;
                continue;
            }

            if (upper) {
                c = Character.toUpperCase(c);
                upper = false;
            }

            if (c != '_'
                    && !(c >= 'A' && c <= 'Z')
                    && !(c >= 'a' && c <= 'z')
                    && !(i > 0 && c >= '0' && c <= '9'))
                c = '_';

            out.append(c);
        }

        String result = out.toString();

        for (String s : keywords)
            if (s.equals(result))
                return "_" + result;

        return result;
    }

    public static String formatFileName(String fileName) {

        if (fileName == null)
            return null;

        final String src = "ąćęśźńżółĄĆĘŚŹŃŻÓŁ";
        final String dest = "acesznzolACESZNZOL";
        final String reserved = "/\\?*:|\"<>|";

        String result = "";
        char c;
        boolean found;

        for (int i = 0; i < fileName.length(); i++) {
            c = fileName.charAt(i);
            found = false;

            for (int j = 0; j < src.length(); j++)
                if (c == src.charAt(j)) {
                    result += dest.charAt(j);
                    found = true;
                    break;
                }

            if (!found)
                for (int j = 0; j < reserved.length(); j++)
                    if (c == reserved.charAt(j)) {
                        result += ' ';
                        found = true;
                        break;
                    }

            if (!found)
                if (c > 127)
                    result += "_";
                else
                    result += c;
        }

        return result;
    }

    public static String convertPolishChars(String text) {

        final String src = "ąćęśźńżółĄĆĘŚŹŃŻÓŁ";
        final String dest = "acesznzolACESZNZOL";
        StrWriter res = new StrWriter();

        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            int v = src.indexOf(c);
            res.append(v >= 0 ? dest.charAt(v) : c);

        }
        return res.toString();
    }

    /**
     * Przycina zbyt dlugie nazwy plików dodając "… " przed rozszerzeniem (jesli
     * balance == null) lub w srodku jako "[… ]" jesli parametr balnace jest
     * zdefiniowany)
     */
    public static String trimFileName(String fileName, int maxLength) {
        return trimFileName(fileName, maxLength, 0.6d);
    }

    public static String trimFileName(String fileName, int maxLength, Double balance) {
        if (fileName == null)
            return fileName;

        if (balance != null && balance < 0)
            balance = 0d;
        if (balance != null && balance > 1)
            balance = 1d;

        fileName = fileName.trim();
        if (fileName.length() <= maxLength || maxLength < 4)
            return fileName;
        if (balance == null && fileName.indexOf(".") < 0)
            return fileName.substring(0, maxLength - 1).trim() + "… ";

        String ext = fileName.contains(".")
                ? fileName.substring(fileName.lastIndexOf("."), fileName.length())
                : "";
        if (balance == null) {
            fileName = fileName.substring(0, maxLength - ext.length() - 1).trim();
            return fileName.trim() + "… " + ext;
        }
        String left = fileName.substring(0, (int) (fileName.length() * balance)).trim();
        String right = fileName.substring(left.length(), fileName.length()).trim();
        int diff = 1 + (int) (Math.ceil((double) (left.length() + right.length()
                + ext.length() + 3) - maxLength) / 2d);
        if (diff < 0)
            diff = 0;

        left = left.substring(0, left.length() - diff);
        right = right.substring(diff, right.length());
        fileName = left + "[…]" + right;

        return fileName.trim() + ext;
    }

    public static String encodeURIComponent(String s) {
        if (s == null)
            return "";
        try {
            return URLEncoder.encode(s, "UTF-8").replaceAll("\\+", "%20");
        } catch (UnsupportedEncodingException e) {
            return "";
        }
    }

    public static String decodeURIComponent(String s) {
        if (s == null)
            return "";
        try {
            return URLDecoder.decode(s, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            return s;
        }
    }

    public static String unescapeHTML(String s) {
        return unescapeHTML(s, 0);
    }

    public static String unescapeHTML(String s, int start) {
        int i, j, k;

        i = s.indexOf("&", start);
        start = i + 1;
        if (i > -1) {
            j = s.indexOf(";", i);
            if (j > i) {
                String temp = s.substring(i, j + 1);
                k = 0;
                while (k < htmlEscape.length)
                    if (htmlEscape[k][0].equals(temp))
                        break;
                    else
                        k++;
                if (k < htmlEscape.length) {
                    s = s.substring(0, i)
                            + htmlEscape[k][1] + s.substring(j + 1);
                    return unescapeHTML(s, i); // recursive call
                }
            }
        }
        return s;
    }

    public static char toHex(int ch) {
        return (char) (ch < 10 ? '0' + ch : 'A' + ch - 10);
    }

    public static String generateText(int minLength, int maxLength) {

        final String charsA = "aeiou";
        final String charsB = "bcdfghjklmnpqrstwvxyz";
        final String plchars = "ąęćśńółźż";
        final String sspec = ".,:;!?-";

        Random rand = new Random();
        int len = minLength + rand.nextInt(maxLength - minLength);
        StrWriter sb = new StrWriter();

        int word = rand.nextInt(15);
        int ispec = 2 + rand.nextInt(8);

        char c = ' ';
        int space = 0;
        while (sb.length() < len) {
            int r = rand.nextInt();

            String src = r % 20 == 0 ? plchars
                    : r % 3 == 0 ? charsB
                            : r % 2 == 0 ? charsA
                                    : null;

            if (src == null)
                continue;

            char x = src.charAt(rand.nextInt(src.length()));

            if (c == x)
                continue;

            c = x;

            if (sb.length() == 0)
                c = Character.toUpperCase(c);

            if (space > word) {
                char sc = sspec.charAt(rand.nextInt(sspec.length()));

                if (sc == '-')
                    sb.append(' ');

                --ispec;
                if (ispec <= 0) {
                    ispec = rand.nextInt(6);
                    sb.append(sc);

                    boolean upper = sc == '.' || sc == '!' || sc == '?';

                    if (upper)
                        c = Character.toUpperCase(c);

                    if (upper && rand.nextBoolean()) {
                        space = 0;
                        sb.append('\n').append(c);
                        continue;
                    }
                }

                if (rand.nextInt(6) % 6 == 0)
                    c = Character.toUpperCase(c);

                sb.append(' ');
                space = 0;
                word = 2 + rand.nextInt(15);
            }

            sb.append(c);
            space++;
        }

        sb.append('.');

        return sb.toString();

    }

    /**
     * Usuń fragment ze stringa
     *
     * @param base
     * @param substring
     * @return
     */
    public static String remove(String base, String substring) {
        if (base == null
                || base.isEmpty()
                || substring == null
                || substring.isEmpty()
                || !base.contains(substring))
            return base;

        return base.substring(0, base.indexOf(substring))
                + base.substring(base.indexOf(substring)
                        + substring.length(), base.length());

    }

    public final static String getUniqueName(String base,
            Iterable<String> elements) {
        return getUniqueName(base, elements, "%base%%index%", true);
    }

    /**
     * Zwraca unikalną nazwę. Jeśli base występuje na liście, to dodawany jest
     * index i formatowany na podstawie maski
     *
     * @param base
     * @param elements
     * @param mask
     * @param caseSensitive
     * @return
     */
    public final static String getUniqueName(String base,
            Iterable<String> elements, String mask, boolean caseSensitive) {

        if (base == null || elements == null)
            return base;

        if (mask == null || mask.isEmpty())
            mask = "%base%%index%";

        if (!mask.contains("%base%"))
            throw new RuntimeException("Parametr mask nie zawiera zmiennej \"%base%\"");
        if (!mask.contains("%index%"))
            throw new RuntimeException("Parametr mask nie zawiera zmiennej \"%index%\"");

        String newName = base;
        boolean unique = false;
        int counter = 1;
        while (!unique) {
            unique = true;
            for (String s : elements)
                if (s != null)
                    if ((caseSensitive && s.equals(newName))
                            || (!caseSensitive && s.equalsIgnoreCase(newName))) {
                        unique = false;
                        break;
                    }

            if (unique)
                break;

            newName = mask.replace("%index%", "" + ++counter).replace("%base%", newName);
        }

        return newName;
    }

    public static String join(String separator, String... values) {
        if (values == null)
            return "";

        StrWriter sb = new StrWriter();
        for (String s : values) {
            if (sb.length() > 0)
                sb.append(separator);
            sb.append(s);
        }
        return sb.toString();
    }

    public static String[] split(String val) {
        if (val == null || val.isEmpty())
            return new String[0];
        return val.replace("\r\n", "|").replace("\n", "|").split("\\|");
    }

    public final static String loremIpsum = "Lorem ipsum dolor sit amet enim. "
            + "Etiam ullamcorper. Suspendisse a pellentesque dui, non felis. "
            + "Maecenas malesuada elit lectus felis, malesuada ultricies. "
            + "Curabitur et ligula. Ut molestie a, ultricies porta urna. "
            + "Vestibulum commodo volutpat a, convallis ac, laoreet enim. "
            + "Phasellus fermentum in, dolor. Pellentesque facilisis. "
            + "Nulla imperdiet sit amet magna. Vestibulum dapibus, mauris "
            + "nec malesuada fames ac turpis velit, rhoncus eu, luctus et "
            + "interdum adipiscing wisi. Aliquam erat ac ipsum. Integer "
            + "aliquam purus. Quisque lorem tortor fringilla sed, vestibulum id, "
            + "eleifend justo vel bibendum sapien massa ac turpis faucibus orci "
            + "luctus non, consectetuer lobortis quis, varius in, purus. "
            + "Integer ultrices posuere cubilia Curae, Nulla ipsum dolor lacus, "
            + "suscipit adipiscing. Cum sociis natoque penatibus et ultrices volutpat. "
            + "Nullam wisi ultricies a, gravida vitae, dapibus risus ante sodales "
            + "lectus blandit eu, tempor diam pede cursus vitae, ultricies eu, "
            + "faucibus quis, porttitor eros cursus lectus, pellentesque eget, "
            + "bibendum a, gravida ullamcorper quam. Nullam viverra consectetuer. "
            + "Quisque cursus et, porttitor risus. Aliquam sem. In hendrerit nulla "
            + "quam nunc, accumsan congue. Lorem ipsum primis in nibh vel risus. "
            + "Sed vel lectus. Ut sagittis, ipsum dolor quam.";
}
