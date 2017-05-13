package com.utils;

public class TCurrency extends Number {

    public static void formatU(double ll) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    private final double value;

    public static TCurrency parse(Double value, TCurrency def) {
        return value != null ? new TCurrency(value) : def;
    }

    public static TCurrency parse(Double value, double def) {
        return value != null ? new TCurrency(value) : new TCurrency(def);
    }

    public static TCurrency parse(String value, double def) {
        return parse(value, new TCurrency(def));
    }

    public static TCurrency parse(String value, TCurrency def) {
        if (value == null)
            return null;
        return TCurrency.parse(Utils.strDouble(
                value.replace(",", ".").replace(" ", "").trim(),
                null), def);
    }

    public static TCurrency parse(String value) {
        TCurrency curr = TCurrency.parse(value, null);
        if (curr == null)
            throw new NumberFormatException(value);
        return curr;
    }

    public TCurrency(double value) {
        this.value = Math.round(value * 100d) / 100d;
    }

    public TCurrency(Double value, double def) {
        this.value = value != null ? Math.round(value * 100d) / 100d : def;
    }

    @Override
    public int intValue() {
        return (int) value;
    }

    @Override
    public long longValue() {
        return (long) value;
    }

    @Override
    public float floatValue() {
        return (float) value;
    }

    @Override
    public double doubleValue() {
        return value;
    }

    @Override
    public String toString() {
        // Musi być zapisane w formacie z kropką !!!
        return format(false, ".");
    }

    public String format() {
        return format(true, ",");
    }

    public String format(boolean addSpaces, String separator) {
        return format(this, addSpaces, separator);
    }

    public static String format(TCurrency curr) {
        return format(curr, true, ",");
    }

    /**
     * Formatuwanie wraz z walutą (zł)
     *
     * @param curr
     * @return
     */
    public static String formatU(TCurrency curr) {
        String frmt = format(curr, true, ",");
        if (frmt != null)
            frmt += " zł";
        return frmt;
    }

    public static String formatU(String curr) {
        String frmt = format(curr) + " zł";
        if (frmt != null)
            frmt += " zł";
        return frmt;
    }

    public static String format(String curr) {
        if (curr == null)
            return null;
        return format(parse(curr), true, ",");
    }

    public static String format(double curr) {
        return format(new TCurrency(curr), true, ",");
    }

    public static String format(TCurrency curr, boolean addSpaces, String separator) {
        if (curr == null)
            return null;

        boolean isNegative = curr.doubleValue() < 0;

        String s = Double.toString(Math.abs(curr.doubleValue()));
        s = s.replace(",", ".");
        if (!s.contains("."))
            return s + ".00";
        String t = s.substring(s.indexOf(".") + 1);
        while (t.length() < 2)
            t += "0";
        s = s.substring(0, s.indexOf("."));

        if (addSpaces) {
            String ss = "";

            for (int i = 0; i < s.length(); i++) {
                ss = s.charAt(s.length() - i - 1) + ss;
                if (((i + 1) % 3) == 0)
                    ss = " " + ss;
            }
            s = ss;
        }
        return (isNegative ? "-" : "") + (s + "." + t).replace(".", separator).trim();
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 59 * hash + (int) (Double.doubleToLongBits(this.value)
                ^ (Double.doubleToLongBits(this.value) >>> 32));
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        final TCurrency other = (TCurrency) obj;
        if (Double.doubleToLongBits(this.value) != Double.doubleToLongBits(other.value))
            return false;
        return true;
    }

}
