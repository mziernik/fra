package com.utils.console;

import com.exceptions.ThrowableException;
import com.utils.Char;
import java.io.*;

public class VT100 implements Appendable {

    private final Appendable writer;

    public VT100(Appendable writer) {
        this.writer = writer;
    }

    @Override
    public String toString() {
        return writer.toString();
    }

    @Override
    public Appendable append(CharSequence csq) {
        try {
            return writer.append(csq);
        } catch (IOException ex) {
            throw new ThrowableException(ex);
        }
    }

    @Override
    public Appendable append(char c) {
        try {
            return writer.append(c);
        } catch (IOException ex) {
            throw new ThrowableException(ex);
        }
    }

    @Override
    public Appendable append(CharSequence csq, int start, int end) {
        try {
            return writer.append(csq, start, end);
        } catch (IOException ex) {
            throw new ThrowableException(ex);
        }
    }

    public VT100 command(String command) {
        append(Char.esc);
        append(command);
        return this;
    }

    public VT100 attr(int value) {
        return command("[" + value + "m");
    }

    public VT100 foreground(CColor color) {
        return attr(color.getForeground());
    }

    public VT100 background(CColor color) {
        return attr(color.getForeground());
    }

    // czyszczenie atrybutów
    public VT100 attrReset() {
        return attr(0);
    }

    public VT100 bright() {
        return attr(1);
    }

    public VT100 dim() {
        return attr(2);
    }

    public VT100 underline() {
        return attr(4);
    }

    public VT100 blink() {
        return attr(5);
    }

    public VT100 reverse() {
        return attr(7);
    }

    public VT100 hidden() {
        return attr(8);
    }

    public VT100 hideCursor() {
        return command("[?25l");
    }

    public VT100 showCursor() {
        return command("[?25h");
    }

    public VT100 cursorPos(int x, int y) {
        return command("[" + x + ";" + y + "H");
        //command(x + ";" + y + "H");
    }

    public VT100 cursorHome(int x, int y) {
        return command("[H");
    }

    public VT100 clearLine() {
        return command("[2K");
    }

    public VT100 clearLineLeft() {
        return command("[1K");
    }

    public VT100 clearLineRight() {
        return command("[K");
    }

    public VT100 clearScreenUp() {
        return command("[1J");
    }

    public VT100 clearScreenDown() {
        return command("[J");
    }

    public VT100 resetDevice() {
        return command("c");
    }

    public VT100 lineWrap(boolean enabled) {
        return command("[7" + (enabled ? "h" : "l"));
    }

    // Wyczyść ekran, ustaw pozycję kurosra na 0, 0
    public VT100 clearScreen() {
        return command("[2J");
    }

    public static enum CColor {

        black(0),
        red(1),
        green(2),
        yellow(3),
        blue(4),
        magenta(5),
        cyan(6),
        white(7);
        private final int id;

        public int getForeground() {
            return 30 + id;
        }

        public int getBackground() {
            return 40 + id;
        }

        private CColor(int id) {
            this.id = id;
        }
    }

}
