package com.script.evaluator;

public class Pos {

    public String file;
    public int line;
    public int column;

    public Pos(String file, int line, int column) {
        this.file = file;
        this.line = line;
        this.column = column;
    }

    @Override
    public String toString() {
        return (file != null ? file + " " : "")
                + "[" + line + ", " + column + "]";
    }

}
