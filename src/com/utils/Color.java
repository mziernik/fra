package com.utils;

import com.utils.hashes.Hex;
import java.util.*;

public enum Color {

    black(0x000000),
    red(0xFF0000),
    lime(0x00FF00),
    blue(0x0000FF),
    white(0xFFFFFF),
    silver(0xC0C0C0),
    gray(0x808080),
    olive(0x808000),
    yellow(0xFFFF00),
    maroon(0x800000),
    navy(0x000080),
    green(0x008000),
    purple(0x800080),
    teal(0x008080),
    fuchsia(0xFF00FF),
    aqua(0x00FFFF);

    int value;

    private Color(int value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return "#" + Hex.toString(value, 3);
    }

    public static Color random(Collection<Color> excluded, Color... colors) {

        if (colors == null || colors.length == 0)
            colors = Color.values();

        if (excluded != null
                && colors.length > 0
                && excluded.containsAll(Arrays.asList(colors)))
            return null;

        while (true) {
            Color c = colors[new Random().nextInt(colors.length)];

            if (excluded != null && excluded.contains(c))
                continue;

            return c;
        }

    }
}
