package com.utils;

/**
 * Miłosz Ziernik 2014/02/08
 */
public enum Char implements CharSequence {

    close(-1, '✘'),
    remove(-1, '✖'),
    apply(-1, '✔'),
    dot(-1, '•'),
    star(-1, '★'),
    tripleDot(-1, '…'),
    returnKey(-1, '↵'),
    tabKey(-1, '⇆'),
    rightArrow(-1, '→'),
    rightArrowDouble(-1, '⇒'),
    play(-1, '►'),
    esc(0x1b, null),
    nbsp(160, null), // twarda spacja
    //───-------------------------- Box Drawings -----------------------------
    BOX_DRAWINGS_LIGHT_HORIZONTAL(9472, '─'),
    BOX_DRAWINGS_HEAVY_HORIZONTAL(9473, '━'),
    BOX_DRAWINGS_LIGHT_VERTICAL(9474, '│'),
    BOX_DRAWINGS_HEAVY_VERTICAL(9475, '┃'),
    BOX_DRAWINGS_LIGHT_TRIPLE_DASH_HORIZONTAL(9476, '┄'),
    BOX_DRAWINGS_HEAVY_TRIPLE_DASH_HORIZONTAL(9477, '┅'),
    BOX_DRAWINGS_LIGHT_TRIPLE_DASH_VERTICAL(9478, '┆'),
    BOX_DRAWINGS_HEAVY_TRIPLE_DASH_VERTICAL(9479, '┇'),
    BOX_DRAWINGS_LIGHT_QUADRUPLE_DASH_HORIZONTAL(9480, '┈'),
    BOX_DRAWINGS_HEAVY_QUADRUPLE_DASH_HORIZONTAL(9481, '┉'),
    BOX_DRAWINGS_LIGHT_QUADRUPLE_DASH_VERTICAL(9482, '┊'),
    BOX_DRAWINGS_HEAVY_QUADRUPLE_DASH_VERTICAL(9483, '┋'),
    BOX_DRAWINGS_LIGHT_DOWN_AND_RIGHT(9484, '┌'),
    BOX_DRAWINGS_DOWN_LIGHT_AND_RIGHT_HEAVY(9485, '┍'),
    BOX_DRAWINGS_DOWN_HEAVY_AND_RIGHT_LIGHT(9486, '┎'),
    BOX_DRAWINGS_HEAVY_DOWN_AND_RIGHT(9487, '┏'),
    BOX_DRAWINGS_LIGHT_DOWN_AND_LEFT(9488, '┐'),
    BOX_DRAWINGS_DOWN_LIGHT_AND_LEFT_HEAVY(9489, '┑'),
    BOX_DRAWINGS_DOWN_HEAVY_AND_LEFT_LIGHT(9490, '┒'),
    BOX_DRAWINGS_HEAVY_DOWN_AND_LEFT(9491, '┓'),
    BOX_DRAWINGS_LIGHT_UP_AND_RIGHT(9492, '└'),
    BOX_DRAWINGS_UP_LIGHT_AND_RIGHT_HEAVY(9493, '┕'),
    BOX_DRAWINGS_UP_HEAVY_AND_RIGHT_LIGHT(9494, '┖'),
    BOX_DRAWINGS_HEAVY_UP_AND_RIGHT(9495, '┗'),
    BOX_DRAWINGS_LIGHT_UP_AND_LEFT(9496, '┘'),
    BOX_DRAWINGS_UP_LIGHT_AND_LEFT_HEAVY(9497, '┙'),
    BOX_DRAWINGS_UP_HEAVY_AND_LEFT_LIGHT(9498, '┚'),
    BOX_DRAWINGS_HEAVY_UP_AND_LEFT(9499, '┛'),
    BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT(9500, '├'),
    BOX_DRAWINGS_VERTICAL_LIGHT_AND_RIGHT_HEAVY(9501, '┝'),
    BOX_DRAWINGS_UP_HEAVY_AND_RIGHT_DOWN_LIGHT(9502, '┞'),
    BOX_DRAWINGS_DOWN_HEAVY_AND_RIGHT_UP_LIGHT(9503, '┟'),
    BOX_DRAWINGS_VERTICAL_HEAVY_AND_RIGHT_LIGHT(9504, '┠'),
    BOX_DRAWINGS_DOWN_LIGHT_AND_RIGHT_UP_HEAVY(9505, '┡'),
    BOX_DRAWINGS_UP_LIGHT_AND_RIGHT_DOWN_HEAVY(9506, '┢'),
    BOX_DRAWINGS_HEAVY_VERTICAL_AND_RIGHT(9507, '┣'),
    BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT(9508, '┤'),
    BOX_DRAWINGS_VERTICAL_LIGHT_AND_LEFT_HEAVY(9509, '┥'),
    BOX_DRAWINGS_UP_HEAVY_AND_LEFT_DOWN_LIGHT(9510, '┦'),
    BOX_DRAWINGS_DOWN_HEAVY_AND_LEFT_UP_LIGHT(9511, '┧'),
    BOX_DRAWINGS_VERTICAL_HEAVY_AND_LEFT_LIGHT(9512, '┨'),
    BOX_DRAWINGS_DOWN_LIGHT_AND_LEFT_UP_HEAVY(9513, '┩'),
    BOX_DRAWINGS_UP_LIGHT_AND_LEFT_DOWN_HEAVY(9514, '┪'),
    BOX_DRAWINGS_HEAVY_VERTICAL_AND_LEFT(9515, '┫'),
    BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL(9516, '┬'),
    BOX_DRAWINGS_LEFT_HEAVY_AND_RIGHT_DOWN_LIGHT(9517, '┭'),
    BOX_DRAWINGS_RIGHT_HEAVY_AND_LEFT_DOWN_LIGHT(9518, '┮'),
    BOX_DRAWINGS_DOWN_LIGHT_AND_HORIZONTAL_HEAVY(9519, '┯'),
    BOX_DRAWINGS_DOWN_HEAVY_AND_HORIZONTAL_LIGHT(9520, '┰'),
    BOX_DRAWINGS_RIGHT_LIGHT_AND_LEFT_DOWN_HEAVY(9521, '┱'),
    BOX_DRAWINGS_LEFT_LIGHT_AND_RIGHT_DOWN_HEAVY(9522, '┲'),
    BOX_DRAWINGS_HEAVY_DOWN_AND_HORIZONTAL(9523, '┳'),
    BOX_DRAWINGS_LIGHT_UP_AND_HORIZONTAL(9524, '┴'),
    BOX_DRAWINGS_LEFT_HEAVY_AND_RIGHT_UP_LIGHT(9525, '┵'),
    BOX_DRAWINGS_RIGHT_HEAVY_AND_LEFT_UP_LIGHT(9526, '┶'),
    BOX_DRAWINGS_UP_LIGHT_AND_HORIZONTAL_HEAVY(9527, '┷'),
    BOX_DRAWINGS_UP_HEAVY_AND_HORIZONTAL_LIGHT(9528, '┸'),
    BOX_DRAWINGS_RIGHT_LIGHT_AND_LEFT_UP_HEAVY(9529, '┹'),
    BOX_DRAWINGS_LEFT_LIGHT_AND_RIGHT_UP_HEAVY(9530, '┺'),
    BOX_DRAWINGS_HEAVY_UP_AND_HORIZONTAL(9531, '┻'),
    BOX_DRAWINGS_LIGHT_VERTICAL_AND_HORIZONTAL(9532, '┼'),
    BOX_DRAWINGS_LEFT_HEAVY_AND_RIGHT_VERTICAL_LIGHT(9533, '┽'),
    BOX_DRAWINGS_RIGHT_HEAVY_AND_LEFT_VERTICAL_LIGHT(9534, '┾'),
    BOX_DRAWINGS_VERTICAL_LIGHT_AND_HORIZONTAL_HEAVY(9535, '┿'),
    BOX_DRAWINGS_UP_HEAVY_AND_DOWN_HORIZONTAL_LIGHT(9536, '╀'),
    BOX_DRAWINGS_DOWN_HEAVY_AND_UP_HORIZONTAL_LIGHT(9537, '╁'),
    BOX_DRAWINGS_VERTICAL_HEAVY_AND_HORIZONTAL_LIGHT(9538, '╂'),
    BOX_DRAWINGS_LEFT_UP_HEAVY_AND_RIGHT_DOWN_LIGHT(9539, '╃'),
    BOX_DRAWINGS_RIGHT_UP_HEAVY_AND_LEFT_DOWN_LIGHT(9540, '╄'),
    BOX_DRAWINGS_LEFT_DOWN_HEAVY_AND_RIGHT_UP_LIGHT(9541, '╅'),
    BOX_DRAWINGS_RIGHT_DOWN_HEAVY_AND_LEFT_UP_LIGHT(9542, '╆'),
    BOX_DRAWINGS_DOWN_LIGHT_AND_UP_HORIZONTAL_HEAVY(9543, '╇'),
    BOX_DRAWINGS_UP_LIGHT_AND_DOWN_HORIZONTAL_HEAVY(9544, '╈'),
    BOX_DRAWINGS_RIGHT_LIGHT_AND_LEFT_VERTICAL_HEAVY(9545, '╉'),
    BOX_DRAWINGS_LEFT_LIGHT_AND_RIGHT_VERTICAL_HEAVY(9546, '╊'),
    BOX_DRAWINGS_HEAVY_VERTICAL_AND_HORIZONTAL(9547, '╋'),
    BOX_DRAWINGS_LIGHT_DOUBLE_DASH_HORIZONTAL(9548, '╌'),
    BOX_DRAWINGS_HEAVY_DOUBLE_DASH_HORIZONTAL(9549, '╍'),
    BOX_DRAWINGS_LIGHT_DOUBLE_DASH_VERTICAL(9550, '╎'),
    BOX_DRAWINGS_HEAVY_DOUBLE_DASH_VERTICAL(9551, '╏'),
    BOX_DRAWINGS_DOUBLE_HORIZONTAL(9552, '═'),
    BOX_DRAWINGS_DOUBLE_VERTICAL(9553, '║'),
    BOX_DRAWINGS_DOWN_SINGLE_AND_RIGHT_DOUBLE(9554, '╒'),
    BOX_DRAWINGS_DOWN_DOUBLE_AND_RIGHT_SINGLE(9555, '╓'),
    BOX_DRAWINGS_DOUBLE_DOWN_AND_RIGHT(9556, '╔'),
    BOX_DRAWINGS_DOWN_SINGLE_AND_LEFT_DOUBLE(9557, '╕'),
    BOX_DRAWINGS_DOWN_DOUBLE_AND_LEFT_SINGLE(9558, '╖'),
    BOX_DRAWINGS_DOUBLE_DOWN_AND_LEFT(9559, '╗'),
    BOX_DRAWINGS_UP_SINGLE_AND_RIGHT_DOUBLE(9560, '╘'),
    BOX_DRAWINGS_UP_DOUBLE_AND_RIGHT_SINGLE(9561, '╙'),
    BOX_DRAWINGS_DOUBLE_UP_AND_RIGHT(9562, '╚'),
    BOX_DRAWINGS_UP_SINGLE_AND_LEFT_DOUBLE(9563, '╛'),
    BOX_DRAWINGS_UP_DOUBLE_AND_LEFT_SINGLE(9564, '╜'),
    BOX_DRAWINGS_DOUBLE_UP_AND_LEFT(9565, '╝'),
    BOX_DRAWINGS_VERTICAL_SINGLE_AND_RIGHT_DOUBLE(9566, '╞'),
    BOX_DRAWINGS_VERTICAL_DOUBLE_AND_RIGHT_SINGLE(9567, '╟'),
    BOX_DRAWINGS_DOUBLE_VERTICAL_AND_RIGHT(9568, '╠'),
    BOX_DRAWINGS_VERTICAL_SINGLE_AND_LEFT_DOUBLE(9569, '╡'),
    BOX_DRAWINGS_VERTICAL_DOUBLE_AND_LEFT_SINGLE(9570, '╢'),
    BOX_DRAWINGS_DOUBLE_VERTICAL_AND_LEFT(9571, '╣'),
    BOX_DRAWINGS_DOWN_SINGLE_AND_HORIZONTAL_DOUBLE(9572, '╤'),
    BOX_DRAWINGS_DOWN_DOUBLE_AND_HORIZONTAL_SINGLE(9573, '╥'),
    BOX_DRAWINGS_DOUBLE_DOWN_AND_HORIZONTAL(9574, '╦'),
    BOX_DRAWINGS_UP_SINGLE_AND_HORIZONTAL_DOUBLE(9575, '╧'),
    BOX_DRAWINGS_UP_DOUBLE_AND_HORIZONTAL_SINGLE(9576, '╨'),
    BOX_DRAWINGS_DOUBLE_UP_AND_HORIZONTAL(9577, '╩'),
    BOX_DRAWINGS_VERTICAL_SINGLE_AND_HORIZONTAL_DOUBLE(9578, '╪'),
    BOX_DRAWINGS_VERTICAL_DOUBLE_AND_HORIZONTAL_SINGLE(9579, '╫'),
    BOX_DRAWINGS_DOUBLE_VERTICAL_AND_HORIZONTAL(9580, '╬'),
    BOX_DRAWINGS_LIGHT_ARC_DOWN_AND_RIGHT(9581, '╭'),
    BOX_DRAWINGS_LIGHT_ARC_DOWN_AND_LEFT(9582, '╮'),
    BOX_DRAWINGS_LIGHT_ARC_UP_AND_LEFT(9583, '╯'),
    BOX_DRAWINGS_LIGHT_ARC_UP_AND_RIGHT(9584, '╰'),
    BOX_DRAWINGS_LIGHT_DIAGONAL_UPPER_RIGHT_TO_LOWER_LEFT(9585, '╱'),
    BOX_DRAWINGS_LIGHT_DIAGONAL_UPPER_LEFT_TO_LOWER_RIGHT(9586, '╲'),
    BOX_DRAWINGS_LIGHT_DIAGONAL_CROSS(9587, '╳'),
    BOX_DRAWINGS_LIGHT_LEFT(9588, '╴'),
    BOX_DRAWINGS_LIGHT_UP(9589, '╵'),
    BOX_DRAWINGS_LIGHT_RIGHT(9590, '╶'),
    BOX_DRAWINGS_LIGHT_DOWN(9591, '╷'),
    BOX_DRAWINGS_HEAVY_LEFT(9592, '╸'),
    BOX_DRAWINGS_HEAVY_UP(9593, '╹'),
    BOX_DRAWINGS_HEAVY_RIGHT(9594, '╺'),
    BOX_DRAWINGS_HEAVY_DOWN(9595, '╻'),
    BOX_DRAWINGS_LIGHT_LEFT_AND_HEAVY_RIGHT(9596, '╼'),
    BOX_DRAWINGS_LIGHT_UP_AND_HEAVY_DOWN(9597, '╽'),
    BOX_DRAWINGS_HEAVY_LEFT_AND_LIGHT_RIGHT(9598, '╾'),
    BOX_DRAWINGS_HEAVY_UP_AND_LIGHT_DOWN(9599, '╿');
    //------------------------------- /Box Drawings ----------------------------//------------------------------- /Box Drawings ----------------------------

    public final char character;

    public static boolean isWhiteChar(char character) {
        for (char c : WHITE)
            if (c == character)
                return true;
        return false;
    }

    public final static char[] WHITE = {0x0009, 0x000A, 0x000B, 0x000C, 0x000D,
        0x0020, 0x0085, 0x00A0, 0x1680, 0x2000, 0x2001, 0x2002, 0x2003, 0x2004,
        0x2005, 0x2006, 0x2007, 0x2008, 0x2009, 0x200A, 0x2028, 0x2029, 0x202F,
        0x205F, 0x3000, 0x180E, 0x200B, 0x200C, 0x200D, 0x2060, 0xFEFF};

    @Override
    public String toString() {
        return Character.toString(character);
    }

    Char(int number, Character example) {
        character = number >= 0 ? (char) number : example;
    }

    @Override
    public int length() {
        return 1;
    }

    @Override
    public char charAt(int index) {
        return character;
    }

    @Override
    public CharSequence subSequence(int start, int end) {
        return Character.toString(character);
    }

}
