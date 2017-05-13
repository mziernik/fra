package com.utils.hashes;

import com.utils.text.StrWriter;
import java.nio.ByteBuffer;

public class Bin {

    public static String toString(byte[] value) {
        return toString(value, false);
    }

    public static String toString(byte[] value, boolean div4) {

        StrWriter sb = new StrWriter();
        for (byte b : value) {
            String val = Integer.toBinaryString(b);
            while ((sb.length() + val.length()) % 8 != 0)
                sb.append("0");
            sb.append(val);
        }

        char[] arr = sb.toString().toCharArray();
        sb = new StrWriter();

        for (int i = 0; i < arr.length; i++) {
            if (div4 && i > 0 && i % 4 == 0)
                sb.append(" ");

            if (i % 8 == 0)
                sb.append(" ");

            sb.append(arr[i]);
        }

        return sb.toString().trim();
    }

    public static String toString(long value) {
        return toString(ByteBuffer.allocate(8).putLong(value).array());
    }

}
