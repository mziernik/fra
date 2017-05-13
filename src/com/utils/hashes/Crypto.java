package com.utils.hashes;

import com.utils.Utils;
import com.utils.Is;
import com.context.AppConfig;
import com.exceptions.ThrowableException;
import com.utils.hashes.Hashes.Hash;
import com.mlogger.Log;
import java.io.ByteArrayOutputStream;
import java.nio.ByteBuffer;
import java.util.Arrays;
import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

/**
 *
 * @author Miłosz Ziernik
 */
public class Crypto {

    /**
     * Metoda szyfruje strumień danych oraz dodaje sumę kontrolną CRC.
     * Szyfrowanie AES/CBC, klucz generowany jest na podstawie
     * AppConfig.applicationSecureKey, wektor IV na bazie AppConfig.serviceName
     *
     * @param data
     * @return
     */
    public static byte[] encrypt(byte[] data) {
        try {
            byte[] pass = Hashes.hashB(Hash.MD5,
                    Base64.encode(AppConfig.getApplicationSecureKey()
                            .getBytes(Utils.UTF8)));

            byte[] iv = Hashes.hashB(Hash.MD5,
                    Base64.encode(AppConfig.getServiceName()
                            .getBytes(Utils.UTF8)));

            Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
            ByteArrayOutputStream bout = new ByteArrayOutputStream();
            bout.write(ByteBuffer.allocate(4).putInt(Hashes.crc32(data)).array());
            bout.write(data);

            cipher.init(Cipher.ENCRYPT_MODE,
                    new SecretKeySpec(pass, "AES"),
                    new IvParameterSpec(iv));

            return cipher.doFinal(bout.toByteArray());
        } catch (Exception ex) {
            throw new ThrowableException(ex);
        }
    }

    public static byte[] encrypt(String string) {
        return string == null ? null : encrypt(string.getBytes(Utils.UTF8));
    }

    /**
     * Metoda seszyfruje strumień danych zaszyfrowany uprzednio metodą encrypt.
     * Po odszyfrowaniu weryfikowana jest poprawność sumy kontolnej. Jeśli CRC
     * jest nieprawidłowe, zwrócony zostanie null
     *
     * @param data
     * @return
     */
    public static byte[] decrypt(byte[] data) {
        try {

            byte[] pass = Hashes.hashB(Hash.MD5,
                    Base64.encode(AppConfig.getApplicationSecureKey()
                            .getBytes(Utils.UTF8)));

            byte[] iv = Hashes.hashB(Hash.MD5,
                    Base64.encode(AppConfig.getServiceName()
                            .getBytes(Utils.UTF8)));

            Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
            SecretKeySpec secretKey = new SecretKeySpec(pass, "AES");
            cipher.init(Cipher.DECRYPT_MODE, secretKey, new IvParameterSpec(iv));

            data = cipher.doFinal(data);
            int crc = ByteBuffer.wrap(data, 0, 4).getInt();
            data = Arrays.copyOfRange(data, 4, data.length);

            if (Hashes.crc32(data) != crc) {
                Log.warning("Crypto", "Nieprawidłowa suma kontrolna");
                return null;
            }

            return data;
        } catch (Exception ex) {
            throw new ThrowableException(ex);
        }
    }

}
