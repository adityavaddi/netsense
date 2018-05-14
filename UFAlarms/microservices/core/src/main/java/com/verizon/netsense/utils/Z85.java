package com.verizon.netsense.utils;

/**
 * Created by brefsdal on 4/12/17.
 */


public class Z85 {

    // A Java Implementation of ZMQ Z85
    // Translated from C
    // https://rfc.zeromq.org/spec:32/Z85/

    //  Maps base 256 to base 85
    private static char[] encoder  =
            ("0123456789" +
            "abcdefghij" +
            "klmnopqrst" +
            "uvwxyzABCD" +
            "EFGHIJKLMN" +
            "OPQRSTUVWX" +
            "YZ.-:+=^!/" +
            "*?&<>()[]{" +
            "}@%$#").toCharArray();

    //  Maps base 85 to base 256
    //  We chop off lower 32 and higher 128 ranges
    private static byte[] decoder = new byte[] {
        0x00, 0x44, 0x00, 0x54, 0x53, 0x52, 0x48, 0x00,
        0x4B, 0x4C, 0x46, 0x41, 0x00, 0x3F, 0x3E, 0x45,
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
        0x08, 0x09, 0x40, 0x00, 0x49, 0x42, 0x4A, 0x47,
        0x51, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2A,
        0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31, 0x32,
        0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A,
        0x3B, 0x3C, 0x3D, 0x4D, 0x00, 0x4E, 0x43, 0x00,
        0x00, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10,
        0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
        0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20,
        0x21, 0x22, 0x23, 0x4F, 0x00, 0x50, 0x00, 0x00};

    public static String z85Encode (byte[] data, int size) {

        //  Accepts only byte arrays bounded to 4 bytes
        if (size % 4 > 0)
            return null;

        int encoded_size = size * 5 / 4;
        char[] encoded = new char[encoded_size];

        int char_nbr = 0;
        int byte_nbr = 0;
        long value = 0;
        while (byte_nbr < size) {
            //  Accumulate value in base 256 (binary)
            value = value * 256 + (data [byte_nbr++] & 0xff);
            //value = value * 256 + ( (int) data[byte_nbr++] & 0xff );
            if (byte_nbr % 4 == 0) {
                //  Output value in base 85
                long divisor = 85 * 85 * 85 * 85;
                while (divisor > 0) {
                    encoded [char_nbr++] = encoder[(int) (value / divisor % 85)];
                    divisor /= 85;
                }
                value = 0;
            }
        }
        assert (char_nbr == encoded_size);
        return new String(encoded);
    }


    //  --------------------------------------------------------------------------
    //  Decode an encoded string into a byte array; size of array will be
    //  strlen (string) * 4 / 5.

    public static byte[] z85Decode (String string) {
        //  Accepts only strings bounded to 5 bytes
        if (string == null || string.length() % 5 > 0)
            return null;

        int length = string.length();

        int decoded_size = length * 4 / 5;
        byte[] decoded = new byte[decoded_size];

        int byte_nbr = 0;
        int char_nbr = 0;
        long value = 0;

        byte[] stringBytes = string.getBytes();
        while (char_nbr < length) {
            //  Accumulate value in base 85
            value = value * 85 + decoder [(stringBytes [char_nbr++] & 0xff) - 32];
            if (char_nbr % 5 == 0) {
                //  Output value in base 256
                long divisor = 256 * 256 * 256;
                while (divisor > 0) {
                    decoded [byte_nbr++] = (byte) ((value / divisor % 256) & 0xff);
                    divisor /= 256;
                }
                value = 0;
            }
        }
        assert (byte_nbr == decoded_size);
        return decoded;
    }

    public static void main (String args[]) {
        
        byte[] test_data_1 = new byte[] {
            (byte)0x86, (byte)0x4F, (byte)0xD2, (byte)0x6F, (byte)0xB5, (byte)0x59, (byte)0xF7, (byte)0x5B};

        byte[] test_data_2 = new byte[] {
            (byte)0x8E, (byte)0x0B, (byte)0xDD, (byte)0x69, (byte)0x76, (byte)0x28, (byte)0xB9, (byte)0x1D,
            (byte)0x8F, (byte)0x24, (byte)0x55, (byte)0x87, (byte)0xEE, (byte)0x95, (byte)0xC5, (byte)0xB0,
            (byte)0x4D, (byte)0x48, (byte)0x96, (byte)0x3F, (byte)0x79, (byte)0x25, (byte)0x98, (byte)0x77,
            (byte)0xB4, (byte)0x9C, (byte)0xD9, (byte)0x06, (byte)0x3A, (byte)0xEA, (byte)0xD3, (byte)0xB7};

        String encoded;
        byte[] decoded;

        encoded = z85Encode (null, 0);
        decoded = z85Decode (encoded);
        assert (encoded.equals(""));

        encoded = z85Encode (test_data_1, 3);
        assert (encoded == null);

        encoded = z85Encode (test_data_1, 8);
        assert (encoded.length() == 10);
        assert (encoded.equals("HelloWorld"));
        decoded = z85Decode (encoded);
        for (int ii = 0; ii < 8; ii++) {
            assert (test_data_1[ii] == decoded[ii]);
        }

        encoded = z85Encode (test_data_2, 32);
        assert (encoded.length() == 40);
        assert (encoded.equals("JTKVSB%%)wK0E.X)V>+}o?pNmC{O&4W4b!Ni{Lh6"));
        decoded = z85Decode (encoded);
        for (int ii = 0; ii < 32; ii++) {
            assert(test_data_2[ii] == decoded[ii]);
        }
    }
}
