package bob;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public final class Tools {
    static final int[] ESCAPED_CHARS={ '|', '&', ';', '<', '>',
                                       '(', ')', '$', '`', '\'',
                                       '"', '*', '?', '[', ']',
                                       '#', '~', '=', '%', ' ',
                                       '\n', '\t' };

    // Could be converted ,e.g., to a lookup in a hash-set, but would it be
    // faster than scan through handful of primitives?

    private static boolean isEscaped(int cp) {
        boolean res=false;
        for(int v : ESCAPED_CHARS) {
            if (v==cp) {
                res=true;
                break;
            }
        }
        return res;
    }

    public static String escapePathString (String s) {
        boolean lastCharWasEscapingBackslash=false;
        StringBuilder sb = new StringBuilder();

        for(int i = 0; i < s.length(); ) {
            int cp = s.codePointAt(i);
            if (cp == '\\') {
                if (lastCharWasEscapingBackslash) {
                    sb.append('\\');
                    sb.append('\\');
                    lastCharWasEscapingBackslash=false;
                }
                else lastCharWasEscapingBackslash=true;
            }
            else if (isEscaped(cp)) {
                sb.append('\\');
                sb.appendCodePoint(cp);
                lastCharWasEscapingBackslash=false;
            }
            else {
                if (lastCharWasEscapingBackslash) {
                    sb.append('\\');
                    sb.append('\\');
                }
                sb.appendCodePoint(cp);
                lastCharWasEscapingBackslash=false;
            }

            i += Character.charCount(cp);
        }

        if (lastCharWasEscapingBackslash) {
            sb.append('\\');
            sb.append('\\');
        }
        return sb.toString();
    }
}
