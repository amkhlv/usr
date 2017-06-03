package AMail;

/**
 * Created by andrei on 5/22/17.
 */

import org.apache.james.mime4j.*;
import org.apache.james.mime4j.parser.AbstractContentHandler;
import org.apache.james.mime4j.stream.BodyDescriptor;
import org.apache.james.mime4j.stream.Field;
import javax.mail.internet.MimeUtility;
import java.io.UnsupportedEncodingException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;

public class EMailContentHandler extends AbstractContentHandler {
    String[] knownHeaders;
    public EMailContentHandler(String[] knownHeaders) {
        this.knownHeaders = knownHeaders;
    }
    public HashMap<String, String> collectedHeaders = new HashMap();

    public void body(BodyDescriptor bd, InputStream is)
            throws MimeException, IOException {
        //System.out.println("header data = " + bd);
    }

    public void field(Field rawField) throws MimeException {
        String nm = rawField.getName();
        for (int i = 0; i < knownHeaders.length; i++) {
            String hdr = knownHeaders[i];
            String bdy = rawField.getBody();
            if (nm.equalsIgnoreCase(hdr)) {
                if (hdr.equalsIgnoreCase("content-type")) {
                    if (collectedHeaders.containsKey(hdr)) {
                        String x = collectedHeaders.get(hdr);
                        collectedHeaders.put(hdr, x + " ¶ " + bdy);
                    } else {
                        collectedHeaders.put(hdr, bdy);
                    }
                } else {
                    collectedHeaders.put(hdr, bdy);
                }
            }
        }
    }

    public void startMultipart(BodyDescriptor bd) throws MimeException {
        //System.out.println("Multipart message detexted, header data = " + bd);
    }
}
