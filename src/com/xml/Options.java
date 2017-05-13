package com.xml;

import java.util.LinkedList;
import java.util.List;
import org.xml.sax.EntityResolver;

public class Options {

    public final static String DEFAULT_HEADER = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";

    public String spaceChar = "  "; //\t";
    public String returnChar = "\n"; //"\r\n";

    public List<String> ignoredDTD = new LinkedList<>();
    public boolean useShortTags = true;
    public boolean singleLine = false; // zamien entery na spacje (dla html-a)
    public boolean escapeWhiteChars = false; // escapowanie znakow o numerze ascii < 32
    public boolean validateDTD = false;
    public boolean trimInnerText = false;

    public String header = DEFAULT_HEADER;
    public String customDTDvalidator = null; // plik DTD
    public EntityResolver entityResolver = null; // wymagany jesli aktwna jest flaga customDTDvalidator
    public boolean useNameSpaces = true; //wlacza obsluge przestrzeni nazw podczas wyszukiwania elementow 
    public boolean caseSensitive = true;
}
