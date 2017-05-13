package com.servlet;

import com.utils.Utils;
import com.utils.Is;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class UserAgent {

    public final OperatingSystem operatingSystem;
    public final Browser browser;
    public final int id;
    private final String userAgentString;

    public UserAgent(String userAgentString) {
        if (userAgentString == null)
            userAgentString = "";

        browser = Browser.parseUserAgentString(userAgentString);

        if (browser != Browser.BOT)
            operatingSystem = OperatingSystem.parseUserAgentString(userAgentString);
        else
            operatingSystem = OperatingSystem.UNKNOWN;

        this.id = ((operatingSystem.getId() << 16) + browser.getId());
        this.userAgentString = userAgentString;
    }

    public static UserAgent parseUserAgentString(String userAgentString) {
        return new UserAgent(userAgentString);
    }

    public Version getBrowserVersion() {
        return this.browser.getVersion(this.userAgentString);
    }

    @Override
    public String toString() {
        return userAgentString;
    }

    public String getShortUA() {
        String ss = "";

        if (browser != Browser.UNKNOWN)
            ss = browser.getName();

        // przytnij wersje w nazwie
        if (ss.indexOf(" ") > 0)
            if (Utils.strInt(ss.substring(ss.lastIndexOf(" ")).trim(), -1) > 0)
                ss = ss.substring(0, ss.lastIndexOf(" "));
        Version ver = browser.getVersion(userAgentString);

        if (ver != null)
            ss += " " + ver.getMajorVersion();

        ss = ss.trim();

        if (operatingSystem != OperatingSystem.UNKNOWN) {
            if (!ss.isEmpty())
                ss += ", ";
            ss += operatingSystem.getName();
        }

        if (ss.isEmpty())
            ss = userAgentString;

        return ss;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((browser == null) ? 0 : browser.hashCode());
        result = prime * result + id;
        result = prime * result
                + ((operatingSystem == null) ? 0 : operatingSystem.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        final UserAgent other = (UserAgent) obj;
        if (browser == null) {
            if (other.browser != null)
                return false;
        } else if (!browser.equals(other.browser))
            return false;
        if (id != other.id)
            return false;
        if (operatingSystem == null) {
            if (other.operatingSystem != null)
                return false;
        } else if (!operatingSystem.equals(other.operatingSystem))
            return false;
        return true;
    }

    public static class Version {

        String version;
        String majorVersion;
        String minorVersion;

        public Version(String version, String majorVersion, String minorVersion) {
            super();
            this.version = version;
            this.majorVersion = majorVersion;
            this.minorVersion = minorVersion;
        }

        public String getVersion() {
            return version;
        }

        public String getMajorVersion() {
            return majorVersion;
        }

        public String getMinorVersion() {
            return minorVersion;
        }

        @Override
        public String toString() {
            return version;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + ((majorVersion == null) ? 0 : majorVersion.hashCode());
            result = prime * result
                    + ((minorVersion == null) ? 0 : minorVersion.hashCode());
            result = prime * result + ((version == null) ? 0 : version.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            Version other = (Version) obj;
            if (majorVersion == null) {
                if (other.majorVersion != null)
                    return false;
            } else if (!majorVersion.equals(other.majorVersion))
                return false;
            if (minorVersion == null) {
                if (other.minorVersion != null)
                    return false;
            } else if (!minorVersion.equals(other.minorVersion))
                return false;
            if (version == null) {
                if (other.version != null)
                    return false;
            } else if (!version.equals(other.version))
                return false;
            return true;
        }
    }

    public static enum Browser {

        OPERA_NEXT(Manufacturer.OPERA, null, 1, "Opera", new String[]{"OPR"}, null, BrowserType.WEB_BROWSER, RenderingEngine.WEBKIT, "Opera\\/(([\\d]+)\\.([\\w]+))"),
        OPERA(Manufacturer.OPERA, null, 1, "Opera", new String[]{"Opera"}, null, BrowserType.WEB_BROWSER, RenderingEngine.PRESTO, "Opera\\/(([\\d]+)\\.([\\w]+))"),
        OPERA_MINI(Manufacturer.OPERA, Browser.OPERA, 20, "Opera Mini", new String[]{"Opera Mini"}, null, BrowserType.MOBILE_BROWSER, RenderingEngine.PRESTO, null),
        OPERA10(Manufacturer.OPERA, Browser.OPERA, 10, "Opera 10", new String[]{"Opera/9.8"}, null, BrowserType.WEB_BROWSER, RenderingEngine.PRESTO, "Version\\/(([\\d]+)\\.([\\w]+))"),
        OPERA9(Manufacturer.OPERA, Browser.OPERA, 5, "Opera 9", new String[]{"Opera/9"}, null, BrowserType.WEB_BROWSER, RenderingEngine.PRESTO, null),
        KONQUEROR(Manufacturer.OTHER, null, 1, "Konqueror", new String[]{"Konqueror"}, null, BrowserType.WEB_BROWSER, RenderingEngine.KHTML, "Konqueror\\/(([0-9]+)\\.?([\\w]+)?(-[\\w]+)?)"),
        OUTLOOK(Manufacturer.MICROSOFT, null, 100, "Outlook", new String[]{"MSOffice"}, null, BrowserType.EMAIL_CLIENT, RenderingEngine.WORD, "MSOffice (([0-9]+))"),
        OUTLOOK2007(Manufacturer.MICROSOFT, Browser.OUTLOOK, 107, "Outlook 2007", new String[]{"MSOffice 12"}, null, BrowserType.EMAIL_CLIENT, RenderingEngine.WORD, null),
        OUTLOOK2010(Manufacturer.MICROSOFT, Browser.OUTLOOK, 108, "Outlook 2010", new String[]{"MSOffice 14"}, null, BrowserType.EMAIL_CLIENT, RenderingEngine.WORD, null),
        IE(Manufacturer.MICROSOFT, null, 1, "Internet Explorer", new String[]{"MSIE"}, null, BrowserType.WEB_BROWSER, RenderingEngine.TRIDENT, "MSIE (([\\d]+)\\.([\\w]+))"),
        OUTLOOK_EXPRESS7(Manufacturer.MICROSOFT, Browser.IE, 110, "Windows Live Mail", new String[]{"Outlook-Express/7.0"}, null, BrowserType.EMAIL_CLIENT, RenderingEngine.TRIDENT, null),
        IEMOBILE9(Manufacturer.MICROSOFT, Browser.IE, 123, "IE Mobile 9", new String[]{"IEMobile/9"}, null, BrowserType.MOBILE_BROWSER, RenderingEngine.TRIDENT, null),
        IEMOBILE7(Manufacturer.MICROSOFT, Browser.IE, 121, "IE Mobile 7", new String[]{"IEMobile 7"}, null, BrowserType.MOBILE_BROWSER, RenderingEngine.TRIDENT, null),
        IEMOBILE6(Manufacturer.MICROSOFT, Browser.IE, 120, "IE Mobile 6", new String[]{"IEMobile 6"}, null, BrowserType.MOBILE_BROWSER, RenderingEngine.TRIDENT, null),
        IE10(Manufacturer.MICROSOFT, Browser.IE, 92, "Internet Explorer 10", new String[]{"MSIE 10"}, null, BrowserType.WEB_BROWSER, RenderingEngine.TRIDENT, null),
        IE9(Manufacturer.MICROSOFT, Browser.IE, 90, "Internet Explorer 9", new String[]{"MSIE 9"}, null, BrowserType.WEB_BROWSER, RenderingEngine.TRIDENT, null),
        IE8(Manufacturer.MICROSOFT, Browser.IE, 80, "Internet Explorer 8", new String[]{"MSIE 8"}, null, BrowserType.WEB_BROWSER, RenderingEngine.TRIDENT, null),
        IE7(Manufacturer.MICROSOFT, Browser.IE, 70, "Internet Explorer 7", new String[]{"MSIE 7"}, null, BrowserType.WEB_BROWSER, RenderingEngine.TRIDENT, null),
        IE6(Manufacturer.MICROSOFT, Browser.IE, 60, "Internet Explorer 6", new String[]{"MSIE 6"}, null, BrowserType.WEB_BROWSER, RenderingEngine.TRIDENT, null),
        IE5_5(Manufacturer.MICROSOFT, Browser.IE, 55, "Internet Explorer 5.5", new String[]{"MSIE 5.5"}, null, BrowserType.WEB_BROWSER, RenderingEngine.TRIDENT, null),
        IE5(Manufacturer.MICROSOFT, Browser.IE, 50, "Internet Explorer 5", new String[]{"MSIE 5"}, null, BrowserType.WEB_BROWSER, RenderingEngine.TRIDENT, null),
        CHROME(Manufacturer.GOOGLE, null, 1, "Chrome", new String[]{"Chrome"}, null, BrowserType.WEB_BROWSER, RenderingEngine.WEBKIT, "Chrome\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"),
        CHROME19(Manufacturer.GOOGLE, Browser.CHROME, 24, "Chrome 19", new String[]{"Chrome/19"}, null, BrowserType.WEB_BROWSER, RenderingEngine.WEBKIT, null),
        CHROME18(Manufacturer.GOOGLE, Browser.CHROME, 23, "Chrome 18", new String[]{"Chrome/18"}, null, BrowserType.WEB_BROWSER, RenderingEngine.WEBKIT, null),
        CHROME17(Manufacturer.GOOGLE, Browser.CHROME, 22, "Chrome 17", new String[]{"Chrome/17"}, null, BrowserType.WEB_BROWSER, RenderingEngine.WEBKIT, null),
        CHROME16(Manufacturer.GOOGLE, Browser.CHROME, 21, "Chrome 16", new String[]{"Chrome/16"}, null, BrowserType.WEB_BROWSER, RenderingEngine.WEBKIT, null),
        CHROME15(Manufacturer.GOOGLE, Browser.CHROME, 20, "Chrome 15", new String[]{"Chrome/15"}, null, BrowserType.WEB_BROWSER, RenderingEngine.WEBKIT, null),
        CHROME14(Manufacturer.GOOGLE, Browser.CHROME, 19, "Chrome 14", new String[]{"Chrome/14"}, null, BrowserType.WEB_BROWSER, RenderingEngine.WEBKIT, null),
        CHROME13(Manufacturer.GOOGLE, Browser.CHROME, 18, "Chrome 13", new String[]{"Chrome/13"}, null, BrowserType.WEB_BROWSER, RenderingEngine.WEBKIT, null),
        CHROME12(Manufacturer.GOOGLE, Browser.CHROME, 17, "Chrome 12", new String[]{"Chrome/12"}, null, BrowserType.WEB_BROWSER, RenderingEngine.WEBKIT, null),
        CHROME11(Manufacturer.GOOGLE, Browser.CHROME, 16, "Chrome 11", new String[]{"Chrome/11"}, null, BrowserType.WEB_BROWSER, RenderingEngine.WEBKIT, null),
        CHROME10(Manufacturer.GOOGLE, Browser.CHROME, 15, "Chrome 10", new String[]{"Chrome/10"}, null, BrowserType.WEB_BROWSER, RenderingEngine.WEBKIT, null),
        CHROME9(Manufacturer.GOOGLE, Browser.CHROME, 10, "Chrome 9", new String[]{"Chrome/9"}, null, BrowserType.WEB_BROWSER, RenderingEngine.WEBKIT, null),
        CHROME8(Manufacturer.GOOGLE, Browser.CHROME, 5, "Chrome 8", new String[]{"Chrome/8"}, null, BrowserType.WEB_BROWSER, RenderingEngine.WEBKIT, null),
        OMNIWEB(Manufacturer.OTHER, null, 2, "Omniweb", new String[]{"OmniWeb"}, null, BrowserType.WEB_BROWSER, RenderingEngine.WEBKIT, null),
        SAFARI(Manufacturer.APPLE, null, 1, "Safari", new String[]{"Safari"}, null, BrowserType.WEB_BROWSER, RenderingEngine.WEBKIT, "Version\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?)"),
        CHROME_MOBILE(Manufacturer.GOOGLE, Browser.SAFARI, 100, "Chrome Mobile", new String[]{"CrMo"}, null, BrowserType.MOBILE_BROWSER, RenderingEngine.WEBKIT, "CrMo\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"),
        MOBILE_SAFARI(Manufacturer.APPLE, Browser.SAFARI, 2, "Mobile Safari", new String[]{"Mobile Safari", "Mobile/"}, null, BrowserType.MOBILE_BROWSER, RenderingEngine.WEBKIT, null),
        SILK(Manufacturer.AMAZON, Browser.SAFARI, 15, "Silk", new String[]{"Silk/"}, null, BrowserType.WEB_BROWSER, RenderingEngine.WEBKIT, "Silk\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?(\\-[\\w]+)?)"),
        SAFARI5(Manufacturer.APPLE, Browser.SAFARI, 3, "Safari 5", new String[]{"Version/5"}, null, BrowserType.WEB_BROWSER, RenderingEngine.WEBKIT, null),
        SAFARI4(Manufacturer.APPLE, Browser.SAFARI, 4, "Safari 4", new String[]{"Version/4"}, null, BrowserType.WEB_BROWSER, RenderingEngine.WEBKIT, null),
        DOLFIN2(Manufacturer.SAMSUNG, null, 1, "Samsung Dolphin 2", new String[]{"Dolfin/2"}, null, BrowserType.MOBILE_BROWSER, RenderingEngine.WEBKIT, null),
        APPLE_MAIL(Manufacturer.APPLE, null, 50, "Apple Mail", new String[]{"AppleWebKit"}, null, BrowserType.EMAIL_CLIENT, RenderingEngine.WEBKIT, null),
        LOTUS_NOTES(Manufacturer.OTHER, null, 3, "Lotus Notes", new String[]{"Lotus-Notes"}, null, BrowserType.EMAIL_CLIENT, RenderingEngine.OTHER, "Lotus-Notes\\/(([\\d]+)\\.([\\w]+))"),
        THUNDERBIRD(Manufacturer.MOZILLA, null, 110, "Thunderbird", new String[]{"Thunderbird"}, null, BrowserType.EMAIL_CLIENT, RenderingEngine.GECKO, "Thunderbird\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"),
        THUNDERBIRD12(Manufacturer.MOZILLA, Browser.THUNDERBIRD, 185, "Thunderbird 12", new String[]{"Thunderbird/12"}, null, BrowserType.EMAIL_CLIENT, RenderingEngine.GECKO, null),
        THUNDERBIRD11(Manufacturer.MOZILLA, Browser.THUNDERBIRD, 184, "Thunderbird 11", new String[]{"Thunderbird/11"}, null, BrowserType.EMAIL_CLIENT, RenderingEngine.GECKO, null),
        THUNDERBIRD10(Manufacturer.MOZILLA, Browser.THUNDERBIRD, 183, "Thunderbird 10", new String[]{"Thunderbird/10"}, null, BrowserType.EMAIL_CLIENT, RenderingEngine.GECKO, null),
        THUNDERBIRD8(Manufacturer.MOZILLA, Browser.THUNDERBIRD, 180, "Thunderbird 8", new String[]{"Thunderbird/8"}, null, BrowserType.EMAIL_CLIENT, RenderingEngine.GECKO, null),
        THUNDERBIRD7(Manufacturer.MOZILLA, Browser.THUNDERBIRD, 170, "Thunderbird 7", new String[]{"Thunderbird/7"}, null, BrowserType.EMAIL_CLIENT, RenderingEngine.GECKO, null),
        THUNDERBIRD6(Manufacturer.MOZILLA, Browser.THUNDERBIRD, 160, "Thunderbird 6", new String[]{"Thunderbird/6"}, null, BrowserType.EMAIL_CLIENT, RenderingEngine.GECKO, null),
        THUNDERBIRD3(Manufacturer.MOZILLA, Browser.THUNDERBIRD, 130, "Thunderbird 3", new String[]{"Thunderbird/3"}, null, BrowserType.EMAIL_CLIENT, RenderingEngine.GECKO, null),
        THUNDERBIRD2(Manufacturer.MOZILLA, Browser.THUNDERBIRD, 120, "Thunderbird 2", new String[]{"Thunderbird/2"}, null, BrowserType.EMAIL_CLIENT, RenderingEngine.GECKO, null),
        CAMINO(Manufacturer.OTHER, null, 5, "Camino", new String[]{"Camino"}, null, BrowserType.WEB_BROWSER, RenderingEngine.GECKO, "Camino\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?)"),
        CAMINO2(Manufacturer.OTHER, Browser.CAMINO, 17, "Camino 2", new String[]{"Camino/2"}, null, BrowserType.WEB_BROWSER, RenderingEngine.GECKO, null),
        FLOCK(Manufacturer.OTHER, null, 4, "Flock", new String[]{"Flock"}, null, BrowserType.WEB_BROWSER, RenderingEngine.GECKO, "Flock\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?)"),
        FIREFOX(Manufacturer.MOZILLA, null, 10, "Firefox", new String[]{"Firefox"}, null, BrowserType.WEB_BROWSER, RenderingEngine.GECKO, "Firefox\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?(\\.[\\w]+)?)"),
        FIREFOX3MOBILE(Manufacturer.MOZILLA, Browser.FIREFOX, 31, "Firefox 3 Mobile", new String[]{"Firefox/3.5 Maemo"}, null, BrowserType.MOBILE_BROWSER, RenderingEngine.GECKO, null),
        FIREFOX13(Manufacturer.MOZILLA, Browser.FIREFOX, 94, "Firefox 13", new String[]{"Firefox/13"}, null, BrowserType.WEB_BROWSER, RenderingEngine.GECKO, null),
        FIREFOX12(Manufacturer.MOZILLA, Browser.FIREFOX, 93, "Firefox 12", new String[]{"Firefox/12"}, null, BrowserType.WEB_BROWSER, RenderingEngine.GECKO, null),
        FIREFOX11(Manufacturer.MOZILLA, Browser.FIREFOX, 92, "Firefox 11", new String[]{"Firefox/11"}, null, BrowserType.WEB_BROWSER, RenderingEngine.GECKO, null),
        FIREFOX10(Manufacturer.MOZILLA, Browser.FIREFOX, 91, "Firefox 10", new String[]{"Firefox/10"}, null, BrowserType.WEB_BROWSER, RenderingEngine.GECKO, null),
        FIREFOX9(Manufacturer.MOZILLA, Browser.FIREFOX, 90, "Firefox 9", new String[]{"Firefox/9"}, null, BrowserType.WEB_BROWSER, RenderingEngine.GECKO, null),
        FIREFOX8(Manufacturer.MOZILLA, Browser.FIREFOX, 80, "Firefox 8", new String[]{"Firefox/8"}, null, BrowserType.WEB_BROWSER, RenderingEngine.GECKO, null),
        FIREFOX7(Manufacturer.MOZILLA, Browser.FIREFOX, 70, "Firefox 7", new String[]{"Firefox/7"}, null, BrowserType.WEB_BROWSER, RenderingEngine.GECKO, null),
        FIREFOX6(Manufacturer.MOZILLA, Browser.FIREFOX, 60, "Firefox 6", new String[]{"Firefox/6"}, null, BrowserType.WEB_BROWSER, RenderingEngine.GECKO, null),
        FIREFOX5(Manufacturer.MOZILLA, Browser.FIREFOX, 50, "Firefox 5", new String[]{"Firefox/5"}, null, BrowserType.WEB_BROWSER, RenderingEngine.GECKO, null),
        FIREFOX4(Manufacturer.MOZILLA, Browser.FIREFOX, 40, "Firefox 4", new String[]{"Firefox/4"}, null, BrowserType.WEB_BROWSER, RenderingEngine.GECKO, null),
        FIREFOX3(Manufacturer.MOZILLA, Browser.FIREFOX, 30, "Firefox 3", new String[]{"Firefox/3"}, null, BrowserType.WEB_BROWSER, RenderingEngine.GECKO, null),
        FIREFOX2(Manufacturer.MOZILLA, Browser.FIREFOX, 20, "Firefox 2", new String[]{"Firefox/2"}, null, BrowserType.WEB_BROWSER, RenderingEngine.GECKO, null),
        FIREFOX1_5(Manufacturer.MOZILLA, Browser.FIREFOX, 15, "Firefox 1.5", new String[]{"Firefox/1.5"}, null, BrowserType.WEB_BROWSER, RenderingEngine.GECKO, null),
        SEAMONKEY(Manufacturer.OTHER, null, 15, "SeaMonkey", new String[]{"SeaMonkey"}, null, BrowserType.WEB_BROWSER, RenderingEngine.GECKO, "SeaMonkey\\/(([0-9]+)\\.?([\\w]+)?(\\.[\\w]+)?)"),
        BOT(Manufacturer.OTHER, null, 12, "Robot/Spider", new String[]{"Googlebot", "bot", "spider", "crawler", "Feedfetcher", "Slurp", "Twiceler", "Nutch", "BecomeBot"}, null, BrowserType.ROBOT, RenderingEngine.OTHER, null),
        MOZILLA(Manufacturer.MOZILLA, null, 1, "Mozilla", new String[]{"Mozilla", "Moozilla"}, null, BrowserType.WEB_BROWSER, RenderingEngine.OTHER, null),
        CFNETWORK(Manufacturer.OTHER, null, 6, "CFNetwork", new String[]{"CFNetwork"}, null, BrowserType.UNKNOWN, RenderingEngine.OTHER, null),
        EUDORA(Manufacturer.OTHER, null, 7, "Eudora", new String[]{"Eudora", "EUDORA"}, null, BrowserType.EMAIL_CLIENT, RenderingEngine.OTHER, null),
        POCOMAIL(Manufacturer.OTHER, null, 8, "PocoMail", new String[]{"PocoMail"}, null, BrowserType.EMAIL_CLIENT, RenderingEngine.OTHER, null),
        THEBAT(Manufacturer.OTHER, null, 9, "The Bat!", new String[]{"The Bat"}, null, BrowserType.EMAIL_CLIENT, RenderingEngine.OTHER, null),
        NETFRONT(Manufacturer.OTHER, null, 10, "NetFront", new String[]{"NetFront"}, null, BrowserType.MOBILE_BROWSER, RenderingEngine.OTHER, null),
        EVOLUTION(Manufacturer.OTHER, null, 11, "Evolution", new String[]{"CamelHttpStream"}, null, BrowserType.EMAIL_CLIENT, RenderingEngine.OTHER, null),
        LYNX(Manufacturer.OTHER, null, 13, "Lynx", new String[]{"Lynx"}, null, BrowserType.TEXT_BROWSER, RenderingEngine.OTHER, "Lynx\\/(([0-9]+)\\.([\\d]+)\\.?([\\w-+]+)?\\.?([\\w-+]+)?)"),
        DOWNLOAD(Manufacturer.OTHER, null, 16, "Downloading Tool", new String[]{"cURL", "wget"}, null, BrowserType.TEXT_BROWSER, RenderingEngine.OTHER, null),
        UNKNOWN(Manufacturer.OTHER, null, 14, "Unknown", new String[0], null, BrowserType.UNKNOWN, RenderingEngine.OTHER, null);
        private final short id;
        private final String name;
        private final String[] aliases;
        private final String[] excludeList;
        private final BrowserType browserType;
        private final Manufacturer manufacturer;
        private final RenderingEngine renderingEngine;
        private final Browser parent;
        private List<Browser> children;
        private Pattern versionRegEx;

        private Browser(Manufacturer manufacturer, Browser parent, int versionId, String name, String[] aliases, String[] exclude, BrowserType browserType, RenderingEngine renderingEngine, String versionRegexString) {
            this.id = (short) ((manufacturer.getId() << 8) + (byte) versionId);
            this.name = name;
            this.parent = parent;
            this.children = new ArrayList<Browser>();
            if (this.parent != null)
                this.parent.children.add(this);
            this.aliases = aliases;
            this.excludeList = exclude;
            this.browserType = browserType;
            this.manufacturer = manufacturer;
            this.renderingEngine = renderingEngine;
            if (versionRegexString != null)
                this.versionRegEx = Pattern.compile(versionRegexString);
        }

        public short getId() {
            return id;
        }

        public String getName() {
            return name;
        }

        private Pattern getVersionRegEx() {
            if (this.versionRegEx == null)
                if (this.getGroup() != this)
                    return this.getGroup().getVersionRegEx();
                else
                    return null;
            return this.versionRegEx;
        }

        public Version getVersion(String userAgentString) {
            Pattern pattern = this.getVersionRegEx();
            if (userAgentString != null && pattern != null) {
                Matcher matcher = pattern.matcher(userAgentString);
                if (matcher.find()) {
                    String fullVersionString = matcher.group(1);
                    String majorVersion = matcher.group(2);
                    String minorVersion = "0";
                    if (matcher.groupCount() > 2)
                        minorVersion = matcher.group(3);
                    return new Version(fullVersionString, majorVersion, minorVersion);
                }
            }
            return null;
        }

        public BrowserType getBrowserType() {
            return browserType;
        }

        public Manufacturer getManufacturer() {
            return manufacturer;
        }

        public RenderingEngine getRenderingEngine() {
            return renderingEngine;
        }

        public Browser getGroup() {
            if (this.parent != null)
                return parent.getGroup();
            return this;
        }

        public boolean isInUserAgentString(String agentString) {
            for (String alias : aliases)
                if (agentString.toLowerCase().indexOf(alias.toLowerCase()) != -1)
                    return true;
            return false;
        }

        private boolean containsExcludeToken(String agentString) {
            if (excludeList != null)
                for (String exclude : excludeList)
                    if (agentString.toLowerCase().indexOf(exclude.toLowerCase()) != -1)
                        return true;
            return false;
        }

        private Browser checkUserAgent(String agentString) {
            if (this.isInUserAgentString(agentString)) {
                if (this.children.size() > 0)
                    for (Browser childBrowser : this.children) {
                        Browser match = childBrowser.checkUserAgent(agentString);
                        if (match != null)
                            return match;
                    }

                if (!this.containsExcludeToken(agentString))
                    return this;

            }
            return null;
        }

        public static Browser parseUserAgentString(String agentString) {
            for (Browser browser : Browser.values())

                if (browser.parent == null) {
                    Browser match = browser.checkUserAgent(agentString);
                    if (match != null)
                        return match;
                }
            return Browser.UNKNOWN;
        }

        public static Browser valueOf(short id) {
            for (Browser browser : Browser.values())
                if (browser.getId() == id)
                    return browser;

            throw new IllegalArgumentException(
                    "No enum const for id " + id);
        }
    }

    public static enum RenderingEngine {

        TRIDENT("Trident"),
        WORD("Microsoft Office Word"),
        GECKO("Gecko"),
        WEBKIT("WebKit"),
        PRESTO("Presto"),
        MOZILLA("Mozilla"),
        KHTML("KHTML"),
        OTHER("Other");
        String name;

        private RenderingEngine(String name) {
            this.name = name;
        }
    }

    public static enum OperatingSystem {

        WINDOWS(Manufacturer.MICROSOFT, null, 1, "Windows", new String[]{"Windows"}, new String[]{"Palm"}, DeviceType.COMPUTER, null),
        WINDOWS_8_1(Manufacturer.MICROSOFT, OperatingSystem.WINDOWS, 23, "Windows 8.1", new String[]{"Windows NT 6.3"}, null, DeviceType.COMPUTER, null),
        WINDOWS_8(Manufacturer.MICROSOFT, OperatingSystem.WINDOWS, 22, "Windows 8", new String[]{"Windows NT 6.2"}, null, DeviceType.COMPUTER, null),
        WINDOWS_7(Manufacturer.MICROSOFT, OperatingSystem.WINDOWS, 21, "Windows 7", new String[]{"Windows NT 6.1"}, null, DeviceType.COMPUTER, null),
        WINDOWS_VISTA(Manufacturer.MICROSOFT, OperatingSystem.WINDOWS, 20, "Windows Vista", new String[]{"Windows NT 6"}, null, DeviceType.COMPUTER, null),
        WINDOWS_2000(Manufacturer.MICROSOFT, OperatingSystem.WINDOWS, 15, "Windows 2000", new String[]{"Windows NT 5.0"}, null, DeviceType.COMPUTER, null),
        WINDOWS_XP(Manufacturer.MICROSOFT, OperatingSystem.WINDOWS, 10, "Windows XP", new String[]{"Windows NT 5"}, null, DeviceType.COMPUTER, null),
        WINDOWS_MOBILE7(Manufacturer.MICROSOFT, OperatingSystem.WINDOWS, 51, "Windows Mobile 7", new String[]{"Windows Phone OS 7"}, null, DeviceType.MOBILE, null),
        WINDOWS_MOBILE(Manufacturer.MICROSOFT, OperatingSystem.WINDOWS, 50, "Windows Mobile", new String[]{"Windows CE"}, null, DeviceType.MOBILE, null),
        WINDOWS_98(Manufacturer.MICROSOFT, OperatingSystem.WINDOWS, 5, "Windows 98", new String[]{"Windows 98", "Win98"}, new String[]{"Palm"}, DeviceType.COMPUTER, null),
        ANDROID(Manufacturer.GOOGLE, null, 0, "Android", new String[]{"Android"}, null, DeviceType.MOBILE, null),
        ANDROID4(Manufacturer.GOOGLE, OperatingSystem.ANDROID, 4, "Android 4.x", new String[]{"Android 4", "Android-4"}, null, DeviceType.MOBILE, null),
        ANDROID4_TABLET(Manufacturer.GOOGLE, OperatingSystem.ANDROID4, 40, "Android 4.x Tablet", new String[]{"Xoom", "Transformer"}, null, DeviceType.TABLET, null),
        ANDROID3_TABLET(Manufacturer.GOOGLE, OperatingSystem.ANDROID, 30, "Android 3.x Tablet", new String[]{"Android 3"}, null, DeviceType.TABLET, null),
        ANDROID2(Manufacturer.GOOGLE, OperatingSystem.ANDROID, 2, "Android 2.x", new String[]{"Android 2"}, null, DeviceType.MOBILE, null),
        ANDROID2_TABLET(Manufacturer.GOOGLE, OperatingSystem.ANDROID2, 20, "Android 2.x Tablet", new String[]{"Kindle Fire", "GT-P1000", "SCH-I800"}, null, DeviceType.TABLET, null),
        ANDROID1(Manufacturer.GOOGLE, OperatingSystem.ANDROID, 1, "Android 1.x", new String[]{"Android 1"}, null, DeviceType.MOBILE, null),
        WEBOS(Manufacturer.HP, null, 11, "WebOS", new String[]{"webOS"}, null, DeviceType.MOBILE, null),
        PALM(Manufacturer.HP, null, 10, "PalmOS", new String[]{"Palm"}, null, DeviceType.MOBILE, null),
        IOS(Manufacturer.APPLE, null, 2, "iOS", new String[]{"like Mac OS X"}, null, DeviceType.MOBILE, null),
        iOS5_IPHONE(Manufacturer.APPLE, OperatingSystem.IOS, 42, "iOS 5 (iPhone)", new String[]{"iPhone OS 5"}, null, DeviceType.MOBILE, null),
        iOS4_IPHONE(Manufacturer.APPLE, OperatingSystem.IOS, 41, "iOS 4 (iPhone)", new String[]{"iPhone OS 4"}, null, DeviceType.MOBILE, null),
        MAC_OS_X_IPAD(Manufacturer.APPLE, OperatingSystem.IOS, 50, "Mac OS X (iPad)", new String[]{"iPad"}, null, DeviceType.TABLET, null),
        MAC_OS_X_IPHONE(Manufacturer.APPLE, OperatingSystem.IOS, 40, "Mac OS X (iPhone)", new String[]{"iPhone"}, null, DeviceType.MOBILE, null),
        MAC_OS_X_IPOD(Manufacturer.APPLE, OperatingSystem.IOS, 30, "Mac OS X (iPod)", new String[]{"iPod"}, null, DeviceType.MOBILE, null),
        MAC_OS_X(Manufacturer.APPLE, null, 10, "Mac OS X", new String[]{"Mac OS X", "CFNetwork"}, null, DeviceType.COMPUTER, null),
        MAC_OS(Manufacturer.APPLE, null, 1, "Mac OS", new String[]{"Mac"}, null, DeviceType.COMPUTER, null),
        MAEMO(Manufacturer.NOKIA, null, 2, "Maemo", new String[]{"Maemo"}, null, DeviceType.MOBILE, null),
        BADA(Manufacturer.SAMSUNG, null, 2, "Bada", new String[]{"Bada"}, null, DeviceType.MOBILE, null),
        GOOGLE_TV(Manufacturer.GOOGLE, null, 100, "Android (Google TV)", new String[]{"GoogleTV"}, null, DeviceType.DMR, null),
        KINDLE(Manufacturer.AMAZON, null, 1, "Linux (Kindle)", new String[]{"Kindle"}, null, DeviceType.TABLET, null),
        KINDLE3(Manufacturer.AMAZON, OperatingSystem.KINDLE, 30, "Linux (Kindle 3)", new String[]{"Kindle/3"}, null, DeviceType.TABLET, null),
        KINDLE2(Manufacturer.AMAZON, OperatingSystem.KINDLE, 20, "Linux (Kindle 2)", new String[]{"Kindle/2"}, null, DeviceType.TABLET, null),
        LINUX(Manufacturer.OTHER, null, 2, "Linux", new String[]{"Linux", "CamelHttpStream"}, null, DeviceType.COMPUTER, null),
        SYMBIAN(Manufacturer.SYMBIAN, null, 1, "Symbian OS", new String[]{"Symbian", "Series60"}, null, DeviceType.MOBILE, null),
        SYMBIAN9(Manufacturer.SYMBIAN, OperatingSystem.SYMBIAN, 20, "Symbian OS 9.x", new String[]{"SymbianOS/9", "Series60/3"}, null, DeviceType.MOBILE, null),
        SYMBIAN8(Manufacturer.SYMBIAN, OperatingSystem.SYMBIAN, 15, "Symbian OS 8.x", new String[]{"SymbianOS/8", "Series60/2.6", "Series60/2.8"}, null, DeviceType.MOBILE, null),
        SYMBIAN7(Manufacturer.SYMBIAN, OperatingSystem.SYMBIAN, 10, "Symbian OS 7.x", new String[]{"SymbianOS/7"}, null, DeviceType.MOBILE, null),
        SYMBIAN6(Manufacturer.SYMBIAN, OperatingSystem.SYMBIAN, 5, "Symbian OS 6.x", new String[]{"SymbianOS/6"}, null, DeviceType.MOBILE, null),
        SERIES40(Manufacturer.NOKIA, null, 1, "Series 40", new String[]{"Nokia6300"}, null, DeviceType.MOBILE, null),
        SONY_ERICSSON(Manufacturer.SONY_ERICSSON, null, 1, "Sony Ericsson", new String[]{"SonyEricsson"}, null, DeviceType.MOBILE, null),
        SUN_OS(Manufacturer.SUN, null, 1, "SunOS", new String[]{"SunOS"}, null, DeviceType.COMPUTER, null),
        PSP(Manufacturer.SONY, null, 1, "Sony Playstation", new String[]{"Playstation"}, null, DeviceType.GAME_CONSOLE, null),
        WII(Manufacturer.NINTENDO, null, 1, "Nintendo Wii", new String[]{"Wii"}, null, DeviceType.GAME_CONSOLE, null),
        BLACKBERRY(Manufacturer.BLACKBERRY, null, 1, "BlackBerryOS", new String[]{"BlackBerry"}, null, DeviceType.MOBILE, null),
        BLACKBERRY7(Manufacturer.BLACKBERRY, OperatingSystem.BLACKBERRY, 7, "BlackBerry 7", new String[]{"Version/7"}, null, DeviceType.MOBILE, null),
        BLACKBERRY6(Manufacturer.BLACKBERRY, OperatingSystem.BLACKBERRY, 6, "BlackBerry 6", new String[]{"Version/6"}, null, DeviceType.MOBILE, null),
        BLACKBERRY_TABLET(Manufacturer.BLACKBERRY, null, 100, "BlackBerry Tablet OS", new String[]{"RIM Tablet OS"}, null, DeviceType.TABLET, null),
        ROKU(Manufacturer.ROKU, null, 1, "Roku OS", new String[]{"Roku"}, null, DeviceType.DMR, null),
        UNKNOWN(Manufacturer.OTHER, null, 1, "Unknown", new String[0], null, DeviceType.UNKNOWN, null);
        private final short id;
        private final String name;
        private final String[] aliases;
        private final String[] excludeList;
        private final Manufacturer manufacturer;
        private final DeviceType deviceType;
        private final OperatingSystem parent;
        private List<OperatingSystem> children;
        private Pattern versionRegEx;

        private OperatingSystem(Manufacturer manufacturer, OperatingSystem parent, int versionId, String name, String[] aliases,
                String[] exclude, DeviceType deviceType, String versionRegexString) {
            this.manufacturer = manufacturer;
            this.parent = parent;
            this.children = new ArrayList<OperatingSystem>();
            if (this.parent != null)
                this.parent.children.add(this);

            this.id = (short) ((manufacturer.getId() << 8) + (byte) versionId);
            this.name = name;
            this.aliases = aliases;
            this.excludeList = exclude;
            this.deviceType = deviceType;
            if (versionRegexString != null)
                this.versionRegEx = Pattern.compile(versionRegexString);
        }

        public short getId() {
            return id;
        }

        public String getName() {
            return name;
        }

        public boolean isMobileDevice() {
            return deviceType.equals(DeviceType.MOBILE);
        }

        public DeviceType getDeviceType() {
            return deviceType;
        }

        public OperatingSystem getGroup() {
            if (this.parent != null)
                return parent.getGroup();
            return this;
        }

        public Manufacturer getManufacturer() {
            return manufacturer;
        }

        public boolean isInUserAgentString(String agentString) {
            for (String alias : aliases)
                if (agentString.toLowerCase().indexOf(alias.toLowerCase()) != -1)
                    return true;
            return false;
        }

        private boolean containsExcludeToken(String agentString) {
            if (excludeList != null)
                for (String exclude : excludeList)
                    if (agentString.toLowerCase().indexOf(exclude.toLowerCase()) != -1)
                        return true;
            return false;
        }

        private OperatingSystem checkUserAgent(String agentString) {
            if (this.isInUserAgentString(agentString)) {
                if (this.children.size() > 0)
                    for (OperatingSystem childOperatingSystem : this.children) {
                        OperatingSystem match = childOperatingSystem.checkUserAgent(agentString);
                        if (match != null)
                            return match;
                    }

                if (!this.containsExcludeToken(agentString))
                    return this;

            }
            return null;
        }

        public static OperatingSystem parseUserAgentString(String agentString) {
            for (OperatingSystem operatingSystem : OperatingSystem.values())

                if (operatingSystem.parent == null) {
                    OperatingSystem match = operatingSystem.checkUserAgent(agentString);
                    if (match != null)
                        return match;
                }
            return OperatingSystem.UNKNOWN;
        }

        public static OperatingSystem valueOf(short id) {
            for (OperatingSystem operatingSystem : OperatingSystem.values())
                if (operatingSystem.getId() == id)
                    return operatingSystem;

            throw new IllegalArgumentException(
                    "No enum const for id " + id);
        }
    }

    public static enum Manufacturer {

        OTHER(1, "Other"),
        MICROSOFT(2, "Microsoft Corporation"),
        APPLE(3, "Apple Inc."),
        SUN(4, "Sun Microsystems, Inc."),
        SYMBIAN(5, "Symbian Ltd."),
        NOKIA(6, "Nokia Corporation"),
        BLACKBERRY(7, "Research In Motion Limited"),
        HP(8, "Hewlet Packard"),
        SONY_ERICSSON(9, "Sony Ericsson Mobile Communications AB"),
        SAMSUNG(20, "Samsung Electronics"),
        SONY(10, "Sony Computer Entertainment, Inc."),
        NINTENDO(11, "Nintendo"),
        OPERA(12, "Opera Software ASA"),
        MOZILLA(13, "Mozilla Foundation"),
        GOOGLE(15, "Google Inc."),
        COMPUSERVE(16, "CompuServe Interactive Services, Inc."),
        YAHOO(17, "Yahoo Inc."),
        AOL(18, "AOL LLC."),
        MMC(19, "Mail.com Media Corporation"),
        AMAZON(20, "Amazon.com, Inc."),
        ROKU(21, "Roku, Inc.");
        private final byte id;
        private final String name;

        private Manufacturer(int id, String name) {
            this.id = (byte) id;
            this.name = name;
        }

        public byte getId() {
            return id;
        }

        public String getName() {
            return name;
        }
    }

    public static enum DeviceType {

        COMPUTER("Computer"),
        MOBILE("Mobile"),
        TABLET("Tablet"),
        GAME_CONSOLE("Game console"),
        DMR("Digital media receiver"),
        UNKNOWN("Unknown");
        String name;

        private DeviceType(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }
    }

    public static enum BrowserType {

        WEB_BROWSER("Browser"),
        MOBILE_BROWSER("Browser (mobile)"),
        TEXT_BROWSER("Browser (text only)"),
        EMAIL_CLIENT("Email Client"),
        ROBOT("Robot"),
        TOOL("Downloading tool"),
        UNKNOWN("unknown");
        private String name;

        private BrowserType(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }
    }

    public static enum ApplicationType {

        WEBMAIL("Webmail client"),
        UNKNOWN("unknown");
        private String name;

        private ApplicationType(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }
    }

    public static enum Application {

        HOTMAIL(Manufacturer.MICROSOFT, 1, "Windows Live Hotmail",
                new String[]{"mail.live.com", "hotmail.msn"}, ApplicationType.WEBMAIL),
        GMAIL(Manufacturer.GOOGLE, 5, "Gmail",
                new String[]{"mail.google.com"}, ApplicationType.WEBMAIL),
        YAHOO_MAIL(Manufacturer.YAHOO, 10, "Yahoo Mail",
                new String[]{"mail.yahoo.com"}, ApplicationType.WEBMAIL),
        COMPUSERVE(Manufacturer.COMPUSERVE, 20, "Compuserve",
                new String[]{"csmail.compuserve.com"}, ApplicationType.WEBMAIL),
        AOL_WEBMAIL(Manufacturer.AOL, 30, "AOL webmail",
                new String[]{"webmail.aol.com"}, ApplicationType.WEBMAIL),
        MOBILEME(Manufacturer.APPLE, 40, "MobileMe",
                new String[]{"www.me.com"}, ApplicationType.WEBMAIL),
        MAIL_COM(Manufacturer.MMC, 50, "Mail.com",
                new String[]{".mail.com"}, ApplicationType.WEBMAIL),
        HORDE(Manufacturer.OTHER, 50, "horde",
                new String[]{"horde"}, ApplicationType.WEBMAIL),
        OTHER_WEBMAIL(Manufacturer.OTHER, 60, "Other webmail client",
                new String[]{"webmail", "webemail"}, ApplicationType.WEBMAIL),
        UNKNOWN(Manufacturer.OTHER, 0, "Unknown",
                new String[0], ApplicationType.UNKNOWN);
        private final short id;
        private final String name;
        private final String[] aliases;
        private final ApplicationType applicationType;
        private final Manufacturer manufacturer;

        private Application(Manufacturer manufacturer, int versionId, String name,
                String[] aliases, ApplicationType applicationType) {
            this.id = (short) ((manufacturer.getId() << 8) + (byte) versionId);
            this.name = name;
            this.aliases = aliases;
            this.applicationType = applicationType;
            this.manufacturer = manufacturer;
        }

        public short getId() {
            return id;
        }

        public String getName() {
            return name;
        }

        public ApplicationType getApplicationType() {
            return applicationType;
        }

        public Manufacturer getManufacturer() {
            return manufacturer;
        }

        public boolean isInReferrerString(String referrerString) {
            for (String alias : aliases)
                if (referrerString.toLowerCase().indexOf(alias.toLowerCase()) != -1)
                    return true;
            return false;
        }

        public static Application parseReferrerString(String referrerString) {

            if (referrerString != null && referrerString.length() > 1)
                for (Application applicationInList : Application.values())
                    if (applicationInList.isInReferrerString(referrerString))
                        return applicationInList;
            return Application.UNKNOWN;
        }

        public static Application valueOf(short id) {
            for (Application application : Application.values())
                if (application.getId() == id)
                    return application;
            throw new IllegalArgumentException("No enum const for id " + id);
        }
    }
}
