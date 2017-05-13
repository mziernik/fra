package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

@LangDict(name = "Config")
public enum LConfig implements LString {
    ACTIVE("Aktywny", "Active"),
    DATABASE("Baza danych", "Databese"),
    DB_NAME("Nazwa bazy", "Database name"),
    DOMAIN("Domena", "Domain"),
    ENABLED("Aktywna", "Enabled"),
    GROUP("Grupa", "Group"),
    HOST("Host", "Host"),
    IP("IP", "IP"),
    KEY("Klucz", "Key"),
    LANG("Język", "Language"),
    LOGIN("Login", "Login"),
    PASS("Hasło", "Password"),
    PATH("Ścieżka", "Path"),
    PORT("Port", "Port"),
    PROCESS("Proces", "Process"),
    RECIPIENT("Odbiorca", "Recipient"),
    RECIPIENTS("Odbiorcy", "Recipients"),
    SSH("SSH", "SSH"),
    TIMEOUT("Timeout", "Timeout"),
    TYPE("Typ", "Type"),
    URL("URL", "URL"),
    USER("Użytkownik", "User"),
    LACK("<brak>", "<lack>"),
    // Administration ------------------------
    ADMINISTRATION__ADMINISTRATION("Administracja", "Administration"),
    ADMINISTRATION__EMAIL_RECIPIENTS("Odbiorcy", "Recipients"),
    // Administration -> Registration --------
    REGISTRATION__REGISTRATION("Rejestracja użytkowników", "Registration"),
    REGISTRATION__EMAIL_VERIFICATION("Weryfikacja emaila", "Email verification"),
    REGISTRATION__ADMIN_VERIFICATION("Weryfikacja admina", "Admin verification"),
    REGISTRATION__PASSWORD_REQUIRED("Wymagane hasło", "Password required"),
    REGISTRATION__GROUPS("Wybór grup", "Groups"),
    // Administration -> SMTP ----------------
    SMTP__SMTP("SMTP", "SMTP"),
    SMTP__SMTP__DESCRIPTION("Serwer poczty wychodzącej", "Outgoing mail server"),
    SMTP__SENDER("Nadawca - email", "Sender - email"),
    SMTP__NAME("Nadawca - nazwa", "Sender - name"),
    // Authorization -------------------------
    AUTH__AUTH("Autoryzacja", "Authorization"),
    AUTH__SSL("SSL", "SSL"),
    AUTH__COOKIE_EXPIRE("Ważność ciastka", "Cookie expire"),
    AUTH__USE_HTACCESS("HT Access", "HT Access"),
    AUTH__AUTO_LOGIN("Automatyczne logowanie", "Auto login"),
    // Authorization -> LDAP -----------------
    LDAP__LDAP("LDAP", "LDAP"),
    LDAP__URL__DESCRIPTION("np: ldap://localhost:389/c=domain", "np: ldap://localhost:389/c=domain"),
    LDAP__ADMIN_LOGIN("Login admina", "Admin login"),
    LDAP__ADMIN_PASS("Hasło admina", "Admin password"),
    LDAP__LIST_USERS_FILTER("Filtr listy użytkowników", "List user filter"),
    LDAP__LIST_USERS_FILTER__DESCRIPTION("Pobieranie listy użytkowników z LDAP-a (np podczas tworzenia nowego użytkownika).\n"
            + "Jesli wartość jest pusta, lista nie zostanie pobrana",
            "Retrieve a list of users from LDAP-a (for example, when creating a new user).\nIf the value is empty, the list will not be charged"),
    // Cache ---------------------------------
    CACHE__CACHE("Cache", "Cache"),
    CACHE__MAX_MEMORY_SIZE("Maksymalny rozmiar pliku w pamięci", "Maximum file size in memory"),
    CACHE__EXPIRE_TIME("Domyślny czas wygaśnięcia", "Default expiration time"),
    // Console -------------------------------
    CONSOLE__CONSOLE("Konsola", "Console"),
    CONSOLE__MAX_LOGS_COUNT("Maksymalna ilość logów", "Maximum logs count"),
    // Content -------------------------------
    CONTENT__CONTENT("Treść", "Content"),
    CONTENT__COMPACT("Tryb kompaktowy", "Compact mode"),
    CONTENT__COMPACT__DESCRIPTION("Bez białych znaków, układ zwarty", "Without whitespace, compact layout"),
    CONTENT__COMPACT_JS("Formatuj skrypty w trybie kompaktowym", "Format scripts in compact mode"),
    CONTENT__COMPACT_JS__DESCRIPTION("Usuwa entery ze skryptu", "Removes line brakes from the script"),
    CONTENT__XHTML_MODE("Tryb XHTML", "XHTML mode"),
    CONTENT__ADD_HREF_UID("Identyfikator instancji serwera jako dodatkowy parametr URL", "ID server instance as an additional URL parameter"),
    CONTENT__ADD_HREF_UID__DESCRIPTION("Dopisuje skrócony identyfikator instancji serwera blokując cachowanie plików przez przeglądarkę", "Writes shortened identifier server instance blocking file caching by browser"),
    CONTENT__ETAGS("Obsługa ETag-ów", "ETagS"),
    CONTENT__MAX_ROWS_COUNT("Maksymalna liczba wyświetlanych wierszy tabeli", "The maximum number of rows to display in table"),
    CONTENT__ERROR_PAGE_MEMORY_INFO("Informacje o pamięci", "Memory information"),
    CONTENT__COMPRESSION_MIME_TYPES("Kompresja \'MIME Types\'", "\'MIME Types\' Compression"),
    CONTENT__SEARCH_INDEX_HTML("Szukaj pliku index.html w katalogu żądania", "Search index.html file in a directory request"),
    // Cron ----------------------------------
    CRON__CRON("CRON", "Cron"),
    CRON__LOG_EVENTS("Logowanie zdarzeń", "Event logging"),
    // Debug ---------------------------------
    DEBUG__DEBUG("Uruchomienie / zatrzymanie wątku", "Start / Stop Thread"),
    DEBUG__THREAD_EVENTS("Uruchomienie / zatrzymanie wątku", "Start / Stop Thread"),
    DEBUG__STATISTICS_ENABLED("Statystyki", "Statistics"),
    DEBUG__CONNECTION_EVENTS("Zdarzenia managera połączeń", "Connection manager events"),
    DEBUG__PROCESS_STDOUT("Logowanie wyjścia standardowego procesów", "Log  standart output of process"),
    DEBUG__PROCESS_STDOUT__DESCRIPTION("Lista fraz będących ścieżkami lub nazwami procesów,\n"
            + "których wyjście standardowe (stdout, stderr) będzie logowane.\n"
            + "Wielkość liter jest ignorowana.", "The list of phrases that are paths or names of processes\n, whose standard output (stdout, stderr) will be logged.\n Case is ignored."),
    // HTTP ----------------------------------
    HTTP__HTTP("HTTP", "HTTP"),
    HTTP__URL("Adres URL usługi", "Service URL address"),
    HTTP__URL__DESCRIPTION("Adres url pod którym można wywołać usługę", "Url address using to call service"),
    HTTP__SESSION_MAX_INTERVAL("Maksymalny czas nieaktywności sesji", "Session maximum interval"),
    HTTP__MAX_REQUESTS("Maksymalna liczba żądań przetwarzanych jednocześnie", "Maximum number of requests processed simultaneously"),
    HTTP__MAX_REQUEST_QUEUE("Maksymalna liczba żądań przetwarzanych jednocześnie", "Maximum number of requests processed simultaneously"),
    HTTP__AUTO_DETECT_LANGUAGE("Wykrywaj automatycznie język (Nagłówek Accept-Language)", "Auto detect language (Accept-Language header)"),
    HTTP__USE_LANGUAGE_COOKIE("Używaj ciastka 'language'", "Use 'language' cookie"),
    HTTP__MAX_SESSIONS("Maksymalna liczba sesji", "Maximum session count"),
    // Logs ----------------------------------
    LOGS__LOGS("Logi", "Logs"),
    LOGS__PROTOCOLS("Protokoły", "Protocols"),
    LOGS__PROTOCOLS__DESCRIPTION("Obsługiwane protokoły: udp://, file://, telnet://", "Supported protocols: udp://, file://, telnet:// "),
    LOGS__JAVASCRIPT_ERRORS("Błędy JavaScript", "JavaScript errors"),
    LOGS__JAVASCRIPT_MESSAGES("Logi JavaScript", "JavaScript logs"),
    // Proxy ---------------------------------
    PROXY__PROXY("Serwer Proxy", "Proxy server"),
    PROXY__BYPASS("Pomiń adresy", "Bypass addresses"),
    PROXY__BYPASS__DESCRIPTION("Adresy hostów, do których połączenia będą"
            + " się odbywać z pominięciem proxy (np loclahost, 192.168.*)", "Host addresses bypassing by proxy"),
    // Resources -----------------------------
    RES__RES("Zasoby", "Resources"),
    RES__USE_CACHE("Włącz cachowanie", "Use cache"),
    RES__CHECK_CHANGES("Monitoruj zmiany plików", "Monitor file chaned"),
    RES__EXPIRE("Czas życia", "Time to live"),
    RES__EXPIRE__DESCRIPTION("Maksymalny czas życia elementu w cache-u", "Maximum time to live cached items"),
    RES__CACHE_SIZE("Maksymalna ilość elementów w cache-u", "Maximum cache size"),
    RES__MAX_CACHED_ITEM_SIZE("Maksymalny rozmiar elementu w cache-u", "Maximum cached item size"),
    // Service -------------------------------
    SERVICE_MODE__DEV("Deweloperski", "Develop"),
    SERVICE_MODE__TEST("Testowy", "Test"),
    SERVICE_MODE__RELEASE("Produkcyjny", "Release"),
    SERVICE__SERVICE("Usługa", "Service"),
    SERVICE__MODE("Tryb pracy serwera", "Server mode"),
    SERVICE__SESSION_MAX_INTERVAL("Maksymalny czas nieaktywności sesji", "Session maximum interval"),
    SERVICE__MAX_REQUESTS("Maksymalna liczba żądań przetwarzanych jednocześnie", "Maximum number of requests processed simultaneously"),
    SERVICE__MAX_REQUEST_QUEUE("Maksymalna liczba żądań oczekujących w kolejce", "Maximum number of requests waiting in the queue"),
    SERVICE__MAX_SESSIONS("Maksymalna liczba sesji", "Maximum number of sessions"),
    SERVICE__URL("Adres url usługi", "Service url address"),
    SERVICE__LANGUAGE("Język", "Language"),
    SERVICE__LOCALE("Lokalizacja", "Locale"),
    HTTP__FDN("FDN usługi (IP, host lub domena)", "Service FDN (IP, host or domain)"),
    // Service -> Technical Break ------------
    SERVICE_BREAK__SERVICE_BREAK("Przerwa techniczna", "Technical break"),
    SERVICE_BREAK__SERVICE_BREAK__DEF_VALUE("Usługa jest chwilowo niedostępna.\nTrwa przerwa techniczna.", "The service is temporarily unavailable.\nMaintenance underway."),
    SERVICE_BREAK__ENABLED("Aktywna", "Enabled"),
    SERVICE_BREAK__MESSAGE("Treść komunikatu", "Message content"),
    SERVICE_BREAK__DETAILS("Szczegóły", "Details"),
    SERVICE_BREAK__EXCEPTION_LIST("Lista wyjątków", "Exception list"),
    SERVICE_BREAK__EXCEPTION_LIST__DESCRIPTION("Lista adresów IP, z których żądania zostaną obsłuzone pomimo przerwy technicznej", "The list of IP addresses from which requests will be handled despite the technical break"),
    SERVICE_BREAK__ALLOW_LOCAL("Zezwól na połączenia lokalne", "Allow local connections"),
    // Storage -------------------------------
    STORAGE__STORAGE("Magazyn lokalny", "Local storagte"),
    STORAGE__MAX_FILE_SIZE("Maksymalny rozmiar pliku", "Maximum file size"),
    STORAGE__SUSPEND_TIME("Maksymalny czas wstrzymania", "Maximum suspend time"),
    STORAGE__SUSPEND_TIME__DESCRIPTION("Większy czas zmniejsza ilość operacji zapisu dyskowego, kosztem większego zużycia pamięci (buforowanie)",
            "Bigger time reduces the amount of disk write operations, at the expense of greater memory (caching)"),
    STORAGE__MEMORY_BUFER_LIMIT("Maksymalny rozmiar bufora pamięci", "Memory buffer limit"),
    STORAGE__DAYS_RANGE("Zakres dni", "Days range"),
    STORAGE__DAYS_RANGE__DESCRIPTION("Ilość dni, z których logi zapisane będą w jednym pliku", "Number of days of logs will be stored in one file"),
    STORAGE__PAGE_LOGS_LIMIT("Maksymalna ilość wyświetlanych logów", "The maximum number of displayed logs");

    // <editor-fold defaultstate="collapsed">
    private LConfig(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>
}
