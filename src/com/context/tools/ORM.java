package com.context.tools;

import com.utils.console.TConsole;
import com.context.AppContext;
import com.database.drivers.postgresql.PostgreSQL;
import com.database.orm.generator.OrmBuilder;
import com.utils.Url;

public class ORM {

    public static void main(String[] args) {
        try {
            ormBuild(args);
            return;
        } catch (Throwable e) {
            TConsole.printErr(e);
        } finally {
            if (!AppContext.unitTestMode)
                System.exit(0);
        }
    }

    private static void ormBuild(String[] args) throws Exception {

        if (args.length < 4)
            throw new Error("Nieprawidłowa ilość argumentów");

        TConsole.print("Tworzenie klas mapowania relacyjno-obiektowego");
        TConsole.print("Klasa bazy danych: " + args[1]);
        TConsole.print("Połączenie: " + args[2]);
        TConsole.print("Pakiet: " + args[3]);
        TConsole.print("Źródła: " + AppContext.sourcesPath);
        TConsole.print("");

        Class.forName("org.postgresql.Driver");

        Url url = new Url("jdbc://" + args[2]);

        PostgreSQL db = new PostgreSQL(url.host(), url.path().first(), url.username(), url.password());

        new OrmBuilder(db, args[1], args[3]).generateStructure();
    }

}
