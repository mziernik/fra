package com.lang;

import com.lang.core.LString;
import com.lang.core.Languages;

public enum LCron implements LString {

    COUNTING_DOWN("Odliczenie", "Counting down"),
    ACTIVE_BETWEEN("Aktywne pomiędzy:", "Acitve between:"),
    LOCKED_BETWEEN("Zablokowane pomiędzy:", "Locked between:"),
    DAYS_OF_WEEK("Dni tygodnia:", "Days of week:"),
    DAYS_OF_MONTH("Dni miesiąca:", "Days of month"),
    DAYS_OF_YEAR("Dni roku:", "Days of year:"),
    WEEKS_OF_MONTH("Tygodnie miesiąca:", "Weeks of month:"),
    WEEKS_OF_YEAR("Tygodnie roku:", "Weeks of year"),
    MONTHS("Miesiące:", "Months:"),
    DATES("Daty:", "Dates:"),
    SINGLE("Pojedynczy", "Single"),
    INTERVAL("Interwał:", "Interval:"),
    DELAY("Opóźnienie", "Delay"),
    SECONDS("sekund", "seconds"),
    HOURS("Godziny", "Hoours"),
    HOUR("Godzina", "Hoour"),
    MINUTE("Minuta", "Minute"),
    SECOND("Sekunda", "Second"),
    ERROR("Błąd", "Error"),
    TIME("Czas", "Time"),
    TASK("zadanie", "task"),
    DELETING("Usuwam", "Deleting"),
    TURNING_OFF("Wyłączam", "Turning off"),
    TASK_ID("Id zadanie", "Task ID"),
    SUBTASK("Podzadanie", "Subtask"),
    MANUALLY("Ręcznie", "Manually"),
    SCHEDULE("Haromogram", "Schedule"),
    STARTED("Rospoczęto", "Started"),
    FINISHED("Zakończono", "Finished"),
    DURATION("Czas trwania", "Duration"),
    // --
    CRONTASK_LACK_OF_CTOR("Klasa %1 nie posiada konstruktora (TaskRunnableInstance runnable)",
            "Class %1 doesn't heve constructor(TaskRunnableInstance runnable)"),
    CRONTASK_METHOD_CANT_ACCPET_PARAMS("Metoda %1.%2 nie może mieć parametrów",
            "Method %1.%2 can not accept parameters"),
    CRONTASK_NO_PERMISSION_TO_RUN_TASK("Brak uprawnień do uruchomienia zadania",
            "No permission to run the task"),
    CRONTASK_TOO_MANY_INSTANCES("Nie można uruchomić więcej niż %1 instancji jednocześnie",
            "You can not run more than %1 instances at the same time"),
    CRONTASK_STARTING_TASK("Uruchamiam zadanie %1", "Starting taks %1"),
    CRONTASK_TEST_MODE("Tryb testowy", "Test mode"),
    CRONTASK_CALL_STACK("Stos Metod", "Call stack"),
    CRONTASK_TOO_MANY_ERRORS("Przekroczono maksymalną dopuszczaną ilość błędów.",
            "Maximum number of errors exceeded."),
    CRONTASK_STOPPING_TASK("Wyłączam zadanie", "Stopping task"),
    CRONTASK_TASK_DONE("Wykonano zadanie %1", "Task %1 is done"),
    CRONTASK_TOO_MANY_CALLS("Przekroczono maksymalną dopuszczaną ilość wywołąń",
            "Too many calls");

    // <editor-fold defaultstate="collapsed">
    private LCron(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>

}
