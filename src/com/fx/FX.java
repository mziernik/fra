package com.fx;

import com.exceptions.EError;
import com.exceptions.ThrowableException;
import com.intf.runnable.RunnableEx;
import com.utils.collections.Strings;
import com.utils.console.TConsole;
import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.net.*;
import javafx.application.Platform;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.layout.*;

public class FX {

    public static void sync(final RunnableEx intf) {
        Platform.runLater(() -> {
            try {
                intf.run();
            } catch (Throwable e) {
                FxApplication.onException(e);
            }
        });
    }

    /**
     * Wczytuje plik FXML i zwraca instancję kontrolera. Jeśli kontroler i plik
     * fxml są w tym samym pakiecie, wystarczy podać samą nazwę pliku fxml
     *
     * @param <Controller>
     * @param cls
     * @param fxmlName
     * @return
     * @throws IOException
     * @throws URISyntaxException
     */
    public static <Controller> Controller loadFXML(Class<Controller> cls, String fxmlName) {

        URL url = cls.getResource(fxmlName);
        if (url == null)
            throw new RuntimeException("Nie znaleziono pliku " + fxmlName);
        try {
            String surl = url.toString();
            if (surl.startsWith("jar:") && surl.contains("/dist/run") && surl.contains("!")) {
                surl = surl.substring(4, surl.indexOf("/dist/run")) + "/src" + surl.substring(surl.indexOf("!") + 1);

                if (new File(new URI(surl)).exists()) {
                    url = new URL(surl);
                    TConsole.print("Mapowanie pliku " + fxmlName + " na " + surl);

                }
            }

            FXMLLoader loader = new FXMLLoader(url);
            loader.load();
            return loader.<Controller>getController();
        } catch (Exception e) {
            throw new ThrowableException(e);
        }
    }

    public static File getSourceFile(Class<?> cls, String fileName) {
        URL url = cls.getResource(fileName != null ? fileName : "");
        String surl = url.toString();
        if (surl.startsWith("jar:") && surl.contains("/dist/run") && surl.contains("!")) {
            surl = surl.substring(4, surl.indexOf("/dist/run")) + "/src" + surl.substring(surl.indexOf("!") + 1);

            try {
                return new File(new URI(surl));
            } catch (URISyntaxException ex) {
                throw new ThrowableException(ex);
            }

        }

        return null;
    }

    public static void loadCSS(Scene scene, Class<?> cls, String cssFileName) {
        try {
            File file = getSourceFile(cls, cssFileName);
            if (file != null)
                scene.getStylesheets().add(file.toURI().toString());
        } catch (Exception e) {
            throw new ThrowableException(e);
        }
    }

    public static void showException(final Throwable ex) {

        if (!Platform.isFxApplicationThread()) {
            Platform.runLater(new Runnable() {

                @Override
                public void run() {
                    showException(ex);
                }
            });
            return;
        }

        Strings strings = new Strings();

        EError err = new EError(ex);

        Throwable e = ex;
        while (e != null && e.getCause() != null
                && (e instanceof InvocationTargetException || e instanceof RuntimeException))
            e = e.getCause();

        Alert alert = new Alert(AlertType.ERROR);
        alert.setTitle(err.shortClasses.toString(", "));
        alert.setHeaderText(err.toString(false));

        alert.setWidth(500);

        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        ex.printStackTrace(pw);
        String exceptionText = sw.toString();

        Label label = new Label("Stos wywołań:");

        TextArea textArea = new TextArea(exceptionText);
        textArea.setEditable(false);
        textArea.setWrapText(false);

        textArea.setMaxWidth(Double.MAX_VALUE);
        textArea.setMaxHeight(Double.MAX_VALUE);
        GridPane.setVgrow(textArea, Priority.ALWAYS);
        GridPane.setHgrow(textArea, Priority.ALWAYS);

        GridPane expContent = new GridPane();
        expContent.setMaxWidth(Double.MAX_VALUE);
        expContent.add(label, 0, 0);
        expContent.add(textArea, 0, 1);

        alert.getDialogPane().setExpandableContent(expContent);

        alert.showAndWait();
    }

    public static void alertInfo(String text) {
        sync(() -> {
            Alert alert = new Alert(AlertType.INFORMATION);
            alert.setHeaderText(null);
            alert.setContentText(text);
            alert.showAndWait();
        });

    }

    public static void alertWarning(String text) {
        sync(() -> {
            Alert alert = new Alert(AlertType.WARNING);
            alert.setHeaderText(null);
            alert.setContentText(text);
            alert.showAndWait();
        });
    }

    public static void alertError(String text) {
        sync(() -> {
            Alert alert = new Alert(AlertType.ERROR);
            alert.setHeaderText(null);
            alert.setContentText(text);
            alert.showAndWait();
        });
    }
    /*
     public static ButtonType confirm(String question) {
     sync(() -> {

     Alert alert = new Alert(AlertType.CONFIRMATION);
     alert.setHeaderText(null);
     alert.setContentText(question);
     alert.show();
     return alert.getResult();
     });
     }

     public static ButtonType question(String question, ButtonType... buttons) {
     sync(() -> { Alert alert = new Alert(AlertType.CONFIRMATION);
     alert.setHeaderText(null);
     alert.setContentText(question);
     ObservableList<ButtonType> btns = alert.getButtonTypes();
     btns.clear();
     if (buttons != null && buttons.length > 0)

     btns.addAll(Arrays.asList(buttons));
     else
     btns.addAll(ButtonType.YES, ButtonType.NO, ButtonType.CANCEL);
     alert.show();
     return alert.getResult();
     });
     }
     */
}
