package com.context;

import com.utils.Path;
import com.cron.*;
import com.io.*;
import com.io.zip.ZipDirectory;
import com.lang.LContext;
import com.mlogger.Log;
import com.utils.date.TDate;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import com.utils.hashes.Hashes;
import java.io.*;
import java.nio.charset.Charset;
import java.util.concurrent.atomic.AtomicInteger;

public class Maintenance {

    public static class Backuper extends CronRunnable<Void> {

        private String prevHash;

        public Backuper() {
            super("service.backuper", LContext.MAINTAIN_BACKUP_DIR.toString());
            task.enabled = false;
            //  task.addTimeSchedule("Nocny backup", new Interval(2, 0, 0));
        }

        public Backuper(TaskRunnableInstance runnable) {
            super(runnable);
        }

        @Override
        public Void runTask() throws Exception {

            File file = AppContext.varPath.getFile("backup",
                    new TDate().toString("yyyy MM dd HH mm ss") + ".zip");
            file.getParentFile().mkdirs();

            HashOutputStream hout = new HashOutputStream(Hashes.Hash.MD5,
                    new BufferedOutputStream(
                            new FileOutputStream(file)));

            final AtomicInteger cnt = new AtomicInteger();

            try {

                try (ZipDirectory zip = new ZipDirectory(hout, Charset.forName("UTF-8")) {

                    @Override
                    public ZipDirectory addFile(Path path, String name,
                            boolean uncompressed, String comment, byte[] extra)
                            throws IOException {

                        if (path.endsWith(".json"))
                            super.addFile(path, name, uncompressed, comment, extra);

                        cnt.incrementAndGet();
                        return this;
                    }

                }) {
                    zip.addDir(AppContext.etcPath.toFile(), true);
                    zip.finish();
                };

                String hash = hout.getAsString();

                if (!hash.equals(prevHash))
                    file = null;

                prevHash = hash;
            } finally {
                hout.close();

                if (file != null)
                    file.delete();
            }

            if (file != null)
                Log.info("Dodano " + cnt.get() + " plików do arciwum "
                        + file.getCanonicalPath());
            return null;
        }

    }

}
