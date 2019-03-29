(ns reader.expectorate
  (:import (java.io FileOutputStream OutputStreamWriter BufferedWriter)))

(defn expectorate
  "spit like, but only with files"
  [dst content]
  (with-open [writer (make-writer dst)]
    (.write writer (str content))))

(defn make-writer
  [dst]
  (-> (condp = (type dst)
        java.io.OutputStream dst
        java.io.File (FileOutputStream. dst)
        java.lang.String (FileOutputStream. dst)
        java.net.Socket (.getOutputStream dst)
        java.net.URL (if (= "file" (.getProtocol dst))
                       (-> dst .getPath FileOutputStream.)
                       (throw (IllegalArgumentException.
                                "Can't write to non-file URL"))))
      OutputStreamWriter.
      BufferedWriter.))

