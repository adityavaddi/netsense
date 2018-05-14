(ns datagen.core
  (:gen-class)
  (import org.supercsv.io.CsvListReader)
  (import org.supercsv.io.CsvMapReader)
  (import org.supercsv.cellprocessor.ift.CellProcessor)
  (import org.supercsv.io.CsvListWriter)
  (import org.supercsv.prefs.CsvPreference)
  (import org.supercsv.cellprocessor.constraint.NotNull)
  (import java.io.FileReader)
  (import java.io.BufferedReader)
  (import java.io.FileWriter)
  (import java.nio.channels.FileChannel)
  (import org.apache.commons.io.FileUtils)
  (:require [crypto.random :as random])
  (:require [clojure.data.json :as json])
  (:require [cheshire.core :refer :all])
  (:require [clojure.string :as strng])
  (:require [clojure.java.io :as io] )
  )


(defn convert-json
  "Converts JSON to Cypher properties"
  [json]
  (strng/replace json #"\"(.*?)\"\s*:\s*(\".*?\")" "$1: $2")
  )

(defn sitename-from-filename [filename]
  (strng/replace filename #"(.*?)-(.*)" "$1")
  )

(defn orgname-from-dirname [dirname]
  (str dirname)
  )


(defn create-site
  [sitename]
  )


(defn copy-seeddata-head
  ([cypherwriter]
    (with-open [r (io/reader "seeddata-head.cypher")]
      (doseq [line (line-seq r)]
        (.write cypherwriter (str line "\n"))))
    (.write cypherwriter "\n\n")
    )

  )

(defn write-site-head
  ([cypherwriter site-index siteid sitename org-index org-name]
    (.write cypherwriter (format "MATCH (o%s) WHERE o%s.name=\"%s\"\n" org-index org-index org-name))
    (.write cypherwriter (format "MERGE (s%s:Site:Active {siteid: \"%s\", name: \"%s\"})\n" site-index siteid sitename))
    (.write cypherwriter (format "CREATE UNIQUE (s%s)-[:BELONGS_TO]->(o%s)\n" site-index org-index ))
    (.write cypherwriter (format "CREATE UNIQUE (o%s)-[:HAS]->(s%s);\n\n" org-index site-index ))
    )
  )

(defn write-org-head
  ([cypherwriter org-index org-id org-name ]


    (.write cypherwriter (format "MATCH (ro_ad) WHERE ro_ad.rolename=\"end_user_admin\"\n"))
    (.write cypherwriter (format "MATCH (ro_op) WHERE ro_op.rolename=\"end_user_lighting_user\"\n"))
    (.write cypherwriter (format "MATCH (ss_ad) WHERE ss_ad.email=\"sensity_admin@sensity.com\"\n"))
    (.write cypherwriter (format "MATCH (ss_ro) WHERE ss_ro.email=\"sensity_read_only@sensity.com\"\n"))
    (.write cypherwriter (format "MATCH (ss_us) WHERE ss_us.email=\"sensity_user@sensity.com\"\n"))

    (.write cypherwriter (format "MERGE (o%s:Org {orgid: \"%s\", name: \"%s\"})\n" org-index org-id org-name))
    (.write cypherwriter (format "         ON CREATE SET o%s.created=timestamp()\n" org-index))
    (.write cypherwriter (format "MERGE (u_ad_%s:User {userid: \"admin-%s\", email: \"admin-%s@sensity.com\", name: \"%s Admin\"})\n" org-index org-index org-index org-name))
    (.write cypherwriter (format "         ON CREATE SET u_ad_%s.created=timestamp()\n" org-index))
    (.write cypherwriter (format "MERGE (u_op_%s:User {userid: \"operator-%s\", email: \"operator-%s@sensity.com\", name: \"%s Operator\"})\n" org-index org-index org-index org-name))
    (.write cypherwriter (format "         ON CREATE SET u_op_%s.created=timestamp()\n" org-index))
    (.write cypherwriter (format "CREATE UNIQUE (u_ad_%s)-[:IS]->(ro_ad)\n" org-index))
    (.write cypherwriter (format "CREATE UNIQUE (ro_ad)-[:HAS]->(u_ad_%s)\n" org-index))
    (.write cypherwriter (format "CREATE UNIQUE (u_op_%s)-[:IS]->(ro_op)\n" org-index))
    (.write cypherwriter (format "CREATE UNIQUE (ro_op)-[:HAS]->(u_op_%s)\n" org-index))
    (.write cypherwriter (format "CREATE UNIQUE (u_ad_%s)-[:IS_USER_OF]->(o%s)\n" org-index org-index))
    (.write cypherwriter (format "CREATE UNIQUE (o%s)-[:HAS_USER]->(u_ad_%s)\n" org-index org-index))
    (.write cypherwriter (format "CREATE UNIQUE (u_op_%s)-[:IS_USER_OF]->(o%s)\n" org-index org-index))
    (.write cypherwriter (format "CREATE UNIQUE (o%s)-[:HAS_USER]->(u_op_%s)\n" org-index org-index))

    (.write cypherwriter (format "CREATE UNIQUE (ss_ad)-[:IS_USER_OF]->(o%s)\n" org-index))
    (.write cypherwriter (format "CREATE UNIQUE (o%s)-[:HAS_USER]->(ss_ad)\n" org-index))
    (.write cypherwriter (format "CREATE UNIQUE (ss_ro)-[:IS_USER_OF]->(o%s)\n" org-index))
    (.write cypherwriter (format "CREATE UNIQUE (o%s)-[:HAS_USER]->(ss_ro)\n" org-index))
    (.write cypherwriter (format "CREATE UNIQUE (ss_us)-[:IS_USER_OF]->(o%s)\n" org-index))
    (.write cypherwriter (format "CREATE UNIQUE (o%s)-[:HAS_USER]->(ss_us);\n\n\n" org-index))
  )
)

(defn write-site-match
  ([cypherwriter sitename site-index]

    (.write cypherwriter (format "MATCH (s%s) WHERE s%s.name=\"%s\"\n" site-index site-index sitename))
  )
)


;; This program reads CSV file generated from geomidpoint.com, deleted first 2 columns and inserts
;; an ID as the first (0) column, a long hex string as the second (1) column, a random pick between
;; 1-4 as the third (2) column, and a random pick between 1-4 as the fourth (3) column
(defn read-csv
  ([cypherwriter]
    (def input-dir (io/file "input/"))
    ;;    (def org-dirs (file-seq input-dir))
    (def jsonwriter (FileWriter. (str "output/AllSites.json")))
    (def org-counter (atom 0))
    (doseq [org-dir (.listFiles input-dir)]
      (let [dirname (.getName org-dir)
            org-name (orgname-from-dirname dirname)
            ]
        (if (.startsWith dirname ".")
          ()
          (let [org-index (swap! org-counter inc)
                org-id (str (random/hex 4) "-" (random/hex 2) "-" (random/hex 2) "-" (random/hex 6))]
            (write-org-head cypherwriter org-index org-id org-name)
            (read-csv dirname org-name org-index jsonwriter cypherwriter)
          ))))
    (.close jsonwriter)
    )

  ([dirname org-name org-index jsonwriter cypherwriter]
    (def site-counter (atom 0))
    (doseq [file (.listFiles (io/file (str "input/" dirname)))]
      (let [filename (.getName file)
            sitename (sitename-from-filename filename)
            ]
        (if (.startsWith filename ".")
          ()
          (let [siteindex (str (swap! site-counter inc) "_" org-index )]
            (read-csv dirname filename sitename siteindex  org-name org-index jsonwriter cypherwriter)
          )
        )
      )
    )
  )


  ([dirname filename sitename site-index  org-name org-index jsonwriter cypherwriter]
    (let [csvoutput (str "output/" dirname "/" sitename ".csv")
          csvreader (new CsvListReader (new FileReader (str "input/" dirname "/" filename)) (CsvPreference/STANDARD_PREFERENCE))
          csvwriter (new CsvListWriter (new FileWriter csvoutput) (CsvPreference/STANDARD_PREFERENCE))
          site (str (random/hex 4) "-" (random/hex 2) "-" (random/hex 2) "-" (random/hex 6))]
      (.write csvwriter ["nodeid" "latitude" "longitude" "building" "level"])
      (def siteid site)
      (loop []
        (let [columns (.read csvreader)]
          (when-not (nil? columns)
            (.remove columns 0)
            (.remove columns 1)
            (.remove columns 2)
            (.remove columns 2)
            (.add columns 0 (str "N01" (int (* (rand 1) 1000000000))))
            (.add columns (rand-nth (range 1 4)))
            (.add columns (rand-nth (range 1 4)))
            (.write csvwriter columns)
            (recur))))
      (.close csvreader)
      (.close csvwriter)
      )

    (let [processors (into-array CellProcessor [(NotNull.) (NotNull.) (NotNull.) (NotNull.) (NotNull.)])
          csvoutput (str "output/" dirname "/" sitename ".csv")
          fpreader (CsvMapReader. (FileReader. csvoutput) (CsvPreference/STANDARD_PREFERENCE))
          headr (.getHeader fpreader true)
          length (loop [len 0]                                   ;; First pass to read the length - kind of shitty
                   (let [c (.read fpreader headr processors)]
                     (if-not (nil? c)
                       (recur (+ len 1))
                       len)))
          mreader (CsvMapReader. (FileReader. csvoutput) (CsvPreference/STANDARD_PREFERENCE))
          header (.getHeader mreader true)]
      (println "Length: " length)
      (write-site-head cypherwriter site-index siteid sitename org-index org-name)

      ;; Create JSON root with the site name
      (.write jsonwriter (str "{\"site\":\"" sitename "\",\n"))
      (.write jsonwriter (str "\"siteid\": \"" siteid "\",\n"))
      (.write jsonwriter (str "\"nodes\": [\n"))
      (loop [x 1]
        (let [cols (.read mreader header processors)
              node-index (str x "_" site-index)
              ]
          (when-not (nil? cols)
            (.remove cols 1)
            (let [node-props (generate-string (into {} cols) {:pretty true})
                  node-props-smile (generate-string (into {} cols))]
              (.write jsonwriter node-props)
              (if (= (mod x 250) 1)
                (write-site-match cypherwriter sitename site-index)
                )
              (.write cypherwriter (format "CREATE UNIQUE (n%s:Node:Active %s)-[:BELONGS_TO]->(s%s)\n" node-index (convert-json node-props) site-index))
              (.write cypherwriter (format "CREATE UNIQUE (s%s)-[:HAS]->(n%s)" site-index node-index)))
              (if (= (mod x 250) 0)
                (.write cypherwriter (format ";"))
              )
              (.write cypherwriter (format "\n\n"))
              (if-not (= x (- length 1))
                (.write jsonwriter ",\n"))
              (recur (+ x 1)))))
      (.write jsonwriter "]}")
      (.close fpreader)
      (.close mreader)

      )
    )
  )

(defn seed-data
  ([]
    (def writer (FileWriter. (str "output/AllSites.cypher")))

    (copy-seeddata-head writer)

    (read-csv writer)

    (.close writer)
    )
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (if (= 0 (count args))
    (seed-data)
    )
  )
