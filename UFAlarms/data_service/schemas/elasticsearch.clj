
(require '[clj-http.client :as client]
         '[clojure.data.json :as json]
         '[clojurewerkz.elastisch.native :as es]
         '[clojurewerkz.elastisch.native.index :as esi])

(defn get-cluster-name [hostname]
  (get (json/read-str (get (client/get (format "http://%s:9200/_nodes/cluster" hostname)) :body)) "cluster_name"))

(let [cluster-name (get-cluster-name "localhost")
      conn (es/connect  [["127.0.0.1" 9300]] {"cluster.name" cluster-name})]
  (esi/create conn "farallones_logevents"))

