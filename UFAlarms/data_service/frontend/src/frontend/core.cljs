(ns frontend.core
  (:require [cljs.nodejs :as node]
            [clojure.set :as set]
            [frontend.datasvc :as datasvc]
            [frontend.common :as common]
            [clojure.string :as string]
            [frontend.schema :as schema]))

(enable-console-print!)

(.install (node/require "source-map-support"))
(def gql (node/require "graphql"))
(def express (node/require "express"))
(def body-parser (node/require "body-parser"))
(def passport (node/require "passport"))
(def sha1 (node/require "sha1"))
(def Strategy (.-Strategy (node/require "passport-http-bearer")))

(def api-secret "d30e48f9990c48d9a89eda3c7b1446e2")
(def user-passes [["admin" "lettherebelight"]])
(def user-tokens (into {} (map (fn [pair] [(sha1 (string/join "" (concat [api-secret] pair))) (first pair)]) user-passes)))

(.use passport (Strategy. (fn [token callback]
   (callback nil (or (get user-tokens token) false)))))

(def app (express))

(def schema ^:private (atom ["Not loaded yet." nil]))
(defn load-schema []
  (let [[schema-str schemas] (schema/get-schemas [(datasvc/GraphQLConsumer) datasvc/DataServiceConsumer])
        loaded (first schemas)]
    (swap! schema (fn [_] [schema-str loaded]))))

(defn authenticate [] (.authenticate passport "bearer" (clj->js {:session false})))

(defn start-server []
  (load-schema)

  (.use app ((node/require "morgan") "combined"))
  (.use app (.text body-parser (js-obj "type" "application/graphql")))
  (.use app (fn [req, res, nxt]
     (.header res "Access-Control-Allow-Origin", "*")
     (.header res "Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept")
     (nxt)))

  (.post app "/graphql" (authenticate) (fn [req res]
    (let [body (.-body req)
          parsed (try (.parse js/JSON body) (catch :default e {}))
          [query params] [(aget parsed "query") (aget parsed "params")]]
    (.then ((.-graphql gql) (second @schema) (or query body) nil params) (fn [result]
      (if (.-errors result) (this-as this (aset this "status" 400)))
      (.send res (.stringify js/JSON result nil 2)))))))

  (.post app "/reload-schema" (authenticate) (fn [req res]
    (do (load-schema) (.send res "succeeded\n"))))

  (.get app "/schema" (authenticate) (fn [req res]
    (.set res (clj->js {"Content-Type" "text/plain"}))
    (.send res (first @schema))))

  (def PORT 3000)
  (def server (.listen app PORT (fn []
      (console/log "GraphQL listening on port:" PORT)))))

(defn -main [& args]
  (swap! common/DEBUG (fn [] (contains? (set args) "--debug")))
  (start-server))

(set! *main-cli-fn* -main)
