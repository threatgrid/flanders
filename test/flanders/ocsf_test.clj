(ns flanders.ocsf-test
  (:require [cheshire.core :as json]
            [clj-http.client :as client]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [flanders.malli :as malli]
            [flanders.ocsf :as ocsf]
            [flanders.schema :as schema]
            [malli.core :as m]
            [schema.core :as s]))

(defn sort-recursive [v]
  (walk/postwalk
    (fn [v]
      (cond->> v
        (map? v) (into (sorted-map))))
    v))

(def nsamples 10)

(defn gen-ocsf-1-3-0 []
  ;"https://schema.ocsf.io/api/1.3.0/classes/http_activity"
  (let [export-schema (-> "https://schema.ocsf.io/export/schema" client/get :body json/decode)
        _ (assert (map? export-schema))
        sample {"objects" (into {} (map (fn [name]
                                          (when (Thread/interrupted) (throw (InterruptedException.)))
                                          [name (doall
                                                  (pmap (fn [_]
                                                          (when (Thread/interrupted) (throw (InterruptedException.)))
                                                          (let [url (str "https://schema.ocsf.io/sample/objects/" name)]
                                                            (prn url)
                                                            (try (-> url client/get :body json/decode)
                                                                 (catch Exception e
                                                                   (prn url)
                                                                   (throw e)))))
                                                        (range nsamples)))]))
                                (keys (get export-schema "objects")))
                "base_event" (doall
                               (pmap (fn [_]
                                       (when (Thread/interrupted) (throw (InterruptedException.)))
                                       (let [url "https://schema.ocsf.io/sample/base_event"]
                                         (prn url)
                                         (try (-> url client/get :body json/decode)
                                              (catch Exception e
                                                (prn url)
                                                (throw e)))))
                                     (range nsamples)))
                "classes" (into {} (map (fn [name]
                                          (when (Thread/interrupted) (throw (InterruptedException.)))
                                          [name (doall (pmap (fn [_]
                                                               (when (Thread/interrupted) (throw (InterruptedException.)))
                                                               (let [url (str "https://schema.ocsf.io/sample/classes/" name)]
                                                                 (prn url)
                                                                 (try (-> url client/get :body json/decode)
                                                                      (catch Exception e
                                                                        (prn url)
                                                                        (throw e)))))
                                                             (range nsamples)))]))
                                (keys (get export-schema "classes")))}]
    (try (spit "resources/flanders/impl/ocsf-1.3.0-export.json"
               (-> export-schema
                   sort-recursive
                   (json/encode {:pretty true})))
         (finally
           (spit "test-resources/flanders/ocsf-1.3.0-sample.json"
                 (-> sample
                     sort-recursive
                     (json/encode {:pretty true})))))))

(comment
  ;;regenerate
  (gen-ocsf-1-3-0)
  )

(def ocsf-1-3-0-export (delay (json/decode (slurp (io/resource "flanders/impl/ocsf-1.3.0-export.json")))))
(def ocsf-1-3-0-sample (delay (json/decode (slurp (io/resource "flanders/ocsf-1.3.0-sample.json")))))

(deftest ocsf-1-3-0-export-test
  (is (= "1.3.0" (get @ocsf-1-3-0-export "version")))
  (doseq [[k nexpected] {"objects" 121 "classes" 72}]
    (let [objects (get @ocsf-1-3-0-export k)
          examples (get @ocsf-1-3-0-sample k)]
      (is (= nexpected (count objects)))
      (doseq [[name obj] objects]
        (testing name
          (let [f (ocsf/oscf-schema->flanders obj)
                m (malli/->malli f)
                s (schema/->schema f)
                good-examples (map walk/keywordize-keys (get examples name))]
            (is (= nsamples (count good-examples)))
            (doseq [good-example good-examples
                    :let [bad-example (assoc good-example ::junk "foo")]]
              (is (nil? (m/explain m good-example)))
              (is (nil? (s/check s good-example)))
              (is (m/explain m bad-example))
              (is (s/check s bad-example))))))))
  (let [types (get @ocsf-1-3-0-export "types")]
    (is (= 22 (count types)))
    (doseq [[name obj] types]
      (testing name
        (is (malli/->malli (ocsf/oscf-schema->flanders obj)))
        (is (schema/->schema (ocsf/oscf-schema->flanders obj))))))
  (testing "base_event"
    (let [base-event (get @ocsf-1-3-0-export "base_event")
          examples (get @ocsf-1-3-0-sample "base_event")
          _m (is (malli/->malli (ocsf/oscf-schema->flanders base-event)))
          _s (is (schema/->schema (ocsf/oscf-schema->flanders base-event)))
          good-examples (map walk/keywordize-keys examples)]
      (is (= nsamples (count good-examples)))
      #_ ;;TODO examples for base event seem to be an open map?
      (doseq [good-example good-examples
              :let [bad-example (assoc good-example ::junk "foo")]]
        (is (nil? (m/explain m good-example)))
        (is (nil? (s/check s good-example)))
        (is (m/explain m bad-example))
        (is (s/check s bad-example))))))


