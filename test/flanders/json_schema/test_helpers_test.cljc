(ns flanders.json-schema.test-helpers-test
  (:require [clojure.test :refer [deftest is testing]]
            [flanders.json-schema.test-helpers :as sut]
            [schema.core :as s]))

(deftest unqualify-vars-test
  (is (= {} (sut/unqualify-vars #{})))
  (is (= {`- 'ns-0/-} (sut/unqualify-vars #{#'-})))
  (is (= {`+ 'ns-0/+
          `- 'ns-0/-
          `sut/unqualify-vars 'ns-1/unqualify-vars}
         (sut/unqualify-vars #{#'- #'+ #'sut/unqualify-vars})))
  (is (= (into #{} (map #(symbol "ns-0" (-> % symbol name))) (vals (ns-publics 'clojure.core)))
         (set (vals (sut/unqualify-vars (vals (ns-publics 'clojure.core)))))))
  (testing "pads namespaces"
    ;; we test with real vars for small numbers, since namespaces must be distinct
    (doseq [[n expected] {1 "ns-0"
                          9 "ns-0"
                          10 "ns-0"
                          11 "ns-00"
                          23 "ns-00"}]
      (testing n
        (is (= expected (first (sort (map namespace (vals (sut/unqualify-vars (sut/vars-from-distinct-namespaces n))))))))))
    (doseq [[n expected] {100 "ns-00"
                          101 "ns-000"
                          999 "ns-000"
                          1000 "ns-000"
                          1001 "ns-0000"}]
      (testing n
        (is (= expected (first (sort (map namespace (vals (sut/unqualify-vars (sut/syms-from-distinct-namespaces n))))))))))))


(declare BSchema)
(s/defschema ASchema [(s/recursive #'BSchema)])
(s/defschema BSchema [(s/recursive #'ASchema)])

(deftest collect-recursive-vars-from-schema-test
  (is (= #{} (sut/collect-recursive-vars-from-schema [s/Any])))
  (is (= #{#'s/Any} (sut/collect-recursive-vars-from-schema [(s/recursive #'s/Any)])))
  (is (= #{#'ASchema} (sut/collect-recursive-vars-from-schema BSchema)))
  (is (= #{#'BSchema} (sut/collect-recursive-vars-from-schema ASchema))))

(deftest collect-transitive-recursive-vars-from-schema-test
  (is (= #{} (sut/collect-transitive-recursive-vars-from-schema [s/Any])))
  (is (= #{#'s/Any} (sut/collect-transitive-recursive-vars-from-schema [(s/recursive #'s/Any)])))
  (is (= #{#'ASchema #'BSchema}
         (sut/collect-transitive-recursive-vars-from-schema ASchema)
         (sut/collect-transitive-recursive-vars-from-schema BSchema))))

(s/defschema b2 (s/enum 42))
(s/defschema a2 (s/recursive #'b2))
(s/defschema b1 (s/recursive #'a2))
(s/defschema a1 (s/recursive #'b1))

(deftest explain-transitive-schema-test
  (is (= '{:schema (recursive #'ns-0/b1),
           :vars
           {ns-0/a2 (recursive #'ns-0/b2),
            ns-0/b1 (recursive #'ns-0/a2),
            ns-0/b2 (enum 42)}}
         (sut/explain-transitive-schema a1))))
