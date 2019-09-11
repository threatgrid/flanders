(ns flanders.type-script-test
  (:require [clojure.test :refer [deftest is]]
            [flanders.core :as f]
            [flanders.type-script :as f.ts]
            [flanders.types :as f.t]))

(deftest anything-type-test
  (is (= "any" (f.ts/type-script-type (f/anything))))

  (is (nil? (f.ts/type-script-declaration (f/anything))))

  (is (= "type T = any;"
         (f.ts/type-script-declaration (f/anything :name "T"))))

  (is (nil? (f.ts/type-script-type-name (f/anything))))

  (is (= "T"
         (f.ts/type-script-type-name (f/anything :name "T")))))

(deftest boolean-type-test
  (is (= "boolean" (f.ts/type-script-type (f/bool))))

  (is (nil? (f.ts/type-script-declaration (f/bool))))

  (is (= "type T = boolean;"
         (f.ts/type-script-declaration (f/bool :name "T"))))

  (is (nil? (f.ts/type-script-type-name (f/bool))))

  (is (= "T" (f.ts/type-script-type-name (f/bool :name "T")))))

(deftest integer-type-test
  (is (nil? (f.ts/type-script-declaration (f/int))))

  (is (= "type T = number;"
         (f.ts/type-script-declaration (f/int :name "T"))))

  (is (nil? (f.ts/type-script-type-name (f/int))))

  (is (= "T"
         (f.ts/type-script-type-name (f/int :name "T")))))

(deftest keyword-type-test
  (is (= "string"
         (f.ts/type-script-type (f/key :k))))

  (is (nil? (f.ts/type-script-declaration (f/key :k))))

  (is (= "type T = string;"
         (f.ts/type-script-declaration (f/key :k :name "T"))))

  (is (nil? (f.ts/type-script-type-name (f/key :k))))

  (is (= "T"
         (f.ts/type-script-type-name (f/key :k :name "T")))))

(deftest string-type-test
  (is (= "string"
         (f.ts/type-script-type (f/str))))

  (is (nil? (f.ts/type-script-type-name (f/str))))

  (is (= "T"
         (f.ts/type-script-type-name (f/str :name "T"))))

  (is (nil? (f.ts/type-script-declaration (f/str))))

  (is (= "type T = string;"
         (f.ts/type-script-declaration (f/str :name "T")))))

;; TODO: Replace `f.t/map->EitherType` with `f/either`.
(deftest either-type-test
  (is (= "number"
         (f.ts/type-script-type (f.t/map->EitherType {:choices [(f/int)]}))))

  (is (= "number | string"
         (f.ts/type-script-type (f.t/map->EitherType {:choices [(f/int) (f/str)]}))))

  (is (nil? (f.ts/type-script-type-name (f.t/map->EitherType {:choices [(f/int) (f/str)]}))))

  (is (= "T"
         (f.ts/type-script-type-name (f.t/map->EitherType {:choices [(f/int) (f/str)] :name "T"}))))

  (is (nil? (f.ts/type-script-declaration (f.t/map->EitherType {:choices [(f/int) (f/str)]}))))

  (is (= "type T = number | string;"
         (f.ts/type-script-declaration (f.t/map->EitherType {:choices [(f/int) (f/str)] :name "T"})))))

(deftest map-type-test
  (is (= "{\n  a: any\n}"
         (f.ts/type-script-type (f/map [(f/entry (f/key :a) (f/anything))]))))

  (is (nil? (f.ts/type-script-type-name (f/map [(f/entry (f/key :a) (f/anything))]))))

  (is (= "T"
         (f.ts/type-script-type-name (f/map [(f/entry (f/key :a) (f/anything))] :name "T"))))

  (is (= "interface T {}"
         (f.ts/type-script-declaration (f/map [] :name "T"))))

  (is (= "interface T {\n  a: any\n}"
         (f.ts/type-script-declaration (f/map [(f/entry (f/key :a) (f/anything))] :name "T")))))
