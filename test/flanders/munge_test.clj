(ns flanders.munge-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [flanders.core :as f]
   [flanders.munge :as m]))

(deftest test-munge
  (testing "changing a field from optional to required"
    (is (= (m/munge-ddl
            (f/map
             [(f/entry :foo f/any-str
                       :required? false)
              (f/entry :bar (f/map
                             [(f/entry :spam f/any-str
                                       :required? false)
                              (f/entry :eggs f/any-str
                                       :required? true)])
                       :required? false)])
            [[:foo :require]
             [:bar :require]
             [:bar :spam :require]])

           (f/map
            [(f/entry :foo f/any-str
                      :required? true)
             (f/entry :bar (f/map
                            [(f/entry :spam f/any-str
                                      :required? true)
                             (f/entry :eggs f/any-str
                                      :required? true)])
                      :required? true)])))

    (is (= (m/munge-ddl
            (f/entry :foo f/any-str
                     :required? false)
            [[:require]])

           (f/entry :foo f/any-str
                    :required? true))))

  (testing "adding entries to a map"
    (is (= (m/munge-ddl
            (f/map
             [(f/entry :foo f/any-str)
              (f/entry :bar (f/map
                             [(f/entry :spam f/any-str)]))])
            [[:bar (m/append-entries
                    [(f/entry :eggs f/any-str)])]])

           (f/map
            [(f/entry :foo f/any-str)
             (f/entry :bar (f/map
                            [(f/entry :spam f/any-str)
                             (f/entry :eggs f/any-str)]))])))


    (is (= (m/munge-ddl
            (f/map
             [(f/entry :foo f/any-str)])
            [[(m/append-entries [(f/entry :bar f/any-str)])]])

           (f/map
            [(f/entry :foo f/any-str)
             (f/entry :bar f/any-str)]))))

  (testing "deleting an entry from a map"
    (is (= (m/munge-ddl
            (f/map
             [(f/entry :foo f/any-str)
              (f/entry :bar f/any-str)])
            [[:bar :delete]])

           (f/map
            [(f/entry :foo f/any-str)])))))
