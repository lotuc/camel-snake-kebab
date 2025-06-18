(ns camel-snake-kebab.internals.string-separator-test
  (:require [camel-snake-kebab.internals.string-separator :refer [split generic-separator]]
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest testing is are]])))

(deftest split-test
  (testing "regex, string and character separators"
    (are [sep]
         (and (= ["foo" "bar"] (split sep "foo.bar"))
              (= [""]          (split sep "")))
      #"\." "." \.))

  (testing "input consisting of separator(s)"
    (is (empty? (split "x" "x")))
    (is (empty? (split "x" "xx"))))

  (testing "generic separator"
    (are [x y]
         (= x (split generic-separator y))

      [""]  ""
      [""]  "   "
      ["x"] " x "

      ["foo" "bar"] "foo bar"
      ["foo" "bar"] "foo\n\tbar"
      ["foo" "bar"] "foo-bar"
      ["foo" "Bar"] "fooBar"
      ["Foo" "Bar"] "FooBar"
      ["foo" "bar"] "foo_bar"
      ["FOO" "BAR"] "FOO_BAR"

      ["räksmörgås"]      "räksmörgås"
      ["Aräksmörgås" "B"] "AräksmörgåsB"
      ["ARÄKSMÖRGÅSB"]    "ARÄKSMÖRGÅSB"

      ["IP" "Address"] "IPAddress"

      ["Adler32"]         "Adler32"
      ["Inet4" "Address"] "Inet4Address"
      ["Arc2" "D"]        "Arc2D"
      ["a123b"]           "a123b"
      ["A123B"]           "A123B"
      ["a123" "B"]        "a123B")))
