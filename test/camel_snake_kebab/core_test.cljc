(ns camel-snake-kebab.core-test
  (:require [camel-snake-kebab.core :as csk]
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest testing is are]]))
  #?(:clj (:import (clojure.lang ExceptionInfo))))

(def zip (partial map vector))

(def all-functions
  [csk/->PascalCase
   csk/->Camel_Snake_Case
   csk/->camelCase
   csk/->SCREAMING_SNAKE_CASE
   csk/->snake_case
   csk/->kebab-case
   csk/->HTTP-Header-Case

   csk/->PascalCaseKeyword
   csk/->camelCaseKeyword
   csk/->SCREAMING_SNAKE_CASE_KEYWORD
   csk/->snake_case_keyword
   csk/->kebab-case-keyword
   csk/->Camel_Snake_Case_Keyword
   csk/->HTTP-Header-Case-Keyword

   csk/->PascalCaseString
   csk/->camelCaseString
   csk/->SCREAMING_SNAKE_CASE_STRING
   csk/->snake_case_string
   csk/->kebab-case-string
   csk/->Camel_Snake_Case_String
   csk/->HTTP-Header-Case-String

   csk/->PascalCaseSymbol
   csk/->camelCaseSymbol
   csk/->SCREAMING_SNAKE_CASE_SYMBOL
   csk/->snake_case_symbol
   csk/->kebab-case-symbol
   csk/->Camel_Snake_Case_Symbol
   csk/->HTTP-Header-Case-Symbol])

(deftest format-case-test
  (testing "examples"
    (are [x y] (= x y)
      'fluxCapacitor  (csk/->camelCase 'flux-capacitor)
      "I_AM_CONSTANT" (csk/->SCREAMING_SNAKE_CASE "I am constant")
      :object-id      (csk/->kebab-case :object_id)
      "X-SSL-Cipher"  (csk/->HTTP-Header-Case "x-ssl-cipher")
      :object-id      (csk/->kebab-case-keyword "object_id"))
      :s3_key         (csk/->snake_case :s3-key :separator \-))

  (testing "return the input when non-transformable"
    (doseq [f all-functions]
      (is (= 42 (f 42)))
      (let [uuid #uuid "66544593-46d7-4c34-8c8e-cf9dada291cb"]
        (is (= uuid (f uuid))))))

  (testing "rejection of namespaced keywords and symbols"
    (is (thrown? ExceptionInfo (csk/->PascalCase (keyword "a" "b"))))
    (is (thrown? ExceptionInfo (csk/->PascalCase (symbol  "a" "b")))))

  (testing "all the type preserving functions"
    (let
     [inputs-lst [["FooBar"
                   "fooBar"
                   "FOO_BAR"
                   "foo_bar"
                   "foo-bar"
                   "Foo_Bar"]
                  ["FööBär"
                   "fööBär"
                   "FÖÖ_BÄR"
                   "föö_bär"
                   "föö-bär"
                   "Föö_Bär"]]
      functions [csk/->PascalCase
                 csk/->camelCase
                 csk/->SCREAMING_SNAKE_CASE
                 csk/->snake_case
                 csk/->kebab-case
                 csk/->Camel_Snake_Case]
      formats   [identity keyword symbol]]

      (doseq [inputs inputs-lst, input inputs, format formats, [output function] (zip inputs functions)]
        (is (= (format output) (function (format input)))))))

  (testing "all the type preserving functions - group chars which have no upper/lower cases"
    (let
     [inputs-lst [["Convert2snake"
                   "convert2snake"
                   "CONVERT2SNAKE"
                   "convert2snake"
                   "convert2snake"
                   "Convert2snake"]
                  ["FileMd5"
                   "fileMd5"
                   "FILE_MD5"
                   "file_md5"
                   "file-md5"
                   "File_Md5"]]
      functions [csk/->PascalCase
                 csk/->camelCase
                 csk/->SCREAMING_SNAKE_CASE
                 csk/->snake_case
                 csk/->kebab-case
                 csk/->Camel_Snake_Case]
      formats   [identity keyword symbol]]
      (doseq [inputs inputs-lst, input inputs, format formats, [output function] (zip inputs functions)]
        (is (= (format output) (function (format input)))))))

  (testing "some of the type converting functions"
    (are [x y] (= x y)
      :FooBar   (csk/->PascalCaseKeyword 'foo-bar)
      "FOO_BAR" (csk/->SCREAMING_SNAKE_CASE_STRING :foo-bar)
      'foo-bar  (csk/->kebab-case-symbol "foo bar")))

  (testing "handling of blank input string"
    (is (= "" (csk/->kebab-case "")))
    (is (= "" (csk/->kebab-case " "))))

  (testing "handling of input consisting of only separator(s)"
    (is (= "" (csk/->kebab-case "a" :separator \a)))
    (is (= "" (csk/->kebab-case "aa" :separator \a)))))

(deftest http-header-case-test
  (are [x y] (= x (csk/->HTTP-Header-Case y))
    "User-Agent"       "user-agent"
    "DNT"              "dnt"
    "Remote-IP"        "remote-ip"
    "TE"               "te"
    "UA-CPU"           "ua-cpu"
    "X-SSL-Cipher"     "x-ssl-cipher"
    "X-WAP-Profile"    "x-wap-profile"
    "X-XSS-Protection" "x-xss-protection"))
