(ns ^:no-doc camel-snake-kebab.internals.macros
  #?(:cljs (:refer-clojure :exclude [resolve]))
  (:require [camel-snake-kebab.internals.alter-name :refer [alter-name]]
            [camel-snake-kebab.internals.misc :refer [convert-case]]))

#?(:cljs
   (defn resolve [sym]
     ;; On self-hosted ClojureScript, macros are evaluated under the `:cljs` conditional branch
     ;; In that case, we need to use `eval` in order to resolve variables instead of `resolve`
     (eval `(~'var ~sym))))

(defn type-preserving-function [case-label first-fn rest-fn sep]
  `(defn ~(symbol (str "->" case-label)) [s# & rest#]
     (if (satisfies? camel-snake-kebab.internals.alter-name/AlterName s#)
       (let [convert-case# #(apply convert-case ~first-fn ~rest-fn ~sep % rest#)]
         (alter-name s# convert-case#))
       s#)))

(defn type-converting-functions [case-label first-fn rest-fn sep &env]
  (letfn [(make-name [type-label]
            (->> (str case-label " " type-label)
                 (convert-case (resolve first-fn) (resolve rest-fn) sep)
                 (str "->")
                 (symbol)))]
    (for [[type-label type-converter] {"string" `identity "symbol" `symbol "keyword" `keyword}]
      (if (some? (:ns &env))
        `(defn ~(make-name type-label) [s# & rest#]
           (if (or (string? s#) (satisfies? cljs.core/INamed s#))
             (~type-converter (apply convert-case ~first-fn ~rest-fn ~sep (name s#) rest#))
             s#))
        `(defn ~(make-name type-label) [s# & rest#]
           (if (or (string? s#) (instance? clojure.lang.Named s#))
             (~type-converter (apply convert-case ~first-fn ~rest-fn ~sep (name s#) rest#))
             s#))))))

(defmacro defconversion [case-label first-fn rest-fn sep]
  `(do  ~(type-preserving-function  case-label first-fn rest-fn sep)
      ~@(type-converting-functions case-label first-fn rest-fn sep &env)))
