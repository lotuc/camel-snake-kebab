(ns ^:no-doc camel-snake-kebab.internals.string-separator
  (:require [clojure.string :as string])
  #?(:clj (:import (java.util.regex Pattern))))

#?(:clj (set! *warn-on-reflection* true))

(defprotocol StringSeparator
  (split [this s] ": StringSeparator -> String -> NonEmptySeq[String]"))

#?(:clj
   (letfn [(split-by-pattern [^Pattern p, ^String s]
             (string/split s p))
           ;; These could be optimized e.g. by using StringUtils in Apache Commons:
           (split-by-string [^String p, ^String s]
             (split-by-pattern (-> p Pattern/quote Pattern/compile) s))
           (split-by-char [^Character p, ^String s]
             (split-by-string (String/valueOf p) s))]
     (extend Pattern   StringSeparator {:split split-by-pattern})
     (extend String    StringSeparator {:split split-by-string})
     (extend Character StringSeparator {:split split-by-char}))

   :cljs
   (extend-protocol StringSeparator
     ;; Notes:
     ;; * Characters are just strings in ClojureScript.
     ;; * Using js/RegExp generates a warning, but what's the right way?

     js/RegExp
     (split [this s] (string/split s this))

     string
     (split [this s] (string/split s this))))

(defn classify-char [c]
  (case c
    (\- \_ \space \tab \newline \o013 \formfeed \return) :whitespace
    (\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z) :lower
    (\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z) :upper
    (let [c  (str c)
          c0 (string/upper-case c)
          c1 (string/lower-case c)]
      (cond
        (= c0 c1) :other
        (= c c0)  :upper
        (= c c1)  :lower
        :else     :other))))

(defn generic-split [ss]
  (let [cs (mapv classify-char ss)
        ss-length #?(:clj (.length ^String ss)
                     :cljs (.-length ss))]
    (loop [result (transient []), start 0, current 0, a :other]
      (let [next (inc current)
            result+new (fn [end]
                         (if (> end start)
                           (conj! result (.substring ^String ss start end))
                           result))]
        (cond (>= current ss-length)
              (or (seq (persistent! (result+new current)))
                  ;; Return this instead of an empty seq:
                  [""])

              (= (nth cs current) :whitespace)
              (recur (result+new current) next next :other)

              :else
              (let [[b c] (subvec cs current)]
                (cond
                  (= a b)
                  (if (and (= b :upper) (= c :lower))
                    (recur (result+new current) current next b)
                    (recur result start next a))

                  ;; a<other>*     b c
                  (= a :other)
                  (recur result start next b)

                  ;; a<non-other>* b<other> c
                  (= b :other)
                  (recur result start next a)

                  (and (not= a :upper)  (= b :upper))
                  (recur (result+new current) current next b)

                  :else
                  (recur result start next b))))))))

(def generic-separator
  (reify StringSeparator
    (split [_ s] (generic-split s))))
