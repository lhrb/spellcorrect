(ns dev.lhrb.spellcorrect
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.core.reducers :as r]
   [clojure.math :as M])
  (:import
   (java.util Properties)
   (edu.stanford.nlp.pipeline StanfordCoreNLP CoreDocument)))

(defn count-word-seq
  ([] {})
  ([freqs word-seq]
    (assoc freqs word-seq (inc (get freqs word-seq 0)))))

(defn merge-counts
  ([] {})
  ([& m] (apply merge-with + m)))

(defn ngram-frequency [ngram-txt-token]
  (r/fold merge-counts count-word-seq ngram-txt-token))

(defn ngrams
  "create ngrams from given tokens"
  [n tokens]
  (partition n 1 tokens))

;; stanford nlp tokenize pipeline options
(def tokenizer-opts
  (let [props (doto (Properties.)
                (.setProperty "annotators" "tokenize")
                (.setProperty "tokenize.language" "German")
                (.setProperty "tokenize.options", "splitHyphenated=false,americanize=false"))]
    (StanfordCoreNLP. props)))

(defn tokenize
  ([text]
   (tokenize text tokenizer-opts))
  ([text tokenize-opts]
   (let [doc (CoreDocument. text)]
     (.annotate tokenize-opts doc)
     (mapv #(.word %) (.tokens doc)))))

(defn lang-model
  "creates a map with n frequencies of ngram"
  [n text-tokens]
  (->> (range 1 (inc n))
       (reduce (fn [acc i]
                 (assoc acc i (ngram-frequency (ngrams i text-tokens))))
               {})))

(comment
  ;; read text files used to train the language model
  ;;
  ;; Using all Harry Potter and Lord of the Rings books for the
  ;; language mode

  (def files
    (->> (file-seq (clojure.java.io/file "resources/"))
         (map str)
         (filter #(str/starts-with? % "resources/h"))))

  (def text-tokens
    (->> files
         (map slurp)
         (mapcat tokenize)))

  (def model (lang-model 3 text-tokens))

  (def known-words (into #{} (map #(apply str %) (keys (get model 1)))))
 ,)

(defn edits1
  "edit distance 1"
  [word]
  (let [alphabet "abcdefghijklmnopqrstuvwxyzäöüß"
        alphabet-captial "ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜ"
        n (count word)
        deletes     (for [i (range n)] (str (subs word 0 i) (subs word (inc i))))
        transposes  (for [i (range (dec n))]
                      (str (subs word 0 i) (nth word (inc i)) (nth word i) (subs word (+ 2 i))))
        replaces    (for [i (range n) c alphabet] (str (subs word 0 i) c (subs word (inc i))))
        replaces-c  (for [c alphabet-captial] (str c (subs word 1 n)))
        inserts     (for [i (range (inc n)) c alphabet] (str (subs word 0 i) c (subs word i)))]
    (distinct (concat deletes transposes replaces replaces-c inserts))))

(defn edits2
  "edit distance 2"
  [word]
  (into [] (mapcat edits1) (edits1 word)))

(defn known-edits1
  "all known words with edit distance 1 from the given word"
  [word nwords]
  (into [] (filter nwords) (edits1 word)))

(defn known-edits2
  "all known words with edit distance 2 from the given word"
  [word nwords]
  {:pre [(set? nwords)]}
  (into [] (comp (mapcat edits1) (filter nwords) (distinct)) (edits1 word)))

(defn known-edits
  [word nwords]
  (if (= 1 (count word))
    (known-edits1 word nwords)
    (known-edits2 word nwords)))

(defn P
  "probability of the word with the given model."
  [model n word]
  (/ (get-in model [n word])
     (apply + (vals (get model n)))))

(defn P-candiate
  "calculates the probability of the candidate. Uses log space
  to prevent underflow."
  [model word]
  (let [n (count (str/split word #" "))]
   (apply +
          (->> (range 1 (inc n))
               (map #(M/log (P model % word)))))))

(defn candidate-sentence
  "simplified model assuming every sentence has only one wrong word in it."
  [known-words n sentence]
  (let [i (nth sentence n)
        f (take n sentence)
        l (drop (inc n) sentence)]
    (for [candidates (known-edits i known-words)]
      (flatten [f candidates l]))))

(defn gen-candidates
  [known-words sentence]
  (mapcat #(candidate-sentence known-words % sentence)
          (range 0 (count sentence))))

(defn split-sentence
  [sentence]
  (concat
   [(take 1 sentence)
    (take 2 sentence)]
   (partition 3 1 sentence)))

(def m-sample-space
  ;; Cache the result this will save us minutes.
  ;; Language model does not change an n is {1,2,3}.
  (memoize (fn [model n]
             (apply + (vals (get model n))))))

(defn P2
  [model s]
  (let [n (count s)]
   (/ (get-in model [n s] 0)
      (m-sample-space model n))))

(defn P-sentence
  [model sentence]
  (apply +
         (->> (split-sentence sentence)
              (map #(P2 model %))
              (map #(if (= 0 %) 0 (M/log %))))))

(defn correct [model known-words sentence]
  (->> (gen-candidates known-words sentence)
       (pmap (fn [s] [(P-sentence model s) s]))
       (sort-by first)))


(comment

  (time (P-sentence model ["Das" "ist" "ein" "Tect" "."]))

  (correct model known-words ["An" "den" "Stellen" ","
                              "die" "Wand" "eingestürzt" "war" ","
                              "gab" "es" "ebenfalls" "anzeichen"
                              "für" "Reperaturarbeiten" "."])

  (time (gen-candidates known-words ["Das" "ist" "ein" "Tect" "."]))
  (M/log (P2 model (last (split-sentence ["Das" "ist" "ein" "Tect" "."]))))


  (def sp (split-sentence ["Das" "ist" "ein" "Test" "."]))
  (time (P2 model '("Das" "ist")))

 (count [1 2 3])

 (def s '("Das" "ist" "ein"))

 (def sentence ["Das" "ist" "ein" "Tect" "."])

 (candidate-sentence known-words 4 sentence)

 (mapcat #(candidate-sentence known-words % sentence) (range 0 (count sentence)))

 (map (fn [c] [c (P-candiate model c)]) (known-edits2 "nähmlich" known-words))
 ,)
