(ns jolly.core
  "Fns for converting Grimoire data stores to Grenada Things."
  (:require
    [grey.core :as grey]
    [plumbing.core :refer [fnk safe-get safe-get-in ?> ?>>] :as plumbing]
    poomoo.bars
    [grenada
     [aspects :as a]
     [bars :as b]
     [things :as t]
     [transformers :as gr-trans]
     [utils :as g-utils]]
    [grenada.things.utils :as t-utils]
    jolly.bars
    [grimoire
     [api :as grim]
     [either :as either]
     [things :as grim-t]]
    grimoire.api.fs.read
    [guten-tag.core :as gt]))

;; A bit repetitive, but at least it allows me to easily check whether an
;; I've covered all the cases and whether there is anything foul in the state of
;; the input data.
(def grim-tag->gren-tag
  "Maps Grimoire Thing tags to Grenada Main Aspect names."
  {::grim-t/group     ::t/group
   ::grim-t/artifact  ::t/artifact
   ::grim-t/version   ::t/version
   ::grim-t/platform  ::t/platform
   ::grim-t/namespace ::t/namespace
   ::grim-t/def       ::t/find})

(defn get-all-coords
  "Given a Grimoire Thing, returns its coordinates.

  This isn't as easy as in Grenada, because we have to traverse the Grimoire
  Thing graph."
  [grim-thing]
  (let [gren-tag (safe-get grim-tag->gren-tag (gt/tag grim-thing))
        ncoords (safe-get-in t/def-for-aspect [gren-tag :ncoords])]
    (-> (for [depth (reverse (range ncoords))
              :let [ks (-> (repeat depth :parent)
                           vec
                           (conj :name))]]
          (safe-get-in grim-thing ks))
        vec)))

(defn determine-aspects
  "Given a map of metadata as read from a Grimoire data store, returns the
  appropriate Grenada Aspects for them."
  [grim-meta]
  (let [t (safe-get grim-meta :type)]
   (case t
    :fn      [::a/var-backed ::a/fn]
    :macro   [::a/var-backed ::a/macro]
    :var     [::a/var-backed]
    :special (-> [::a/special]
                 (?> (get grim-meta :special-form) (conj ::a/var-backed))
                 (?> (get grim-meta :macro)        (conj ::a/macro)))

    (throw (IllegalArgumentException.
             (str "Unknown type of def: " t))))))

(defmulti grim-thing->gren-thing
  "Converts Grimoire metadata and Thing to a Grenada Thing."
  (fn [_ tval] (gt/tag tval)))

(defmethod grim-thing->gren-thing ::grim-t/def [grim-meta d]
  (->> (t/map->thing {:coords (get-all-coords d)})
       (t/attach-aspect t/def-for-aspect ::t/find)
       (t/attach-aspects a/def-for-aspect (determine-aspects grim-meta))))

(defmethod grim-thing->gren-thing :default [_ t]
  (if-let [gren-tag (get grim-tag->gren-tag (gt/tag t))]
    (->> (t/map->thing {:coords (get-all-coords t)})
         (t/attach-aspect t/def-for-aspect gren-tag))
    (throw (IllegalArgumentException.
             (str "Unknown Grimoire type of Grimoire thing: " (pr-str t))))))

(def read-all-things
  "Reads all Things it can find in the Grimoire data store denoted by
  LIB-GRIM-CONFIG."
  (memoize
    (fn read-all-things-fn [lib-grim-config]
      (->> (for [[i k] (plumbing/indexed [:group
                                          :artifact
                                          :version
                                          :platform
                                          :ns
                                          :def])]
             (grim/search lib-grim-config
                          (into [k] (repeat (inc i) :any))))
           (map either/result)
           plumbing/aconcat))))

;; REVIEW: New versions of lib-grimoire might allow examples on non-def Things.
;;         (RM 2015-07-25)
;;
;; Notes:
;;
;;  - The doc string of grimoire.api/list-examples is a bit strange. It sounds
;;    like list-examples would return Fail if there are no examples for a
;;    given Thing. This would be a behaviour I wouldn't approve of. – Returning
;;    a Success with an empty list would make more sense to me. Luckily this is
;;    indeed what happens.
;;
;;  - Don't be fooled by the doc string of grimoire.api/list-examples.
;;
(defn read-examples
  "Reads and returns the examples for Grimoire THING from the datastore denoted
  by CONFIG.

  Returns an empty collection if no examples are found.

  CONFIG is a :grimoire.api.fs/Config Tmap."
  [config thing]
  (if (grim-t/def? thing)
    (map #(assoc % :contents (either/result (grim/read-example config %)))
         (either/result (grim/list-examples config thing)))
    []))

;; Note:
;;
;;  - This looks kind of similar to read-examples, but it doesn't have the
;;    grim-t/def? clause, because all Things support notes.
;;
;;  - Unfortunately list-notes doesn't do the thing that's more sensible than
;;    the doc string as list-examples does. (See note above.) It Fails if there
;;    are no notes on a group. So we need a different clause.
;;
(defn read-notes
  "Reads and returns the notes for Grimoire THING from the datastore denoted by
  CONFIG.

  CONFIG is a :grimoire.api.fs/Config Tmap."
  [config thing]
  (let [notes (grim/list-notes config thing)]
    (if (or (grim-t/versioned? thing) (either/succeed? notes))
      (map #(assoc % :contents (either/result (grim/read-note config %)))
           (either/result notes))
      [])))

(defn maybe-attach
  "Attaches Grimoire examples or notes (GRIM-STHS) to GREN-THING as a Bar of the
  type with name BAR-TYPE-TAG.

  If GRIM-STHS is empty, attaches nothing.

  sths … somethings"
  [bar-type-tag grim-sths gren-thing]
  {:pre [(t/thing?+ gren-thing)]}
  (if-not (empty? grim-sths)
    (t/attach-bar jolly.bars/def-for-bar-type
                  bar-type-tag
                  (map #(t-utils/safe-select-keys % #{:name :contents})
                       grim-sths)
                  gren-thing)
    gren-thing))

(grey/with-handler! #'gr-trans/specify-cmeta-any
  gr-trans/missing-bar-type-defs? :attach-anyway)

(defn grim-t->gren-t-with-bars
  "Returns the corresponding Grenada thing for the Grimoire Thing GRIM-T,
  reading data from the data store configured in LIB-GRIM-CONFIG.

  Cmetadata Bars from the Cmetadata of the Grimoire Thing are attached as
  Grenada Bars. Their validity is checked if the definitions of their types are
  contained in grenada.bars or poomoo.bars. Bars of unknown type are
  attached, but not checked. This will be improved in a visible future.

  If the metadata for GRIM-T tell that it is a sentinel, return nil. Sentinels
  are Grimoire Things that refer to other Things, because they're not really
  Things, but syntax, like the 'catch' in a (try …) form. I don't want these to
  be Grenada Things."
  [lib-grim-config grim-t]
  (let [grim-meta (either/result (grim/read-meta lib-grim-config grim-t))]
    (if-not (and (grim-t/def? grim-t) (= :sentinel (safe-get grim-meta :type)))
      (->> grim-t
           (grim-thing->gren-thing grim-meta)
           (?>> (seq grim-meta)
                (t/attach-bar b/def-for-bar-type ::b/any grim-meta))
           (gr-trans/specify-cmeta-any poomoo.bars/def-for-bar-type)
           (maybe-attach :jolly.bars/examples
                         (read-examples lib-grim-config grim-t))
           (maybe-attach :jolly.bars/notes
                         (read-notes lib-grim-config grim-t))))))

(defn grim-ts->gren-ts-with-bars
  "Converts all Grimoire Things in GRIM-TS into Grenada Things, skipping the
  ones for which grim-t->gren-ts-with-bars returns nil."
  [lib-grim-config grim-ts]
  (->> grim-ts
       (map #(grim-t->gren-t-with-bars lib-grim-config %))
       (remove nil?)))
