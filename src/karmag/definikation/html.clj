(ns karmag.definikation.html
  (:require [clojure.java.io :as io]
            [clojure.pprint]
            [clojure.string :as cs]
            [clojure.walk :refer [postwalk]]
            [karmag.definikation.core :as df]
            [karmag.definikation.common :as common]
            [karmag.definikation.spec :as spec]
            [hiccup.core :as html]))

(defn- esc [obj] (html/h (str obj)))

(defn- id-key [id]
  (cs/replace (str (:type id) "__" (:id id)) #"[^a-zA-Z0-9_/]" "_"))

(defn- make-anchor [id] [:a {:id (id-key id)}])
(defn- make-link [id contents] [:a {:href (str "#" (id-key id))} contents])

(defn- make-id-table [ids]
  [:table
   [:tr [:th "type"] [:th "id"] [:th ""]]
   (->> (set ids)
        (sort-by (comp str :id))
        (sort-by (comp str :type))
        (map (fn [id]
               [:tr
                [:td (esc (:type id))]
                [:td (esc (:id id))]
                [:td (make-link id "GO")]])))])

(defn- render-item [item]
  (let [id->text (->> (set (common/find-ids item))
                      (mapcat #(vector % (symbol (id-key %))))
                      (apply hash-map))
        replaced-item (postwalk (fn [obj]
                                  (get id->text obj obj))
                                item)
        text-block (esc
                    (with-out-str
                      (binding [clojure.pprint/*print-right-margin* 100]
                        (clojure.pprint/pprint replaced-item))))]
    (reduce (fn [result [id text]]
              (cs/replace result
                          (str text)
                          (html/html
                           "["
                           (make-link id (esc (str (:type id) " " (:id id))))
                           "]")))
            text-block
            id->text)))

(defn- generate-item-def [spec item]
  (let [referenced (common/find-ids (dissoc item :id))
        referenced-by (map :id (spec/find-referencing spec item))
        [id-type id-unique] [(:type (:id item)) (:id (:id item))]]
    [:div {:id (id-key (:id item))}
     [:h2 (esc (str id-type " " id-unique))]
     [:pre {:style (str "background: #cc9966;"
                        "color: black;")}
      (render-item item)]
     (when-not (empty? referenced)
       [:div
        [:h4 "Referencing"]
        (make-id-table referenced)])
     (when-not (empty? referenced-by)
       [:div
        [:h4 "Referenced by"]
        (make-id-table referenced-by)])]))

(defn generate [spec]
  (html/html
   [:html
    [:head
     [:style
      (str "a:link, a:visited, a:hover, a:active {"
           "color: #003366;"
           "background-color: #7fbfff;"
           "}")]]
    [:body {:style (str "background-color: wheat;")}
     [:div
      [:h2 "All items"]
      (make-id-table (keys spec))]
     [:hr]
     (->> (sort-by (comp str first) spec)
          (map second)
          (map (partial generate-item-def spec))
          (interpose [:hr]))]]))

(defn -main [input-path output-path]
  (let [[spec errors] (df/read (java.io.File. input-path))]
    (assert (empty? errors) (str "Errors: " errors))
    (spit (java.io.File. output-path) (generate spec))))
