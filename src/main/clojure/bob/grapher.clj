(ns bob.grapher
  (:require
     [clojure.set :as set]
     [clojure.string :as s]
     [bob.ruler :refer [ conv-rules-to-tag*inp-res
                         conv-rules-to-out-res*tag]]))

; Converting to/from names to tags describing nodes appearance

(def  ^:private default-tag-attrs
  { :shape "rectangle"
    :color "#FFFFFF" })

(defn conv-tags-to-maps [ tags ]
  (map
    #(assoc default-tag-attrs :tag %)
    tags))

(defn conv-maps-to-tags [tags]
  (map :tag tags))

; Calculating tag dependency graph

(defn- calc-edges-to-tag [ tag conv-tag-to-inp-set-f conv-out-to-tag-set-f ]
  (->> tag
       (conv-tag-to-inp-set-f)
       (map conv-out-to-tag-set-f)
       (reduce set/union)
       (map #(vector % tag))))

(defn calc-tag-graph-edges [ rules needed-tag-maps ]
  (let [
         needed-tag-set (into #{} (conv-maps-to-tags needed-tag-maps))
         tag*inp-res
           (conv-rules-to-tag*inp-res rules)
         out-res*tags
           (conv-rules-to-out-res*tag rules)
         conv-tag-to-inp-set-f
           #(get tag*inp-res % #{})
         conv-out-to-tag-set-f
           #(set/intersection (get out-res*tags % #{}) needed-tag-set)
        ]
    (mapcat
      #(calc-edges-to-tag % conv-tag-to-inp-set-f
                            conv-out-to-tag-set-f )
      needed-tag-set)))

(defn filter-out-self-edges [ edges ]
  (filter #(not= (first %) (second %)) edges))

; Exporting to dot format

(defn- conv-edge-to-dot-str [ [from to]]
  (str "    \"" from "\" -> \"" to "\";"))

(defn- conv-isolated-nodes-to-dot-str [ node ]
  (str "   \"" node "\";"))

(defn- find-isolated-nodes-set [ nodes-set edges ]
  (let [
         connected-nodes-set
           (reduce set/union
              (map #(set %) edges))
       ]
  (set/difference nodes-set connected-nodes-set)))

(defn- conv-node-to-dot-str [ tag-map ]
  (str "    \"" (:tag tag-map) "\" ["
       "shape=\"" (:shape tag-map) "\" "
       "fillcolor=\"" (:color tag-map) "\" "
       "style=\"filled\" ]"))

(defn conv-tag-graph-to-dot-str [ needed-tag-maps edges ]
  (s/join "\n"
     (concat
       [ "digraph G {" ]
       (map conv-node-to-dot-str needed-tag-maps)
       (map conv-edge-to-dot-str edges)
       [ "}"])))

; Exporting to graphml

(defn- get-graphml-header-str []
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\""
    "         xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""
    (str "         xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns"
         " http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\""
         " xmlns:y=\"http://www.yworks.com/xml/graphml\">")
    "  <key attr.name=\"ID\" attr.type=\"string\" for=\"node\" id=\"d1\"><default>empty</default></key>"
    "  <key for=\"node\" id=\"d2\" yfiles.type=\"nodegraphics\"/>"
    "  <graph id=\"G\" edgedefault=\"directed\">" ])

(defn- get-graphml-footer-str []
  [ "  </graph>"
    "</graphml>" ])

(def ^:private dot-shape->graph-ml-shape
  { "trapezium" "trapezoid"
    "invtrapezium" "trapezoid2"
    "box3d" "rectangle3d" })

(defn- get-graphml-shape [ tag-map ]
  (get dot-shape->graph-ml-shape
    (:shape tag-map)
    (:shape tag-map)))

(defn- conv-node-to-graphml-str [ tag-map ]
  (str "    <node id=\"" (:tag tag-map) "\">\n"
       "      <data key=\"d1\">" (:tag tag-map) "</data>\n"
       "      <data key=\"d2\">\n"
       "        <y:ShapeNode>\n"
       "            <y:Shape type=\"" (get-graphml-shape tag-map)  "\"/>\n"
       "            <y:Fill color=\"" (:color tag-map)
                      "\" transparent=\"false\"/>\n"
       "        </y:ShapeNode>\n"
       "      </data>\n"
       "    </node>"))

(defn- conv-edge-to-graphml-str [edge]
  (str "    <edge source=\"" (first edge) "\""
              " target=\"" (second edge) "\"/>"))

(defn conv-tag-graph-to-graphml-str [ needed-tag-maps edges ]
  (s/join "\n"
    (concat
      (get-graphml-header-str)
      (map conv-node-to-graphml-str needed-tag-maps)
      (map conv-edge-to-graphml-str edges)
      (get-graphml-footer-str))))
