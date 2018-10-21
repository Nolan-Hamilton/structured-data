(ns structured-data)

;131 tests total

(defn do-a-thing [x]
  (let [xDoubled (+ x x)]
    (Math/pow xDoubled xDoubled)))

(defn spiff [v]
  (if (= (get v 2) nil)
    "?"
    (+ (get v 0) (get v 2))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[v1 v2 v3] v]
    (if (= v3 nil)
      "?"
      (+ v1 v3))))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (= (- y2 y1) (- x2 x1))
      true
      false)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (if (and (<= x1 px x2) (<= y1 py y2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[[ox1 oy1] [ox2 oy2]] outer
        [[x1 y1] [x2 y2]] inner]
    (if (and (contains-point? (rectangle [ox1 oy1] [ox2 oy2]) (point x1 y1))
             (contains-point? (rectangle [ox1 oy1] [ox2 oy2]) (point x2 y2)))
      true
      false)))

;-----------------------------------------------------------
; Here are a couple of authors and books pulled from MOOC
;(def china {:name "China Miéville", :birth-year 1972})
;(def octavia {:name "Octavia E. Butler"
;              :birth-year 1947
;              :death-year 2006})
;(def friedman {:name "Daniel Friedman" :birth-year 1944})
;(def felleisen {:name "Matthias Felleisen"})
;
;(def cities {:title "The City and the City" :authors [china]})
;(def wild-seed {:title "Wild Seed", :authors [octavia]})
;(def embassytown {:title "Embassytown", :authors [china]})
;(def little-schemer {:title "The Little Schemer"
;                     :authors [friedman, felleisen]})
;
;(def books [cities, wild-seed, embassytown, little-schemer])

; Here are the same books where authors are in a set.
;(def china {:name "China Miéville", :birth-year 1972})
;(def octavia {:name "Octavia E. Butler"
;              :birth-year 1947
;              :death-year 2006})
;(def friedman {:name "Daniel Friedman" :birth-year 1944})
;(def felleisen {:name "Matthias Felleisen"})
;
;(def cities {:title "The City and the City" :authors #{china}})
;(def wild-seed {:title "Wild Seed", :authors #{octavia}})
;(def embassytown {:title "Embassytown", :authors #{china}})
;(def little-schemer {:title "The Little Schemer"
;                     :authors #{friedman, felleisen}})
;
;(def books [cities, wild-seed, embassytown, little-schemer])
;(def authors #{china, felleisen, octavia, friedman})
;
; Here is another book
;(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
;(def christopher {:name "Christopher Tolkien" :birth-year 1924})
;(def kay {:name "Guy Gavriel Kay" :birth-year 1954})
;
;(def silmarillion {:title "Silmarillion"
;                   :authors #{jrrtolkien, christopher, kay}})
;
; Here is another book with multiple dead authors
;(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
;(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})
;
;(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})
;-----------------------------------------------------------

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (= (count (get book :authors)) 1)
    false
    true))

(defn add-author [book new-author]
  (let [authorList (get book :authors)
        authorName (get new-author :name)]
    (assoc book :authors (conj authorList {:name authorName}))))

(defn alive? [author]
  (if (= (contains? author :death-year) true)
    false
    true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [getSecond (fn [v] (get v 1))]
    (map getSecond collection)))

(defn titles [books]
  (let [getTitle (fn [v] (get v :title))]
    (map getTitle books)))

(defn monotonic? [a-seq]
  (if (or (= (apply <= a-seq) true)
          (= (apply >= a-seq) true))
    true
    false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (= (contains? a-set elem) true)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count (set a-seq)) (count a-seq))
    false
    true))

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (if (= (contains? (get book :authors) author) true)
    true
    false))

; This method is provided by MOOC
; I returns a set of
(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

; This function displays the details of the names
(defn authors [books]
  (let [authorSet (fn [book] (get book :authors))
        authorSets (fn [bookSet] (map authorSet bookSet))]
    (apply clojure.set/union (authorSets books))))

; Fortunately this works correctly
(defn all-author-names [books]
  (let [setOfAuthors (authors books)]
    (set (map (fn [author] (get author :name)) setOfAuthors))))

(defn author->string [author]
  (let [name (get author :name)
        birth (get author :birth-year)
        death (get author :death-year)]
    (if (= birth nil)
      (str name)
	    (if (= death nil)
	      (str name " (" birth " - )")
	      (str name " (" birth " - " death ")")))))

(defn authors->string [authors]
  (let [stringSet (map author->string authors)]
    (apply str (interpose ", " stringSet))))

(defn book->string [book]
  (let [title (get book :title)
        authors (authors->string (get book :authors))]
    (str title ", written by " authors)))

(defn books->string [books]
  (let [numBooks (count books)
        strings (map book->string books)
        sentences (apply str (interpose ". " strings))]
    (if (= numBooks 0)
      (str "No books.")
      (if (= numBooks 1)
        (str numBooks " book. " sentences ".")
        (str numBooks " books. " sentences ".")))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))
 
(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

; This works
(defn has-a-living-author? [book]
  (let [authorlist (get book :authors)
        livingList (filter alive? authorlist)]
    (if (empty? livingList)
      false
      true)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
