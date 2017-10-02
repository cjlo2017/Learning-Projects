;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname puzzle_V2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "puzlib.rkt")

;; DATA DEFINITIONS

;; A Puzzle is a (list (listof String) (listof String))

;; A Grid is a (listof (listof Char))

(define-struct wpos (row col horiz? len))
;; A WPos (Word Position) is a (make-wpos Nat Nat Bool Nat)
;; requires: len > 1

(define-struct state (grid positions words))
;; A State is a (make-state Grid (listof WPos) (listof Str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS FOR TESTING:

(define puzz01 (read-puzzle "puzzle01.txt"))
(define grid-abc '((#\A #\B #\C) (#\X #\Y #\Z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED HELPER:

;; (flip wp) transposes wp by reversing row/col and negating horiz?
;; flip: WPos -> WPos
;; Example:
(check-expect (flip (make-wpos 3 4 true 5))
              (make-wpos 4 3 false 5))

(define (flip wp)
  (make-wpos (wpos-col wp) (wpos-row wp) (not (wpos-horiz? wp)) (wpos-len wp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REQUIRED FUNCTIONS:


;; (transpose g)
;; transpose: Grid -> Grid
;; Examples:
(check-expect (transpose grid-abc) '((#\A #\X) (#\B #\Y) (#\C #\Z)))

(define (transpose g)
  (cond [(empty? g) empty]
        [else 
         (local [(define (nth-element lst n)
                   (cond [(empty? lst) empty]
                         [(zero? n) (first lst)]
                         [else (nth-element (rest lst)(sub1 n))]))
                 (define (trans-row grid current)
                   (cond [(empty? grid) empty]
                         [else (cons (nth-element (first grid) current)
                                     (trans-row (rest grid) current))]))
                 (define (trans-all grid bound counter)
                   (cond [(= bound counter) empty]
                         [else (cons (trans-row grid counter)
                                     (trans-all grid bound (add1 counter)))]))]
           (trans-all g (length (first g)) 0))]))
                            
;; Tests:
(check-expect (transpose empty) empty)
(check-expect (transpose (list (list #\a #\b #\c #\d)))
                         (list (list #\a)(list #\b)(list #\c)(list #\d)))
(check-expect (transpose (transpose grid-abc)) grid-abc)


;; (find-wpos loc row)
;; find-wpos: (listof Char) Nat -> (listof WPos)
;; Examples:
(check-expect (find-wpos (string->list "####") 0)
              (list (make-wpos 0 0 true 4)))

(define (find-wpos loc row)
  (local [;; it creates a list of wpos of strings of #s
          ;; list->slots: (listof Char) Nat Nat -> (listof WPos)
          (define (list->slots loc row count)
            (cond [(empty? loc) empty]
                  [(not(char=? (first loc) #\#))  (list->slots (second (rest-blanks count loc))
                                                               row
                                                               (first (rest-blanks count loc)))]
                  [(empty? (rest-blanks count loc)) (list (first-blank loc row count 0))]
                  [else (cons (first-blank loc row count 0)
                              (list->slots (second (rest-blanks count loc))
                                           row
                                           (first(rest-blanks count loc))))]))
          ;; it creates a wpos out of the first consecutive string of #s
          ;; first-blank: (listof Char) Nat Nat Nat -> WPos
          (define (first-blank loc row start sharps)
            (cond [(empty? loc) empty]
                  [(or (and (char=? (first loc)#\#)(empty? (rest loc)))
                       (and (char=? (first loc)#\#)(not (char=? (first (rest loc)) #\#))))
                   (make-wpos row start true (add1 sharps))]
                  [else #|(and (char=? (first loc) #\#)(char=? (first (rest loc)) #\#))|#
                   (first-blank (rest loc) row start (add1 sharps))]))
          ;; it extracts the list starting with the next #, or empty, if there is non
          ;; rest-blanks: Nat (listof Char) -> 
          (define (rest-blanks start-loc loc)
            (cond [(empty? loc) empty]
                  [(or (and (not (char=? (first loc) #\#))(empty? (rest loc)))
                       (and (not (char=? (first loc) #\#))(char=? (first (rest loc))#\#)))
                   (list (add1 start-loc)(rest loc))]
                  [else (rest-blanks (add1 start-loc)(rest loc))]))
          ] ;; end of local
    (filter (lambda (wpos-rec)
              (not(<= (wpos-len wpos-rec) 1)))
            (list->slots loc row 0) )))

;; Tests:
(check-expect (find-wpos (string->list "###") 5)
              (list (make-wpos 5 0 true 3)))
(check-expect (find-wpos (string->list "..####..") 5)
              (list (make-wpos 5 2 true 4)))
;; the order does not matter: here is an example
;; that uses lists-equiv?
(check-expect (lists-equiv?
               (find-wpos (string->list "..####...###..") 5)
               (list (make-wpos 5 2 true 4)
                     (make-wpos 5 9 true 3)))
              true)
(check-expect (find-wpos (string->list "#.#..#.#") 5)
              empty)

;; the function (get-wpos grid) gets all wpos in a grid
;; get-wpos: (listof (listof Char)) -> (listof WPos)
;; Examples
;; Definitions
(define (get-wpos grid)
  (local [;; it gets all wpos horizontally
          ;; get-wpos-horiz: (listof (listof Char)) Nat -> (listof WPos)
          (define (get-wpos-horiz grid count)
            (cond [(empty? grid) empty]
                  [else (append (find-wpos (first grid) count)      
                                (get-wpos-horiz (rest grid)(add1 count)))]))
          ;; it gets all wpos vertically
          ;; get-wpos-vert: (listof (listof Char)) -> (listof WPos)
          (define (get-wpos-vert grid)
            (local [(define (flip-lowpos lowpos)
                      (map flip lowpos))]
              (flip-lowpos (get-wpos-horiz (transpose grid) 0))))
          ] ;; end of local
    (append (get-wpos-horiz grid 0)(get-wpos-vert grid))) )
;; Tests








;; (initial-state puzzle) creates the initial state using a provided puzzle and a list of words
;; initial-state: Puzzle -> State
;;Examples:
(check-expect (initial-state puzz01)
              (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))
(define (initial-state puzzle)
  (local [;; it converts a list of string into a list of list of char
          ;; los->loloc: (listof Str) -> (listof (listof Char))
          (define (los->loloc los)
            (cond [(empty? los) empty]
                  [else (cons (string->list (first los))
                              (los->loloc (rest los)))]))    
          ] ;; end of local
    (make-state (los->loloc (first puzzle))
                (get-wpos (los->loloc (first puzzle)))
                (second puzzle)) )  )

;; Tests:


;; (extract-wpos g wp) extracts the list from the grid using information from wpos
;; extract-wpos: Grid WPos -> (listof Char)
;; Examples: 
(check-expect (extract-wpos grid-abc (make-wpos 0 0 true 2)) '(#\A #\B))
(check-expect (extract-wpos grid-abc (make-wpos 0 0 false 2)) '(#\A #\X))
(check-expect (extract-wpos grid-abc (make-wpos 0 2 false 2)) '(#\C #\Z))

(define (extract-wpos g wp)
  (local [(define (nth-element lst n)
            (cond [(empty? lst) empty]
                  [(= n 0) (first lst)]
                  [else (nth-element (rest lst)(sub1 n))]))
          (define (sub-list lst start end)
            (cond [(empty? lst) empty]
                  [(= start end) empty]
                  [else (cons (nth-element lst start)
                              (sub-list lst (add1 start) end))]))
          
          ] ;; end of locals
    (cond [(boolean=? (wpos-horiz? wp) true)
           (sub-list (nth-element g (wpos-row wp))
                     (wpos-col wp)
                     (+ (wpos-col wp)(wpos-len wp)))]
          [else
           (local [(define gtrans (transpose g))]
             (sub-list (nth-element gtrans (wpos-col wp))
                       (wpos-row wp)
                       (+ (wpos-row wp)(wpos-len wp))))])))
   

;; Tests:


;; (replace-wpos g wp loc) replaces the characters of the word positions from a provided list of characters
;; replace-wpos: Grid WPos (listof Char) -> Grid
;; requires: len in WPos is equal to length of (listof Char)
;; Examples:
(check-expect (replace-wpos grid-abc (make-wpos 0 0 true 2) '(#\J #\K))
              '((#\J #\K #\C) (#\X #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 0 false 2) '(#\J #\K))
              '((#\J #\B #\C) (#\K #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 2 false 2) '(#\J #\K))
              '((#\A #\B #\J) (#\X #\Y #\K)))

(define (replace-wpos g wp loc)
  (local [(define (replace-one lst element n)
            (cond [(empty? lst) empty]
                  [(zero? n) (cons element (rest lst))]
                  [else (cons (first lst)
                              (replace-one (rest lst) element (sub1 n)))]))
          (define (replace-within origlist replacelst start end now)
            (cond [(empty? origlist) empty]
                  [(empty? replacelst) origlist]
                  [(and (>= now start)(>= end now))
                   (cons (first replacelst)
                         (replace-within (rest origlist)(rest replacelst) start end (add1 now)))]
                  [else (cons (first origlist)
                              (replace-within (rest origlist) replacelst start end (add1 now)))]))
          (define (nth-element lst n)
            (cond [(empty? lst) empty]
                  [(= n 0) (first lst)]
                  [else (nth-element (rest lst)(sub1 n))]))
          
          (define (replace-wpos-horiz grid wp loc)
            (replace-one grid
                         (replace-within (nth-element grid (wpos-row wp)) loc (wpos-col wp) (+ (wpos-col wp)(wpos-len wp) -1) 0)
                         (wpos-row wp) ))
          (define (replace-wpos-vert grid wp loc)
            (transpose
             (replace-one (transpose grid)
                          (replace-within (nth-element (transpose grid)(wpos-col wp))
                                          loc
                                          (wpos-row wp)
                                          (+ (wpos-row wp)(wpos-len wp) -1)0)
                          (wpos-col wp))))
          ] ;; end of local
    (cond [(boolean=?(wpos-horiz? wp)true) (replace-wpos-horiz g wp loc)]
          [else (replace-wpos-vert g wp loc)])))
;; Tests:


;; (fit? word cells) determines whether the given word fits into the cells
;; fit? (listof Char) (listof Char) -> Bool
;; Examples:
(check-expect (fit? (string->list "STARWARS") (string->list "S##RW##S")) true)
(check-expect (fit? (string->list "STARWARS") (string->list "S##RT##K")) false)

(define (fit? word cells)
  (local [(define (first-elements-equal word cells)
            (cond [(and (empty? word)(empty? cells)) true]
                  [(and (empty? word)(cons? cells)) true]
                  [else (and (or (char=?(first word)(first cells))(char=? (first cells) #\#))
                             (first-elements-equal (rest word)(rest cells)))]))
          ];; end of local
    (cond [(= (length word)(length cells))
           (foldr (lambda (a b)
                    (and a b))
                  true
                  (map (lambda (x y)
                         (or (char=? x y)
                             (char=? y #\#)))
                       word cells))]
          [(> (length word)(length cells)) false]
          [else (first-elements-equal word cells)])))
           
;; Tests:


;; (neighbours s) generates a list of out-neighbor states from a given state
;; neighbours: State -> (listof State)
;; Examples:
(check-expect (neighbours (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))
              (list (make-state '((#\C #\A #\T)) empty empty)))

(define (neighbours s)
  (local [(define grid0 (state-grid s))
          (define positions0 (state-positions s))
          (define words0 (state-words s))
          (define (count-letters grid wpos)
            (length (filter (lambda (element)
                                          (not (or (char=? element #\#)
                                                   (char=? element #\.))))
                                        (extract-wpos grid wpos))))
          (define (max-count-wpos grid lowpos max-wpos-so-far)
            (cond [(empty? lowpos) max-wpos-so-far]
                  [(>(count-letters grid (first lowpos))(count-letters grid max-wpos-so-far))
                   (max-count-wpos grid (rest lowpos) (first lowpos))]
                  [else (max-count-wpos grid (rest lowpos) max-wpos-so-far)]))
          (define (wpos-selector grid lowpos)
            (cond [(empty? (rest lowpos)) (first lowpos)]
                  [else (max-count-wpos grid
                                        (rest lowpos)
                                        (first lowpos))]))
          (define (placeable-states grid lowp wp lostr processing-lostr)
            (cond [(empty? lostr) empty]
                  [(empty?(rest processing-lostr))
                   (cons (make-state
                          (replace-wpos grid wp (string->list (first processing-lostr)))
                          (filter (lambda (x)
                                    (not (and (=(wpos-row x)(wpos-row wp))
                                              (=(wpos-col x)(wpos-col wp))
                                              (boolean=?(wpos-horiz? x)(wpos-horiz? wp))
                                              (=(wpos-len x)(wpos-len wp)))))
                                  lowp)
                          (filter (lambda (x)
                                    (not(string=? x (first processing-lostr))))
                                  lostr))
                         empty)]
                  [(fit? (string->list(first processing-lostr))(extract-wpos grid wp))
                   (cons (make-state
                          (replace-wpos grid wp (string->list (first processing-lostr)))
                          (filter (lambda (x)
                                    (not (and (=(wpos-row x)(wpos-row wp))
                                              (=(wpos-col x)(wpos-col wp))
                                              (boolean=?(wpos-horiz? x)(wpos-horiz? wp))
                                              (=(wpos-len x)(wpos-len wp)))))
                                  lowp)
                          (filter (lambda (x)
                                    (not(string=? x (first processing-lostr))))
                                  lostr))
                         (placeable-states grid lowp wp lostr (rest processing-lostr)))]
                  [else (placeable-states grid lowp wp lostr (rest processing-lostr))]))
          ];; end of local
    (placeable-states grid0 positions0 (wpos-selector grid0 positions0) words0 words0)))

;(wpos-selector '((#\C #\# #\#)))
;(extract-wpos '((#\C #\# #\#)) (make-wpos 0 0 true 3))


;; Tests:
(check-expect (neighbours (make-state '((#\C #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      '("CAT" "DOG" "CAR")))
              (list (make-state '((#\C #\A #\T)) empty '("DOG" "CAR"))
                    (make-state '((#\C #\A #\R)) empty '("CAT" "DOG"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED FUNCTIONS:

;; (solved? s) determines if s is a solved criss-cross problem
;;   by checking if all of the word positions have been filled
;; solved?: State -> Bool
(define (solved? s)
  (empty? (state-positions s)))


;; (criss-cross puzzle) produces a list of strings corresponding
;;   to the solution of the the criss-cross puzzle,
;;   or false if no solution is possible
;; criss-cross: Puzzle -> (anyof false (listof Str))

(define (criss-cross puzzle)
  (local [(define result (solve (initial-state puzzle)
                                neighbours
                                solved?))]
    (cond [(false? result) false]
          [else (map list->string (state-grid result))])))

; (check-expect (criss-cross puzz01) '("CAT"))

;; note that [solve] (used in criss-cross above) is provided in puzlib.rkt

;; when you are all done, you can use disp to
;; view your solutions:

; (disp (criss-cross (read-puzzle "puzzle02.txt")))

;; NOTE: Do NOT leave top-level expressions in your code.
;;       In other words, when your code is run, only the
;;       check-expect message "All X tests passed!"
;;       should appear in the interactions window

