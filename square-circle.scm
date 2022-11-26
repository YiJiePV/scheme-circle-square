; Square-Circle Program
; Author: Karena Qian
; Professor: Carlos Arias
; Purpose: This program implements the makepoint, print, contained, and intersects functions of the square-circle grammar introduced in the Rust Compiler program
; Date: October 30, 2022

; The PI global variable
(define PI 3.14159265358979)
; `(makepoint x-cor y-cor)`. Creates a new point with `x-cor` and `y-cor`.
; Param: x-cor (x value of point) y-cor (y value of point)
; Returns: "list" with values x-cor and y-cor (x-cor . y-cor)
(define (makepoint x-cor y-cor)
    (cons x-cor y-cor))
; Helper function: `(get-x point)`. Gets the x value of a point.
; Param: point (point to get x value from)
; Returns: x value of given point
(define (get-x point)
    (car point))
; Helper function: `(get-y point)`. Gets the y value of a point.
; Param: point (point to get y value from)
; Returns: y value of given point
(define (get-y point)
    (cdr point))
; `(print-circle center radius)`. Prints area and perimeter of given circle with center point and radius value in following format:
; Circle
; Area:      #.#####
; Perimeter: #.#####
; Param: center (point representing circle's center) radius (circle's radius)
; Returns: none
(define (print-circle center radius)
    (display "Circle")
    (newline)
    (display "Area:      ")
    (fluid-let ((flonum-unparser-cutoff '(absolute 5)))
        (display(number->string(* radius radius PI))))
    (newline)
    (display "Perimeter: ")
    (fluid-let ((flonum-unparser-cutoff '(absolute 5)))
        (display(number->string(* radius PI 2))))
    (newline))
;`(print-square corner length)`. Prints area and perimeter of square with top-left-corner point and length value in following format:
; Square
; Area:      #
; Perimeter: #
; Param: corner (point representing square's top-left-corner) length (square's side length)
; Returns: none
(define (print-square corner length)
    (display "Square")
    (newline)
    (display "Area:      ")
    (display (* length length))
    (newline)
    (display "Perimeter: ")
    (display (* length 4))
    (newline))
; Helper function: `(dist-point point1 point2)`. Gives the distance between two points.
; Param: point1 (first point) point2 (second point)
; Returns: distance between given 2 points
(define (dist-point point1 point2)
    (define x1 (get-x point1))
    (define x2 (get-x point2))
    (define y1 (get-y point1))
    (define y2 (get-y point2))
    (sqrt(+ (* (- x1 x2) ( - x1 x2)) (* (- y1 y2) (- y1 y2)))))
; `(contained-circle-circle center1 radius1 center2 radius2)`. Checks whether circle with `center1` and `radius1` is contained in circle with `center2` and `radius2`.
; Param: center1 (center of first circle) radius1 (radius of first circle) center2 (center of second circle) radius2 (radius of second circle)
; Returns: true if first circle is contained in second circle, false otherwise
(define (contained-circle-circle center1 radius1 center2 radius2)
    (define dist (dist-point center1 center2))
    (define r-diff (- radius2 radius1))
    (<= dist r-diff))
; Helper function: `(get-squ-center corner length)`. Gets center of square with `corner` and `length`.
; Param: corner (top-left-corner of square) length (side-length of square)
; Returns: center point of given square
(define (get-squ-center corner length)
    (define x (get-x corner))
    (define y (get-y corner))
    (makepoint (+ x (/ length 2)) (- y (/ length 2))))
; Helper function: `(get-squ-radius length)`. Gets radius of circle surrounding given square with side-length `length`.
; Param: length (side-length of square)
; Returns: radius of circle surrounding square (half of square's diagonal)
(define (get-squ-radius length)
    (sqrt (+ (* (/ length 2) (/ length 2)) (* (/ length 2) (/ length 2)))))
; `(contained-circle-square center radius corner length)`. Checks whether circle with `center` and `radius` is contained in square with `corner` and `length`.
; Param: center (center of circle) radius (radius of circle) corner (top-left-corner of square) length (side-length of square)
; Returns: true if square contains circle, false otherwise
(define (contained-circle-square center radius corner length)
    ;convert square to circle
    (define squ-center (get-squ-center corner length))
    (define squ-radius (get-squ-radius length))
    (contained-circle-circle center radius squ-center squ-radius))
; `(contained-square-circle corner length center radius)`. Checks whether square with `corner` and `length` is contained in circle with `center` and `radius`.
; Param: corner (top-left-corner of square) length (side-length of square) center (center of circle) radius (radius of circle)
; Returns: true if circle contains square, false otherwise
(define (contained-square-circle corner length center radius)
    ;convert square to circle
    (define squ-center (get-squ-center corner length))
    (define squ-radius (get-squ-radius length))
    (contained-circle-circle squ-center squ-radius center radius))
; `(contained-square-square corner1 length1 corner2 length2)`. Checks whether square with `corner1` and `length1` is contained in square with `corner2` and `length2`.
; Param: corner1 (top-left-corner of first square) length1 (side-length of first square) corner2 (top-left-corner of second square) length2 (side-length of second square)
; Returns: true if first square is contained in second square, false otherwise
(define (contained-square-square corner1 length1 corner2 length2)
    ;convert squares to circles
    (define squ-center1 (get-squ-center corner1 length1))
    (define squ-radius1 (get-squ-radius length1))
    (define squ-center2 (get-squ-center corner2 length2))
    (define squ-radius2 (get-squ-radius length2))
    (contained-circle-circle squ-center1 squ-radius1 squ-center2 squ-radius2))
; `(intersects-circle-circle center1 radius1 center2 radius2)`. It checks whether circle with `center1` and `radius1` intersects circle with `center2` and `radius2`.
; Param: center1 (center of first circle) radius1 (radius of first circle) center2 (center of second circle) radius2 (radius of second circle)
; Returns: true if circles intersect each other, false otherwise
(define (intersects-circle-circle center1 radius1 center2 radius2)
    (define dist (dist-point center1 center2))
    (define r-add (+ radius2 radius1))
    (and 
        (< dist r-add)
        (and 
            (not (contained-circle-circle center1 radius1 center2 radius2)) 
            (not (contained-circle-circle center2 radius2 center1 radius1)))))
; `(intersects-circle-square center radius corner length)`. It checks whether circle with `center` and `radius`  intersects square with `corner` and `length`.
; Param: center (center of circle) radius (radius of circle) corner (top-left-corner of square) length (side-length of square)
; Returns: true if circle intersects with square, false otherwise
(define (intersects-circle-square center radius corner length)
    (define cir-corner (makepoint (- (get-x center) radius) (+ (get-y center) radius))) ;top-left corner of square surrounding circle
    (define cir-length (* radius 2)) ;length of square surrounding circle
    (define squ-x (get-x corner))
    (define squ-y (get-y corner))
    (define top-right (makepoint (+ squ-x length) squ-y)) ;top-right corner of square
    (define bottom-right (makepoint (+ squ-x length) (- squ-y length))) ;bottom-right corner of square
    (define bottom-left (makepoint squ-x (- squ-y length))) ;bottom-left corner of square
    (and ;two cases: distance between a corner and center < radius || square surrounding circle intersects with square
        (or
            (or
                (or
                    (<= (dist-point corner center) radius)
                    (<= (dist-point top-right center) radius))
                (or
                    (<= (dist-point bottom-right center) radius)
                    (<= (dist-point bottom-left center) radius)))
            (intersects-square-square corner length cir-corner cir-length))
        (and
            (not (contained-circle-square center radius corner length))
            (not (contained-square-circle corner length center radius)))))
; `(intersects-square-circle corner length center radius)`. It checks whether square with `corner` and `length`  intersects circle with `center` and `radius`.
; Param: corner (top-left-corner of square) length (side-length of square) center (center of circle) radius (radius of circle)
; Returns: true if square intersects with circle, false otherwise
(define (intersects-square-circle corner length center radius)
    (intersects-circle-square center radius corner length))
; Helper function: `(squ-contain-point x1 y1 x2-1 x2-2 y2-1 y2-2)`. Checks if point (x1, y1) is within square with corners (x2-1, y2-1) and (x2-2, y2-2)
; Param: x1 (x value of point) y1 (y value of point) x2-1 (x value of square corner (x2-1, y2-1)) y2-1 (y value of square corner (x2-1, y2-1)) x2-2 (x value of square corner (x2-2, y2-2)) y2-2 (y value of square corner (x2-2, y2-2))
; Returns: true if square with corners (x2-1, y2-1) and (x2-2, y2-2) contains point (x1, y1), false otherwise
(define (squ-contain-point x1 y1 x2-1 x2-2 y2-1 y2-2)
    (and 
        (and
            (>= x1 x2-1)
            (< x1 x2-2))
        (and
            (<= y1 y2-1)
            (> y1 y2-2))))
;Helper function: `(corner-in-corner corner1 length1 corner2 length2)`. Checks if at least one of the corners of square with `corner1` and `length1` is within square with `corner2` and `length2`. 
; Param: corner1 (top-left-corner of first square) length1 (side-length of first square) corner2 (top-left-corner of second square) length2 (side-length of second square)
; Returns: true if one or more corners of square with `corner1` and `length1` is within square with `corner2` and `length2`, false otherwise
(define (corner-in-corner corner1 length1 corner2 length2)
    (define squ1-x (get-x corner1))
    (define squ1-y (get-y corner1))
    (define squ2-x (get-x corner2))
    (define squ2-y (get-y corner2))
    (define squ1-xr (+ squ1-x length1)) ;x value on right side of first square
    (define squ1-yb (- squ1-y length1)) ;y value on bottom side of first square
    (define squ2-xr (+ squ2-x length2)) ;x value on right side of second square
    (define squ2-yb (- squ2-y length2)) ;y value on bottom side of second square
    (or
        (or 
            (squ-contain-point squ1-x squ1-y squ2-x squ2-xr squ2-y squ2-yb)
            (squ-contain-point squ1-xr squ1-y squ2-x squ2-xr squ2-y squ2-yb))
        (or 
            (squ-contain-point squ1-xr squ1-yb squ2-x squ2-xr squ2-y squ2-yb)
            (squ-contain-point squ1-x squ1-yb squ2-x squ2-xr squ2-y squ2-yb))))
; `(intersects-square-square corner1 length1 corner2 length2)`. It checks whether square represented by `corner1` and `length1`  intersects square represented by `corner2` and `length2`.
; Param: corner1 (top-left-corner of first square) length1 (side-length of first square) corner2 (top-left-corner of second square) length2 (side-length of second square)
; Returns: true if squares intersect, false otherwise
(define (intersects-square-square corner1 length1 corner2 length2)
    (define squ1-x (get-x corner1))
    (define squ1-y (get-y corner1))
    (define squ2-x (get-x corner2))
    (define squ2-y (get-y corner2))
    (and
        (and 
            (not (contained-square-square corner1 length1 corner2 length2))
            (not (contained-square-square corner2 length2 corner1 length1)))
        (and
            (corner-in-corner corner1 length1 corner2 length2)
            (corner-in-corner corner2 length2 corner1 length1))))