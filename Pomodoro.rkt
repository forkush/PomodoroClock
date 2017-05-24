;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Pomodoro) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; A Pomodoro Clock with two modes, 25 minutes and 5 minutes

;; =================
;; Constants:
(define WIDTH 400)
(define HEIGHT 400)
(define MTS (empty-scene WIDTH HEIGHT))
(define BLANK (square 0 "solid" "white"))
(define DEFAULT-FONT-SIZE 48)
(define FOCUS-FONT-COLOR "red")
(define BREAK-FONT-COLOR "blue")
(define FOCUS-START-TIME 25)
(define BREAK-START-TIME 5)
(define TIME-X-POS (/ WIDTH 2))
(define TIME-Y-POS 150)
(define TITLE-X-POS (/ WIDTH 2))
(define TITLE-Y-POS 50)


;; =================
;; Data definitions:

(define-struct clock (min sec mode))
;; Clock is (make-clock Natural Natural Boolean)
;; interp. a countdown clock with min and seconds remaining, in the Focus or Break mode
(define C1 (make-clock 25 0 false))
(define C2 (make-clock 4 30 true))
#;
(define (fn-for-clock c)
  (... (clock-min c)        ;Natural
       (clock-sec c)        ;Natural
       (clock-mode c)))     ;Boolean    

;; =================
;; Functions:

;; Clock -> Clock
;; start the world with (main (make-clock 25 0 true))
;; 
(define (main c)
  (big-bang c                           ; Clock
            (on-tick   next-clock)      ; Clock -> Clock
            (to-draw   render)          ; Clock -> Image
            (on-key    handle-key)))    ; Clock KeyEvent -> Clock

;; Clock -> Clock
;; produce the next lowest clock time by one second and change mode after it reaches 0.
(check-expect (next-clock (make-clock 25  0  true)) (make-clock 24 59 true))
(check-expect (next-clock (make-clock 24 59  true)) (make-clock 24 58 true))
(check-expect (next-clock (make-clock  4 30 false)) (make-clock  4 29 false))
(check-expect (next-clock (make-clock  0 59  true)) (make-clock  0 58  true))
(check-expect (next-clock (make-clock  0  0  true)) (make-clock  5  0 false))  ;Focus clock reaches 0, converts to Break clock
(check-expect (next-clock (make-clock  0  0 false)) (make-clock 25  0 true))   ;Break clock reaches 0, converts to Focus clock
;(define (next-clock c ) c)    ;stub

(define (next-clock c)
  (cond [(> (clock-sec c) 0) (make-clock (clock-min c) (- (clock-sec c) 1) (clock-mode c))]
        [(and (> (clock-min c) 0) (= (clock-sec c) 0)) (make-clock (- (clock-min c) 1) 59 (clock-mode c))]
        [else
         (make-clock (focus-mode? c) 0 (not (clock-mode c)))]))

;; Clock -> Natural
;; produces 5 if mode is true, 25 if mode is false
(check-expect (focus-mode? (make-clock 10 10 true)) 5)
(check-expect (focus-mode? (make-clock 10 10 false)) 25)
;(define (focus-mode? c) 0)     ;stub

(define (focus-mode? c)
  (if (clock-mode c)
      BREAK-START-TIME
      FOCUS-START-TIME))



;; Clock -> Image
;; render the appropriate clock time 
(check-expect (render (make-clock 24 30 true))
              (place-image (text "Focus" DEFAULT-FONT-SIZE FOCUS-FONT-COLOR) TITLE-X-POS TITLE-Y-POS
                           (place-image (text  (string-append  (number->string 24) ":" (number->string 30)) DEFAULT-FONT-SIZE FOCUS-FONT-COLOR)
                                        TIME-X-POS TIME-Y-POS MTS)))
(check-expect (render (make-clock 24 3 true))
              (place-image (text "Focus" DEFAULT-FONT-SIZE FOCUS-FONT-COLOR) TITLE-X-POS TITLE-Y-POS
                           (place-image (text  (string-append  (number->string 24) ":" "0" (number->string 3)) DEFAULT-FONT-SIZE FOCUS-FONT-COLOR)
                                        TIME-X-POS TIME-Y-POS MTS)))

(check-expect (render (make-clock 4 30 false))
              (place-image (text "Break" DEFAULT-FONT-SIZE BREAK-FONT-COLOR) TITLE-X-POS TITLE-Y-POS
                           (place-image (text  (string-append  (number->string 4) ":" (number->string 30)) DEFAULT-FONT-SIZE BREAK-FONT-COLOR)
                                        TIME-X-POS TIME-Y-POS MTS)))
;(define (render c) MTS)     ;stub

(define (render c)
  (place-image (text (focus-text? c) DEFAULT-FONT-SIZE (focus-color? c)) TITLE-X-POS TITLE-Y-POS
               (place-image (text  (string-append  (number->string (clock-min c)) ":" (need-padding? c) (number->string (clock-sec c))) DEFAULT-FONT-SIZE (focus-color? c))
                            TIME-X-POS TIME-Y-POS MTS)))

;; Clock -> String
;; return "Focus" if true, "Break" if false
(check-expect (focus-text? (make-clock 1 1 true)) "Focus")
(check-expect (focus-text? (make-clock 1 1 false)) "Break")
;(define (focus-text? c) "")    ;stub

(define (focus-text? c)
  (if (clock-mode c)
      "Focus"
      "Break"))

;; Clock -> String
;; return "red" if true, "blue" if false
(check-expect (focus-color? (make-clock 1 1 true)) "red")
(check-expect (focus-color? (make-clock 1 1 false)) "blue")
;(define (focus-color? c) "green")    ;stub

(define (focus-color? c)
  (if (clock-mode c)
      FOCUS-FONT-COLOR
      BREAK-FONT-COLOR))

;; Clock -> String
;; return "0" if true, return "" if false
(check-expect (need-padding? (make-clock 1 10 true)) "")
(check-expect (need-padding? (make-clock 1 9 true)) "0")
;(define (need-padding? c) "x")    ;stub

(define (need-padding? c)
  (if (< (clock-sec c) 10)
      "0"
      ""))



;; Clock KeyEvent -> Clock
;; changes clock mode between Focus and Break when space bar is pressed
;;!!!
(define (handle-key c ke)c)

