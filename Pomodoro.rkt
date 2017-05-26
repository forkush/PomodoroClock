;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Pomodoro) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; A Pomodoro Clock with a 25 minute Focus mode and a 5 minute Break mode

;; =================
;; Constants:

(define WIDTH 150)
(define HEIGHT 150)
(define MTS (empty-scene WIDTH HEIGHT))
(define FONT-SIZE 48)
(define FOCUS-FONT-COLOR "red")
(define BREAK-FONT-COLOR "blue")
(define FOCUS-START-MINUTES 25)
(define BREAK-START-MINUTES 5)
(define FOCUS-TITLE "Focus")
(define BREAK-TITLE "Break")
(define TIME-X-POS (/ WIDTH 2))
(define TIME-Y-POS (* HEIGHT .7))
(define TITLE-X-POS (/ WIDTH 2))
(define TITLE-Y-POS (* HEIGHT .3))


;; =================
;; Data definitions:

(define-struct clock (min sec mode))
;; Clock is (make-clock Natural Natural Boolean)
;; interp. the minutes and seconds remaining on a countdown clock, true mode means it is in 25 min "Focus" mode, otherwise it is in 5 min "Break" mode

(define C1 (make-clock 25 0 true))
(define C2 (make-clock 4 30 false))
#;
(define (fn-for-clock c)
  (... (clock-min  c)       ;Natural
       (clock-sec  c)       ;Natural
       (clock-mode c)))     ;Boolean

;; Template rules used:
;;  - compound: 3 fields

;; =================
;; Functions:

;; Clock -> Clock
;; start the world with (main (make-clock 25 0 true))
;; 
(define (main c)
  (big-bang c                           ; Clock
            (on-tick   next-clock 1)    ; Clock -> Clock
            (to-draw   render)          ; Clock -> Image
            (on-key    handle-key)))    ; Clock KeyEvent -> Clock

;; Clock -> Clock
;; produce the next lowest clock time by one second, changing modes and resetting after it reaches 0.
(check-expect (next-clock (make-clock 25  0  true)) (make-clock                   24 59 true))
(check-expect (next-clock (make-clock 24 59  true)) (make-clock                   24 58 true))
(check-expect (next-clock (make-clock  4 30 false)) (make-clock                    4 29 false))
(check-expect (next-clock (make-clock  0 59  true)) (make-clock                    0 58  true))
(check-expect (next-clock (make-clock  0  0  true)) (make-clock BREAK-START-MINUTES   0 false))  ;Focus clock reaches 0, converts to Break clock
(check-expect (next-clock (make-clock  0  0 false)) (make-clock FOCUS-START-MINUTES   0 true))   ;Break clock reaches 0, converts to Focus clock

;(define (next-clock c ) c)    ;stub
; Template from Clock

(define (next-clock c)
  (cond [(> (clock-sec c) 0)                           (make-clock (clock-min c) (- (clock-sec c) 1) (clock-mode c))]
        [(and (> (clock-min c) 0) (= (clock-sec c) 0)) (make-clock (- (clock-min c) 1) 59 (clock-mode c))]
        [else                                          (make-clock (set-start-time c) 0 (not (clock-mode c)))]))

;; Clock -> Natural
;; produces BREAK-START-MINUTES if clock mode is true, otherwise produces FOCUS-START-MINUTES 
(check-expect (set-start-time (make-clock 10 10 true))  BREAK-START-MINUTES)
(check-expect (set-start-time (make-clock 10 10 false)) FOCUS-START-MINUTES)

;(define (set-start-time c) 0)     ;stub
; Template from Clock

(define (set-start-time c)
  (if (clock-mode c)
      BREAK-START-MINUTES
      FOCUS-START-MINUTES))

;; Clock -> Image
;; render the appropriate clock time, title, and color
(check-expect (render (make-clock 24 30 true))
              (place-image (text FOCUS-TITLE FONT-SIZE FOCUS-FONT-COLOR) TITLE-X-POS TITLE-Y-POS
                           (place-image (text  (string-append  (number->string 24) ":" (number->string 30)) FONT-SIZE FOCUS-FONT-COLOR)
                                        TIME-X-POS TIME-Y-POS MTS)))
(check-expect (render (make-clock 24 3 true))
              (place-image (text FOCUS-TITLE FONT-SIZE FOCUS-FONT-COLOR) TITLE-X-POS TITLE-Y-POS
                           (place-image (text  (string-append  (number->string 24) ":" "0" (number->string 3)) FONT-SIZE FOCUS-FONT-COLOR)
                                        TIME-X-POS TIME-Y-POS MTS)))

(check-expect (render (make-clock 4 30 false))
              (place-image (text BREAK-TITLE FONT-SIZE BREAK-FONT-COLOR) TITLE-X-POS TITLE-Y-POS
                           (place-image (text  (string-append  (number->string 4) ":" (number->string 30)) FONT-SIZE BREAK-FONT-COLOR)
                                        TIME-X-POS TIME-Y-POS MTS)))

;(define (render c) MTS)     ;stub
; Template from Clock

(define (render c)
  (place-image
   (render-title c) TITLE-X-POS TITLE-Y-POS
   (place-image
    (render-clock c) TIME-X-POS TIME-Y-POS MTS)))

;; Clock -> Image
;; render the minutes and seconds in the appropriate color
(check-expect (render-clock (make-clock 24 30 true))
              (text  (string-append  (number->string 24) ":" (number->string 30)) FONT-SIZE FOCUS-FONT-COLOR))
(check-expect (render-clock (make-clock 4 30 false))
              (text  (string-append  (number->string 4) ":" (number->string 30)) FONT-SIZE BREAK-FONT-COLOR))              
              
;(define (render-clock c) MTS)     ;stub
; Template from Clock

(define (render-clock c)
  (text  (string-append  (number->string (clock-min c)) ":" (zero-padding c) (number->string (clock-sec c))) FONT-SIZE (set-font-color c)))
  

;; Clock -> Image
;; render the appropriate title in the appropriate color
(check-expect (render-title (make-clock 24 30 true))
              (text FOCUS-TITLE FONT-SIZE FOCUS-FONT-COLOR))
(check-expect (render-title (make-clock 24 30 false))
              (text BREAK-TITLE FONT-SIZE BREAK-FONT-COLOR))
              
;(define (render-title c) MTS)   stub
; Template from Clock

(define (render-title c)
  (text (set-title c) FONT-SIZE (set-font-color c)))

;; Clock -> String
;; produces FOCUS-TITLE if clock mode is true, otherwise produces BREAK-TITLE
(check-expect (set-title (make-clock 1 1 true))  FOCUS-TITLE)
(check-expect (set-title (make-clock 1 1 false)) BREAK-TITLE)

;(define (set-title c) "")    ;stub
; Template from Clock

(define (set-title c)
  (if (clock-mode c)
      FOCUS-TITLE
      BREAK-TITLE))

;; Clock -> String
;; produces FOCUS-FONT-COLOR if clock mode is true, otherwise produces BREAK-FONT-COLOR
(check-expect (set-font-color (make-clock 1 1 true))  FOCUS-FONT-COLOR)
(check-expect (set-font-color (make-clock 1 1 false)) BREAK-FONT-COLOR)

;(define (set-font-color c) "green")    ;stub 
; Template from Clock

(define (set-font-color c)
  (if (clock-mode c)
      FOCUS-FONT-COLOR
      BREAK-FONT-COLOR))

;; Clock -> String
;; produces the string "0" if clock mode is true, otherwise produces ""
(check-expect (zero-padding (make-clock 1 10 true)) "")
(check-expect (zero-padding (make-clock 1 9 true)) "0")

;(define (zero-padding c) "x")    ;stub
; Template from Clock

(define (zero-padding c)
  (if (< (clock-sec c) 10)
      "0"
      "")) 

;; Clock KeyEvent -> Clock
;; changes clock mode between Focus and Break and resets clocks appropriately when space bar is pressed
(check-expect (handle-key (make-clock 1 1 true)  " ") (make-clock  BREAK-START-MINUTES 0 false))
(check-expect (handle-key (make-clock 1 1 false) "x") (make-clock                    1 1 false))
(check-expect (handle-key (make-clock 1 1 false) " ") (make-clock FOCUS-START-MINUTES  0 true))
(check-expect (handle-key (make-clock 1 1 true)  "x") (make-clock                    1 1 true))

;(define (handle-key c ke)c)    ;stub
; Template according to KeyEvent

(define (handle-key c ke)
  (cond [(key=? ke " ") (make-clock (set-start-time c) 0 (not (clock-mode c)))]
        [else c]))