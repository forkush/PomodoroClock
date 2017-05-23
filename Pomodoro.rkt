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
(define MESSAGE-FONT-SIZE 30)
(define MESSAGE "Trust the tomato.")
(define FOCUS-FONT-COLOR "red")
(define BREAK-FONT-COLOR "blue")
(define FOCUS-START-TIME 25)
(define BREAK-START-TIME 5)
(define TIME-X-POS (/ WIDTH 2))
(define TIME-Y-POS 50)
(define MESSAGE-X-POS (/ WIDTH 2))
(define MESSAGE-Y-POS 300)


;; =================
;; Data definitions:

;; Clock is ... (give Clock a better name)



;; =================
;; Functions:

;; Clock -> Clock
;; start the world with ...
;; 
(define (main c)
  (big-bang c                   ; Clock
            (on-tick   next-clock)     ; Clock -> Clock
            (to-draw   render)   ; Clock -> Image
            (on-key    handle-key)))    ; Clock KeyEvent -> Clock

;; Clock -> Clock
;; produce the next ...
;; !!!
(define (next-clock c ) ...)


;; Clock -> Image
;; render ... 
;; !!!
(define (render c) ...)

;; Clock KeyEvent -> Clock
;; changes clock mode between Focus and Break when space bar is pressed
;;!!!
(define (handle-key c ke)c)

