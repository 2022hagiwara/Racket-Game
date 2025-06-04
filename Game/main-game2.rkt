#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(define midori (bitmap/file "midori2.jpg"));;;;サイズは800*600になってる
(define tou (bitmap/file "tou.png"))
(define pz-siki (bitmap/file "pz-siki.png")) ;パズルの答えの画像

(define tr-ura0 (bitmap/file "imgpsh_mobile_save.jpg"));トランプ裏の画像
(define tr-ura (scale 0.24 tr-ura0));トランプ裏のサイズをいい感じに

;トランプの画像読み込み
(define tr1 (bitmap/file "1.png"))
(define tr2 (bitmap/file "2.png"))
(define tr3 (bitmap/file "3.png"))
(define tr4 (bitmap/file "4.png"))
(define tr5 (bitmap/file "5.png"))
(define tr6 (bitmap/file "6.png"))
(define tr7 (bitmap/file "7.png"))
(define tr8 (bitmap/file "8.png"))
(define tr9 (bitmap/file "9.png"))
(define tr10 (bitmap/file "10.png"))
(define tr11 (bitmap/file "11.png"))
(define tr12 (bitmap/file "12.png"))
(define tr13 (bitmap/file "13.png"))


;;ランダムに出た数字に対応するトランプの画像を返す
;;トランプの画像サイズがデカかったのでscaleで0.165倍の大きさで返してる
(define (cardhyouji x) 
   (cond ((equal? (car x) 13) (scale 0.165 tr13))
         ((equal? (car x) 12) (scale 0.165 tr12))
         ((equal? (car x) 11) (scale 0.165 tr11))
         ((equal? (car x) 10) (scale 0.165 tr10))
         ((equal? (car x) 9) (scale 0.165 tr9))
         ((equal? (car x) 8) (scale 0.165 tr8))
         ((equal? (car x) 7) (scale 0.165 tr7))
         ((equal? (car x) 6) (scale 0.165 tr6))
         ((equal? (car x) 5) (scale 0.165 tr5))
         ((equal? (car x) 4) (scale 0.165 tr4))
         ((equal? (car x) 3) (scale 0.165 tr3))
         ((equal? (car x) 2) (scale 0.165 tr2))
         ((equal? (car x) 1) (scale 0.165 tr1))
         (else tou)
         ))


(define (draw-scene x);世界の表示
  (define main

    (place-image
     (text "プレイヤー" 35 "sky blue")
     100 150


      (place-image
       (line 800 440 "white")
       400 220

       (place-image
        (my-bet (sekai-bet x))
        650 480

        (place-image
         (money (sekai-me-money x))
         650 560

        (place-image
         (hyouji-me-goukei (sekai-me-goukei x))
         390 520
         
          (place-image
           hyouji-setumei
           140 520

           (place-image
            (display-me-card (car x) 1)
            400 300
             
            midori
            ))))))))
  (cond ((and (< (sekai-judge x) 2) (>= (sekai-judge x) 1))
      (place-image
       hyouji-setumei-gamen
       400 300
       main))
        
        ((= (sekai-judge x) 11)
         (place-image
          hyouji-gameover
          400 300
          main))
        
        ((= (sekai-judge x) 2)
         (place-image
          (hyouji-win? x)
           400 240

         (place-image
          (hyouji-win?-big x)
          405 245

         (place-image
          (rectangle 250 100 "solid" (color 250 250 200 200))
          407 247 
          
         (place-image
          (display-enemy-card (sekai-en-card x) 1)
          400 300
          
          (place-image
           (text (format "ディーラー\n合計： ~a" (sekai-en-goukei x)) 35 "orange")
           670 270

           main))))))
        
        ((= (sekai-judge x) 10)
         (puzzle-scene x))
        
        ((= (sekai-judge x) 0)
         (place-image
          (text (format "ディーラー\n合計： ~a" 0) 35 "orange")
          670 270

          (place-image
           (display-enemy-card-ura (sekai-en-card x) 1)
           400 300
           main)))
        
         ((= (sekai-judge x) 3)
         (place-image
          (text (format "ディーラー\n合計： ~a" 0) 35 "orange")
           670 270

             (place-image
           (display-enemy-card-ura (sekai-en-card x) 1)
           400 300
           main)))
      (else main))
        
  )

  
;世界(world)のリスト
(define (sekai
         my-card     ;引いたカード
         counter     ;何回引いたか
         key         ;押されたキー
         me-goukei   ;自分の合計
         en-goukei   ;相手の合計
         result      ;結果
         bet         ;ベット数
         me-money    ;所持金
         en-card     ;相手のカード
         judge       ;判定
         yo1
         yo2
         )
  (list my-card counter key me-goukei en-goukei result bet me-money en-card judge yo1 yo2)
  )

;;世界を変えるわかりやすい
(define (sekai-my-card x) (first x))
(define (sekai-counter x) (second x))
(define (sekai-key x) (third x))
(define (sekai-me-goukei x) (fourth x))
(define (sekai-en-goukei x) (fifth x))
(define (sekai-result x) (sixth x))
(define (sekai-bet x) (seventh x))
(define (sekai-me-money x) (eighth x))
(define (sekai-en-card x) (ninth x))
(define (sekai-judge x) (tenth x))
(define (sekai-yo1 x) (caddr (cddddr (cddddr x))))
(define (sekai-yo2 x) (last x))


;キー操作
(define (control x k)
  (define random-x (random 1 14))
  (define enemy (append (make-en-card (list) 0 4) (list 0)))
  (define enemy-sum (en-sum enemy 0))

  (if (= (sekai-judge x) 10)
      (cond ((string=? k "\r") (if (same-list? (sekai-yo2 x) seikairisuto)
                                   (sekai
                                    (list 0 0 0 0 0 0)
                                    0
                                    "p"
                                    (list 0 0)
                                    (en-sum syoki-ene-card 0)
                                    0 ;負け
                                    50
                                    500
                                    syoki-ene-card
                                    0
                                    0
                                    0)
                                   x))
            ((string=? k "o") (sekai;パズルを正解ひとつ前にする操作op
                               (sekai-my-card x)
                               (sekai-counter x)
                               (sekai-key x)
                               (sekai-me-goukei x)
                               (sekai-en-goukei x)
                               (sekai-result x)
                               (sekai-bet x)
                               (sekai-me-money x)
                               (sekai-en-card x)
                               (sekai-judge x)
                               (sekai-yo1 x)
                               '(1 2 3 4 5 6 7 0 8)
                               ))
            (else x))
  (cond ((string=? k " ") (if (= (sekai-counter x) 5)
                              x
                              (cond ((or (= (sekai-judge x) 3) (= (sekai-judge x) 0))
                              (sekai
                               (cardireru (sekai-my-card x) random-x)
                               (+ (sekai-counter x) 1)
                               " "
                               (list (+ (car (sekai-me-goukei x)) (over10? random-x)) (+(eleven-goukei (sekai-my-card x)) (one? (over10? random-x))))
                               (sekai-en-goukei x)
                               (sekai-result x)
                               (sekai-bet x)
                               (sekai-me-money x)
                               (sekai-en-card x)
                               3
                               (sekai-yo1 x)
                               (sekai-yo2 x)
                               ))
                              ((= (sekai-judge x) 2) x)
                              (else x)
                              )))
  
        ((string=? k "r") (sekai (list 0 0 0 0 0 0);ブラックジャックを初期状態に戻すop
                                  0
                                  "p"
                                  (list 0 0)
                                  (en-sum syoki-ene-card 0)
                                  0 ;負け
                                  50
                                  500
                                  syoki-ene-card
                                  0
                                  0
                                  0
                                  ))
        
        ((string=? k "escape") (sekai
                                (sekai-my-card x)
                                (sekai-counter x)
                                "escape"
                                (sekai-me-goukei x)
                                (sekai-en-goukei x)
                                (sekai-result x)
                                (sekai-bet x)
                                (sekai-me-money x)
                                (sekai-en-card x)
                                (cond ((= (sekai-judge x) 0) 1)
                                      ((= (sekai-judge x) 2) 1.2)
                                      ((= (sekai-judge x) 3) 1.3)
                                      ((= (sekai-judge x) 1) 0)
                                      ((= (sekai-judge x) 1.2) 2)
                                      ((= (sekai-judge x) 1.3) 3)
                                      )
                                (sekai-yo1 x)
                                (sekai-yo2 x)
                                ))
        
        ((string=? k "\r") (cond ((= (sekai-judge x) 2);win loseのとき
                                  (sekai
                                   (list 0 0 0 0 0 0)
                                   0
                                   "\r"
                                   (list 0 0)
                                   enemy-sum
                                   0
                                   50
                                   (money-change x)
                                   enemy
                                   (if (<= (money-change x) 0) 11 0)
                                   (sekai-yo1 x)
                                   (sekai-yo2 (syokirisuto x))
                                   ))
                                 ((= (sekai-judge x) 11);ゲームオーバーの時
                                  (sekai
                                   (sekai-my-card x)
                                   (sekai-counter x)
                                   "\r"
                                   (sekai-me-goukei x)
                                   (sekai-en-goukei x)
                                   (result1 enemy-sum (sekai-me-goukei x))
                                   (sekai-bet x)
                                   (sekai-me-money x)
                                   (sekai-en-card x)
                                   10
                                   (sekai-yo1 x)
                                   (sekai-yo2 x)
                                   ))
                                 ((and (< (sekai-judge x) 2) (>= (sekai-judge x) 1));説明画面
                                  x)
                                 (else
                                  (sekai
                                   (sekai-my-card x)
                                   (sekai-counter x)
                                   "\r"
                                   (sekai-me-goukei x)
                                   (if (= (sekai-en-goukei x) 0) enemy-sum (sekai-en-goukei x))
                                   (result1 (sekai-en-goukei x) (sekai-me-goukei x))
                                   (sekai-bet x)
                                   (sekai-me-money x)
                                   (if (= (car (sekai-en-card x)) 0) enemy (sekai-en-card x))
                                   2
                                   (sekai-yo1 x)
                                   (sekai-yo2 x)
                                   ))
                                 ))
        
        ((string=? k "up")  (if (= (sekai-judge x) 3)
                                x
                                (if (= (sekai-bet x) (sekai-me-money x))
                                    x
                                    (sekai
                                     (sekai-my-card x)
                                     (sekai-counter x)
                                     "up"
                                     (sekai-me-goukei x)
                                     (sekai-en-goukei x)
                                     (sekai-result x)
                                     (+ (sekai-bet x) 50)
                                     (sekai-me-money x)
                                     (sekai-en-card x)
                                     (sekai-judge x)
                                     (sekai-yo1 x)
                                     (sekai-yo2 x)
                                     ))))
        ((string=? k "down")  (if (= (sekai-judge x) 3)
                                  x
                                  (if (= (sekai-bet x) 50)
                                      x
                                      (sekai
                                       (sekai-my-card x)
                                       (sekai-counter x)
                                       "down"
                                       (sekai-me-goukei x)
                                       (sekai-en-goukei x)
                                       (sekai-result x)
                                       (- (sekai-bet x) 50)
                                       (sekai-me-money x)
                                       (sekai-en-card x)
                                       (sekai-judge x)
                                       (sekai-yo1 x)
                                       (sekai-yo2 x)
                                       ))))
        (else x)
        )
  )
  )

(define (eleven-goukei x);エースを11にする場合の合計
  (apply + (iti-ju (subete10 x))))

 
(define (subete10 x);11、12、13を10としたリストを返す
  (cond ((null? x) x)
        ((equal? (car x) 11) (cons 10 (subete10 (cdr x))))
        ((equal? (car x) 12) (cons 10 (subete10 (cdr x))))
        ((equal? (car x) 13) (cons 10 (subete10 (cdr x))))
        (else (cons (car x) (subete10 (cdr x))))
        ))

 

(define (iti-ju x);1を11にしたリストを返す
  (cond ((null? x) x)
        ((= (car x) 1) (cons 11 (cdr x)))
        (else (cons (car x) (iti-ju (cdr x))))
        ))

(define (result1 enemy me);勝ちか負けの判定
  (if (> (cadr me) 21)
      (cond ((or (> (car me) 21) (and (<= enemy 21) (> enemy (car me)))) 1)
            ((= (car me) enemy) 0)
            (else 2))
      (cond ((or (> (cadr me) 21) (and (<= enemy 21) (> enemy (cadr me)))) 1)
            ((= (cadr me) enemy) 0)
            (else 2))))

(define (en-sum x amount);敵の合計を返す
  (if (null? x)
      amount
      (en-sum (cdr x)
              (+ amount (over10? (car x))))))  
  
(define (over10? x);10を超えていたら10そうでなければそのまま返す
  (if (>= x 10)
      10
      x))

  (define (one? x);1だったら11そうでなければそのまま返す
    (if (= x 1) 11 x))



(define (make-en-card card-lst sum n);敵が引いたカードのリストを作る
  (define random-1 (random 1 14))
  (define random-2 (random 1 5))
  (define random-3 (random 8 10))
  (define random-4 (random 5 7))
  (cond ((= n 0)
         card-lst)
        ((= sum 0)
         (make-en-card (append card-lst
                               (list random-1))
                       (+ sum (over10? random-1))
                       (- n 1)))
        ((>= sum 17)
         card-lst)
       
        (else
         (if (< sum 7)
             (make-en-card (append card-lst
                                (list random-3))
                        (+ sum (over10? random-3))
                        (- n 1))
             (make-en-card (append card-lst
                                   (list random-4))
                           (+ sum (over10? random-4))
                           (- n 1))))
       
        ))

(define syoki-ene-card;最初の敵のカードのリストを作る
 (append (make-en-card (list) 0 4) (list 0)))

(define (cardireru x y);リストの数字が０ならyをいれる。２つ目以降はいれない
  (cond ((null? (cdr x)) x)
        ((= 0 (car x)) (cons y (cdr x)))
        (else (cons (car x) (cardireru (cdr x) y)))
        ))

(define (money-change x);勝敗で所持金が変わる
  (cond ((= (sekai-result x) 0) (sekai-me-money x)) ;;引き分け
        ((= (sekai-result x) 1) (-(sekai-me-money x)(sekai-bet x))) ;;負け
        ((= (sekai-result x) 2) (+(sekai-me-money x)(sekai-bet x))) ;;勝ち
        ))

(define (display-me-card x y);自分のカードを表示するやつ
  (cond ((= 0 (cadr x)) (place-image
                         (cardhyouji x)
                         (me-pointX y) (me-pointY y)
                         tou))
        ((null? (cdr x)) (place-image
                          (cardhyouji x)
                          (me-pointX y) (me-pointY y)
                          tou))
        (else (place-image
               (display-me-card (cdr x) (+ y 1))
               400 300
               (place-image
                (cardhyouji x)
                (me-pointX y) (me-pointY y)
                tou)
               ))
        )
  )

(define (display-enemy-card-ura x y);敵のカード、裏がある場合を表示するやつ
   (cond  ((null? (cdr x)) (place-image
                          tr-ura
                          (enemy-pointX y) (enemy-pointY y)
                          tou))
          ((= 0 (cadr x)) (place-image
                         tr-ura
                         (enemy-pointX y) (enemy-pointY y)
                         tou))
        (else (place-image
               (display-enemy-card-ura (cdr x) (+ y 1))
               400 300
               (place-image
                (if (= y 1) (cardhyouji x) tr-ura)
                (enemy-pointX y) (enemy-pointY y)
                tou)
               ))
        ))

  (define (display-enemy-card x y);敵のカードを表示するやつ、全て表
   (cond  ((null? (cdr x)) (place-image
                          (cardhyouji x)
                          (enemy-pointX y) (enemy-pointY y)
                          tou))
          ((= 0 (cadr x)) (place-image
                         (cardhyouji x)
                         (enemy-pointX y) (enemy-pointY y)
                         tou))
        (else (place-image
               (display-enemy-card (cdr x) (+ y 1))
               400 300
               (place-image
                (cardhyouji x)
                (enemy-pointX y) (enemy-pointY y)
                tou)
               ))
        ))


 
(define (me-pointX y);自分のカードの表示位置X軸
  (cond ((= y 1) (* 80 y))
        ((= y 2) (* 80 y))
        ((= y 3) (* 80 y))
        ((= y 4) (* 80 y))
        (else (* 80 5))
        ))

(define (me-pointY y);自分のカードの表示位置Y軸
  (cond ((= y 1) (+ 260 (* 16 y)))
        ((= y 2) (+ 260 (* 16 y)))
        ((= y 3) (+ 260 (* 16 y)))
        ((= y 4) (+ 260 (* 16 y)))
        (else (+ 260 (* 16 y)))
        ))

(define (enemy-pointX y);敵のカードの表示位置X軸
  (cond ((= y 1) (+ 400 (* 80 y)))
        ((= y 2) (+ 400 (* 80 y)))
        ((= y 3) (+ 400 (* 80 y)))
        ((= y 4) (+ 400 (* 80 y)))
        ((= y 5) (+ 400 (* 80 y)))))
        

(define (enemy-pointY y);敵のカードの表示位置Y軸
  (cond ((= y 1) (+ 90 (* 16 y)))
        ((= y 2) (+ 90 (* 16 y)))
        ((= y 3) (+ 90 (* 16 y)))
        ((= y 4) (+ 90 (* 16 y)))
        (else (+ 90 (* 16 y)))
        ))
;------------------------ゲーム画面-----------------------------------

(define (hyouji-me-goukei x);自分のカードの合計を表示するやつ
  (place-image
    (text (format "1の場合：~a\n11の場合：~a" (car x) (cadr x)) 30 "black")
    110 80
    (rectangle 220 160 "solid" "white")
  ))

(define hyouji-setumei;説明はエスケープキーを表示するやつ
  (place-image (text "説明は\nエスケープキー" 25 "black")
               140 80
               (rectangle 280 160 "solid" "white")))

(define hyouji-setumei-gamen;エスケープキー押した後の説明
  (place-image (text "説明
                      自分の合計を21に近づけろ
                      スペースキーでカードを引く
                      1回Enterキーで勝負
                      2回Enterキーで次のゲームへ
                      エスケープキーで説明を閉じる
                      引ける枚数は最大5枚" 25 "black")
               350 250
               (rectangle 700 500 "solid" "light yellow")))

(define (my-bet bet);ベット数の表示
  (define bet-text
    (text (format "bet (up/down): ~a\n勝ち：×2" bet) 24 "black"))
  (place-image bet-text
               150 40
               (rectangle 300 80 "solid" "white")
               )
  )

(define (money syoji);所持金の表示
  (define kane-text
    (text (format "持ち金: ~a" syoji) 25 "black"))
   (place-image kane-text
               150 40
               (rectangle 300 80 "solid" "white")
               )
  )

(define hyouji-gameover;ゲームオーバーの表示
  (place-image (text "ゲームオーバー！！\n所持金がなくなりました。\n続けるにはエンターキーを押して、\nパズルを解いてください。"
                     30 "red")
               400 100
               (rectangle 800 200 "solid" "lightyellow")))


  (define (hyouji-win? x);win lose draw の表示
    (cond ((= (sekai-result x) 0) (text "Draw" 70 "green"))
          ((= (sekai-result x) 1) (text "Lose..." 70 "blue"))
          ((= (sekai-result x) 2) (text "Win!!" 70 "tomato"))
          ))

 (define (hyouji-win?-big x)
    (cond ((= (sekai-result x) 0) (text "Draw" 70 "black"))
          ((= (sekai-result x) 1) (text "Lose..." 70 "black"))
          ((= (sekai-result x) 2) (text "Win!!" 70 "black"))
          ))


;--------------------------------------------------------------------







(define (s) ;;スタートするやつ
  (big-bang (sekai (list 0 0 0 0 0 0)
                   0
                   "p"
                   (list 0 0)
                   (en-sum syoki-ene-card 0)
                   0 ;負け
                   50
                   500
                   syoki-ene-card
                   0
                   0
                   seikairisuto)
            (to-draw draw-scene)
            (on-key control)
            (on-mouse put-disk)
            ))




;;--------------ここからパズル----------------------------------

(define (get-third-digit n);座標を0~6に変換する
  (modulo (quotient (abs n) 100) 10))

(define (same-list? list1 list2);二つのリストが同じかの判定
  (cond
    ((and (null? list1) (null? list2)) #t)
    ((or (null? list1) (null? list2)) #f)       
    ((equal? (car list1) (car list2))           
     (same-list? (cdr list1) (cdr list2)))
    (else #f)))      


(define (hyouwo y x);マウスの座標を受け取ってどこかの判定
  (cond ((= 0 (quotient (get-third-digit x) 2))
         (cond ((= 0 (quotient (get-third-digit y) 2)) 0)
               ((= 1 (quotient (get-third-digit y) 2)) 1)
               ((= 2 (quotient (get-third-digit y) 2)) 2)
               (else 9)
               ))
        ((= 1 (quotient (get-third-digit x) 2))
         (cond ((= 0 (quotient (get-third-digit y) 2)) 3)
               ((= 1 (quotient (get-third-digit y) 2)) 4)
               ((= 2 (quotient (get-third-digit y) 2)) 5)
               (else 9)
               ))
        ((= 2 (quotient (get-third-digit x) 2))
         (cond ((= 0 (quotient (get-third-digit y) 2)) 6)
               ((= 1 (quotient (get-third-digit y) 2)) 7)
               ((= 2 (quotient (get-third-digit y) 2)) 8)
               (else 9)
               ))
        (else 9)
        ))


;---------------------------押された数字の移動の処理-----------------
(define (koukan w i j)
  (cond ((null? w) w)
        ((= (car w) i) (cons j (koukan (cdr w) i j)))
        ((= (car w) j) (cons i (koukan (cdr w) i j)))
        (else (cons (car w) (koukan (cdr w) i j ))))
  )

(define (kaekae0 w)
  (cond ((= (list-ref w 1) 0) (koukan w (list-ref w 0) 0))
        ((= (list-ref w 3) 0) (koukan w (list-ref w 0) 0))
        (else w)
        ))

(define (kaekae1 w)
  (cond ((= (list-ref w 0) 0) (koukan w (list-ref w 1) 0))
        ((= (list-ref w 2) 0) (koukan w (list-ref w 1) 0))
        ((= (list-ref w 4) 0) (koukan w (list-ref w 1) 0))
        (else w)
        ))

(define (kaekae2 w)
  (cond ((= (list-ref w 1) 0) (koukan w (list-ref w 2) 0))
        ((= (list-ref w 5) 0) (koukan w (list-ref w 2) 0))
        (else w)
        ))

(define (kaekae3 w)
  (cond ((= (list-ref w 0) 0) (koukan w (list-ref w 3) 0))
        ((= (list-ref w 4) 0) (koukan w (list-ref w 3) 0))
        ((= (list-ref w 6) 0) (koukan w (list-ref w 3) 0))
        (else w)
        ))

(define (kaekae4 w)
  (cond ((= (list-ref w 1) 0) (koukan w (list-ref w 4) 0))
        ((= (list-ref w 3) 0) (koukan w (list-ref w 4) 0))
        ((= (list-ref w 5) 0) (koukan w (list-ref w 4) 0))
        ((= (list-ref w 7) 0) (koukan w (list-ref w 4) 0))
        (else w)
        ))

(define (kaekae5 w)
  (cond ((= (list-ref w 2) 0) (koukan w (list-ref w 5) 0))
        ((= (list-ref w 4) 0) (koukan w (list-ref w 5) 0))
        ((= (list-ref w 8) 0) (koukan w (list-ref w 5) 0))
        (else w)
        ))

(define (kaekae6 w)
  (cond ((= (list-ref w 3) 0) (koukan w (list-ref w 6) 0))
        ((= (list-ref w 7) 0) (koukan w (list-ref w 6) 0))
        (else w)
        ))

(define (kaekae7 w)
  (cond ((= (list-ref w 4) 0) (koukan w (list-ref w 7) 0))
        ((= (list-ref w 6) 0) (koukan w (list-ref w 7) 0))
        ((= (list-ref w 8) 0) (koukan w (list-ref w 7) 0))
        (else w)
        ))


(define (kaekae8 w)
  (cond ((= (list-ref w 5) 0) (koukan w (list-ref w 8) 0))
        ((= (list-ref w 7) 0) (koukan w (list-ref w 8) 0))
        (else w)
        )
  )
;--------------------------------------------------------------

(define (yo2-koukan x y);yo2だけ変えるやつ
  (sekai (sekai-my-card x) (sekai-counter x) (sekai-key x) (sekai-me-goukei x) (sekai-en-goukei x) (sekai-result x) (sekai-bet x) (sekai-me-money x) (sekai-en-card x) (sekai-judge x) (sekai-yo1 x) y))

(define (put-disk w x y kind);マウスの操作処理
      (if (string=? kind "button-down")
          (cond ((= (hyouwo x y) 0) (yo2-koukan w (kaekae0 (sekai-yo2 w))))
                ((= (hyouwo x y) 1) (yo2-koukan w (kaekae1 (sekai-yo2 w))))
                ((= (hyouwo x y) 2) (yo2-koukan w (kaekae2 (sekai-yo2 w))))
                ((= (hyouwo x y) 3) (yo2-koukan w (kaekae3 (sekai-yo2 w))))
                ((= (hyouwo x y) 4) (yo2-koukan w (kaekae4 (sekai-yo2 w))))
                ((= (hyouwo x y) 5) (yo2-koukan w (kaekae5 (sekai-yo2 w))))
                ((= (hyouwo x y) 6) (yo2-koukan w (kaekae6 (sekai-yo2 w))))
                ((= (hyouwo x y) 7) (yo2-koukan w (kaekae7 (sekai-yo2 w))))
                ((= (hyouwo x y) 8) (yo2-koukan w (kaekae8 (sekai-yo2 w))))
                (else w)
                )
          w)
  )

(define (puzzle-scene w);パズルの描画処理
  (define px (sekai-yo2 w))
  (define puzzle-mein
  (place-image
   (sikaku (car px))
   100 100
  (place-image
   (sikaku (cadr px))
   300 100
  (place-image
   (sikaku (caddr px))
   500 100
  (place-image
   (sikaku (cadddr px))
   100 300
  (place-image
   (sikaku (cadddr (cdr px)))
   300 300
  (place-image
   (sikaku (cadddr (cddr px)))
   500 300
  (place-image
   (sikaku (cadddr (cdddr px)))
   100 500
  (place-image
   (sikaku (cadddr (cddddr px)))
   300 500
  (place-image
   (sikaku (car (cddddr (cddddr px))))
   500 500
  (place-image
   puzzle-setsumei
   700 300
  (place-image
   (rectangle 600 600 "solid" "white")
   400 300
   (empty-scene 800 600)
  ))))))))))))
  (if (same-list? px seikairisuto) 
  (place-image
   (text "クリア！！" 60 "black")
   300 300
  (place-image
   (rectangle 600 200 "solid" "lightyellow")
   300 300 
   puzzle-mein))
  puzzle-mein
  ))

(define (sikaku x);数字とその周りの四角の表示
  (place-image
   (text (format "~a" (if (= x 0) " " x)) 30 "black")
   100 100
   (place-image
    (rectangle 195 195 "solid" "white")
    100 100
   (rectangle 200 200 "solid" "blue"))))

(define puzzle-setsumei;パズルの説明表示
  (place-image
   (text "所持金が\n無くなりました。\n\n左のパズルを\n下の画像と\n同じにしてください。\n\nクリアしたら\nエンターキーで\n復活できます。" 20 "black")
   110 200
   (place-image
    (scale 0.25 pz-siki)
    100 500
   (rectangle 200 600 "solid" "white")
   )))

(define (randamurisuto-honto risuto n m);パズルを崩す処理
  (if (= n m) risuto
      (randamurisuto-honto (put-disk risuto
                                     (random 0 600)
                                     (random 0 600)
                                     "button-down")
                           (+ 1 n)
                           m)))



(define seikairisuto '(1 2 3 4 5 6 7 8 0));正解のリスト
(define (syokirisuto x) (randamurisuto-honto (yo2-koukan x seikairisuto) 0 1000));パズルを崩したやつを返す

