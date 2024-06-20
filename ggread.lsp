(setq loop T) 
(while loop
  (setq code (grread T 8))
  (cond
    ((= (car code) 5)     (do_Move))               ;;; mouse move
    ((= (car code) 3)     (do_Left))               ;;; mouse left button
    ((= (car code) 11)    (do_Right))              ;;; mouse right button, right button as return
    ((= (car code) 25)    (do_Right))              ;;; mouse right button, right button as screen menu
    ((equal code '(2 0))  (do_CTRL-@))             ;;; CTRL-@
    ((equal code '(2 1))  (do_CTRL-A))             ;;; CTRL-A 
    ((equal code '(2 2))  (do_F9))                 ;;; CTRL-B or F9
    ((equal code '(2 3))  (do_F12))                ;;; CTRL-C or F12
    ((equal code '(2 4))  (do_F6))                 ;;; CTRL-D or F6
    ((equal code '(2 5))  (do_F5))                 ;;; CTRL-E or F5
    ((equal code '(2 6))  (do_F3))                 ;;; CTRL-F or F3
    ((equal code '(2 7))  (do_F7))                 ;;; CTRL-G or F7
    ((equal code '(2 8))  (do_Back))               ;;; CTRL-H or backspace
    ((equal code '(2 9))  (do_Tab))                ;;; CTRL-I or Tab
    ((equal code '(2 10)) (do_CTRL-J))             ;;; CTRL-J
    ((equal code '(2 11)) (do_CTRL-K))             ;;; CTRL-K
    ((equal code '(2 12)) (do_CTRL-L))             ;;; CTRL-L
    ((equal code '(2 13)) (do_Return))             ;;; CTRL-M or return
    ((equal code '(2 14)) (do_CTRL-N))             ;;; CTRL-N
    ((equal code '(2 15)) (do_F8))                 ;;; CTRL-O or F8
    ((equal code '(2 16)) (do_CTRL-P))             ;;; CTRL-P
    ((equal code '(2 17)) (do_CTRL-Q))             ;;; CTRL-Q
    ((equal code '(2 18)) (do_CTRL-R))             ;;; CTRL-R
    ((equal code '(2 19)) (do_CTRL-S))             ;;; CTRL-S
    ((equal code '(2 20)) (do_F4))                 ;;; CTRL-T or F4
    ((equal code '(2 21)) (do_F10))                ;;; CTRL-U or F10
    ((equal code '(2 22)) (do_CTRL-V))             ;;; CTRL-V
    ((equal code '(2 23)) (do_F11))                ;;; CTRL-W or F11
    ((equal code '(2 24)) (do_CTRL-X))             ;;; CTRL-X
    ((equal code '(2 25)) (do_CTRL-Y))             ;;; CTRL-Y
    ((equal code '(2 26)) (do_CTRL-Z))             ;;; CTRL-Z
    ((equal code '(2 27)) (do_CTRL-[))             ;;; CTRL-[ or ESC
    ((equal code '(2 28)) (do_CTRL-\))             ;;; CTRL-\
    ((equal code '(2 29)) (do_CTRL-]))             ;;; CTRL-]
    ((equal code '(2 30)) (do_CTRL-^))             ;;; CTRL-^
    ((equal code '(2 31)) (do_CTRL-_))             ;;; CTRL-_
    ((equal code '(2 32)) (do_Space))              ;;; space key
    ((equal code '(2 33)) (do_ExclamationMark))    ;;; ! key
    ((equal code '(2 34)) (do_DoubleQuote))        ;;; " key
    ((equal code '(2 35)) (do_Hash))               ;;; # key
    ((equal code '(2 36)) (do_Dollar))             ;;; $ key
    ((equal code '(2 37)) (do_Percent))            ;;; % key
    ((equal code '(2 38)) (do_Ampersand))          ;;; & key
    ((equal code '(2 39)) (do_Apostrophe))         ;;; ' key
    ((equal code '(2 40)) (do_OpenParenthesis))    ;;;  ( key
    ((equal code '(2 41)) (do_CloseParenthesis))   ;;; ) key
    ((equal code '(2 42)) (do_Asterisk))           ;;; * key
    ((equal code '(2 43)) (do_Plus))               ;;; + key
    ((equal code '(2 44)) (do_Comma))              ;;; , key
    ((equal code '(2 45)) (do_Minus))              ;;; - key
    ((equal code '(2 46)) (do_Dot))                ;;; . key
    ((equal code '(2 47)) (do_Slash))              ;;; / key
    ((equal code '(2 48)) (do_0))                  ;;; 0 key
    ((equal code '(2 49)) (do_1))                  ;;; 1 key
    ((equal code '(2 50)) (do_2))                  ;;; 2 key
    ((equal code '(2 51)) (do_3))                  ;;; 3 key
    ((equal code '(2 52)) (do_4))                  ;;; 4 key
    ((equal code '(2 53)) (do_5))                  ;;; 5 key
    ((equal code '(2 54)) (do_6))                  ;;; 6 key
    ((equal code '(2 55)) (do_7))                  ;;; 7 key
    ((equal code '(2 56)) (do_8))                  ;;; 8 key
    ((equal code '(2 57)) (do_9))                  ;;; 9 key
    ((equal code '(2 58)) (do_Colon))              ;;; : key
    ((equal code '(2 59)) (do_Semicolon))          ;;; ; key
    ((equal code '(2 60)) (do_LessThan))           ;;; < key
    ((equal code '(2 61)) (do_Equals))             ;;; = key
    ((equal code '(2 62)) (do_GreatThan))          ;;; > key
    ((equal code '(2 63)) (do_QuestionMark))       ;;; ? key
    ((equal code '(2 64)) (do_At))                 ;;; @ key
    ((equal code '(2 65)) (do_A))                  ;;; A key
    ((equal code '(2 66)) (do_B))                  ;;; B key
    ((equal code '(2 67)) (do_C))                  ;;; C key
    ((equal code '(2 68)) (do_D))                  ;;; D key
    ((equal code '(2 69)) (do_E))                  ;;; E key
    ((equal code '(2 70)) (do_F))                  ;;; F key
    ((equal code '(2 71)) (do_G))                  ;;; G key
    ((equal code '(2 72)) (do_H))                  ;;; H key
    ((equal code '(2 73)) (do_I))                  ;;; I key
    ((equal code '(2 74)) (do_J))                  ;;; J key
    ((equal code '(2 75)) (do_K))                  ;;; K key
    ((equal code '(2 76)) (do_L))                  ;;; L key
    ((equal code '(2 77)) (do_M))                  ;;; M key
    ((equal code '(2 78)) (do_N))                  ;;; N key
    ((equal code '(2 79)) (do_O))                  ;;; O key
    ((equal code '(2 80)) (do_P))                  ;;; P key
    ((equal code '(2 81)) (do_Q))                  ;;; Q key
    ((equal code '(2 82)) (do_R))                  ;;; R key
    ((equal code '(2 83)) (do_S))                  ;;; S key
    ((equal code '(2 84)) (do_T))                  ;;; T key
    ((equal code '(2 85)) (do_U))                  ;;; U key
    ((equal code '(2 86)) (do_V))                  ;;; V key
    ((equal code '(2 87)) (do_W))                  ;;; W key
    ((equal code '(2 88)) (do_X))                  ;;; X key
    ((equal code '(2 89)) (do_Y))                  ;;; Y key
    ((equal code '(2 90)) (do_Z))                  ;;; Z key
    ((equal code '(2 91)) (do_OpenSquareBracket))  ;;; [ key
    ((equal code '(2 92)) (do_BackSlash))          ;;; \ key
    ((equal code '(2 93)) (do_CloseSquareBracket)) ;;; ] key
    ((equal code '(2 94)) (do_Caret))              ;;; ^ key
    ((equal code '(2 95)) (do_UnderScore))         ;;; _ key
    ((equal code '(2 96)) (do_BackQuote))          ;;; ` key
    ((equal code '(2 97)) (do_a))                  ;;; a key
    ((equal code '(2 98)) (do_b))                  ;;; b key
    ((equal code '(2 99)) (do_c))                  ;;; c key
    ((equal code '(2 100))(do_d))                  ;;; d key
    ((equal code '(2 101))(do_e))                  ;;; e key
    ((equal code '(2 102))(do_f))                  ;;; f key
    ((equal code '(2 103))(do_g))                  ;;; g key
    ((equal code '(2 104))(do_h))                  ;;; h key
    ((equal code '(2 105))(do_i))                  ;;; i key
    ((equal code '(2 106))(do_j))                  ;;; j key
    ((equal code '(2 107))(do_k))                  ;;; k key
    ((equal code '(2 108))(do_l))                  ;;; l key
    ((equal code '(2 109))(do_m))                  ;;; m key
    ((equal code '(2 110))(do_n))                  ;;; n key
    ((equal code '(2 111))(do_o))                  ;;; o key
    ((equal code '(2 112))(do_p))                  ;;; p key
    ((equal code '(2 113))(do_q))                  ;;; q key
    ((equal code '(2 114))(do_r))                  ;;; r key
    ((equal code '(2 115))(do_s))                  ;;; s key
    ((equal code '(2 116))(do_t))                  ;;; t key
    ((equal code '(2 117))(do_u))                  ;;; u key
    ((equal code '(2 118))(do_v))                  ;;; v key
    ((equal code '(2 119))(do_w))                  ;;; w key
    ((equal code '(2 120))(do_x))                  ;;; x key
    ((equal code '(2 121))(do_y))                  ;;; y key
    ((equal code '(2 122))(do_z))                  ;;; z key
    ((equal code '(2 123))(do_OpenBrace))          ;;; { key
    ((equal code '(2 124))(do_VerticalBar))        ;;; | key
    ((equal code '(2 125))(do_CloseBrace))         ;;; } key
    ((equal code '(2 126))(do_Tilde))              ;;; ~ key
    ((equal code '(2 127))(do_Delete))             ;;; Delete key
 )
)