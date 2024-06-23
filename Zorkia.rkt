#lang racket
(require csc151)
(require csc151/main)
(require csc151/rex)

; +-------------+----------------------------------------------------
; | High Scores |
; +-------------+

;;; Creates a `high-scores.txt` file to keep track of high scores, if it does not exist already. 
(when (not (file-exists? "high-scores.txt"))
  (string->file "" "high-scores.txt"))

; +------+-----------------------------------------------------------
; | Name |
; +------+

;;; Player name
(define name "")

;;; Getting player name
(define get-name
  (lambda ()
    (letrec ([helper
              (lambda (str)
                (if (non-empty-string? str)
                    str
                    (helper (get-input "Looks like you like Blank Space. You know how it goes, right?\nBut I've got a blank space, baby\nAnd I'll write [your name]"))))])
      (helper (get-input "Please tell me your name!")))))

; +-----------+------------------------------------------------------
; | Get Input |
; +-----------+

;;; (get-input prompt) -> string?
;;; prompt: any?
;;; Displays `prompt` and returns user input, removing unnecessary spaces.
(define get-input
  (lambda (prompt)
    (display prompt)
    (newline)
    (string-normalize-spaces (read-line))))

; +--------------------+---------------------------------------------
; | Position, Movement |
; +--------------------+

;;; Player position
(define pos
  (make-list 2 0))

;;; (pos-x) -> exact-integer?
;;; Returns x-coordinate of `pos`.
(define pos-x
  (lambda ()
    (car pos)))

;;; (pos-y) -> exact-integer?
;;; Returns y-coordinate of `pos`.
(define pos-y
  (lambda ()
    (cadr pos)))

;;; Move counter
(define moves 0)

;;; (pos-generate x y) -> list?
;;; x: number?
;;; y: number?
;;; Generates a new position by increasing `pos`'s x-coordinate by `x` and y-coordinate by `y`.
(define pos-generate
  (lambda (x y)
    (list (+ x (pos-x)) (+ y (pos-y)))))
  
;;; (pos-change x y) -> void?
;;; Changes `pos` by increasing x-coordinate by `x` and y-coordinate by `y`.
;;; Increases move counter by 1. 
(define pos-change
  (lambda (x y)
    (set! pos (pos-generate x y))
    (set! moves (+ 1 moves))))

;;; (mover) -> void?
;;; Changes position and increases move counter when given a correct cardinal input ("n", "s", "e", or "w").
;;; If the input is not correct, requires another attempt.
(define mover
  (lambda ()
    (letrec [(mover-helper
              (lambda (str)
                (cond
                  [(string-ci=? str "n")
                   (pos-change 0 1)]
                  [(string-ci=? str "s")
                   (pos-change 0 -1)]
                  [(string-ci=? str "e")
                   (pos-change 1 0)]
                  [(string-ci=? str "w")
                   (pos-change -1 0)]
                  [else
                   (mover-helper (get-input "~ Enter [n], [s], [e], or [w] to go north, south, east, or west ~"))])))]
      (mover-helper (get-input "What direction would you like to go?\n~ Enter [n], [s], [e], or [w] to go north, south, east, or west ~")))))

; +----------------------+-------------------------------------------
; | Tiles - Fundamentals |
; +----------------------+

;;; Creates tile-types` hash.
(define tile-types
  (make-hash (list (cons '("field"
                           (event-tile "field")
                           #f
                           "You see a grassy field."
                           "You step into a field of rich green grass.\nThere are bright flowers as far as the eye can see.\nIt’s a relatively peaceful place, but you’re still uneasy.\nIn this place nothing is that simple.")
                         '(1/2))
                   (cons '("woods"
                           (event-tile "woods")
                           #f
                           "You see thick woods."
                           "As you cross the tree line, everything darkens.\nThe crowded canopy of trees completely blocks out the sun.\nThe constant sound of rustling trees and wildlife fills your ears, but you can’t shake an expectant stillness.\nYou find yourself holding your breath. Something is nearby, you can sense it.")
                         '(1/10))
                   (cons '("lake"
                           (event-tile "lake")
                           #f
                           "You see a large body of water."
                           "You walk up to the banks of a muddy lake.\nYou can’t see the bottom through all the murk.\nHopefully there’s nothing lurking just beneath the surface...")
                         '(2/10))
                   (cons '("cabin"
                           (event-tile "cabin")
                           #f
                           "You see a silhouette of a cabin in the distance."
                           "You cannot see any lights in the windows, and there is no smoke coming out of the chimney.\nYou knock on the faded wooden door, and it promptly crumbles in front of you.\nYou cautiously step over the threshold into the damp cabin.")
                         '(1/10))
                   (cons '("campaign-tile"
                           campaign-tile-function
                           #f
                           campaign-tile-outer-description
                           "")
                         '(1/10)))))

;;; Creates `tiles` hash.
(define tiles
  (make-hash (list (cons '(0 0)
                         '("field"
                           (event-tile "field")
                           #f
                           "You see a grassy field."
                           "You step into a field of rich green grass.\nThere are bright flowers as far as the eye can see.\nIt’s a relatively peaceful place, but you’re still uneasy.\nIn this place nothing is that simple.")))))

;;; (make-tile pos info) -> void?
;;;   pos : list?
;;;   info : list?, tile info
;;; Creates a new tile in `tiles` hash.
(define make-tile!
  (lambda (pos info)
    (and (not (hash-has-key? tiles pos))
         (hash-set! tiles pos info))))

;;; (tile-update) -> void?
;;; Generates tiles in 4 cardinal directions around `pos`. Puts them to `tiles` hash.
(define tile-update
  (lambda ()
    (for-each (section make-tile! <> (random-tile-hash tile-types))
              (map (section pos-generate <> <>) '(0 0 1 -1) '(1 -1 0 0)))))

;;; (probabilistic-select-helper list rand pos start) -> void?
;;;   list : list?
;;;   rand : inexact-number?
;;;   pos : number?
;;;   start : number?
;;; Iterative helper to probabilistic-select
(define probabilistic-select-helper
  (lambda (list rand pos start)
    (let* ([num (sub1 (length list))]
           [x (list-ref list pos)]
           [end (+ start (car x))])
      (if (= pos num)
          (cdr x)
          (if (<= start rand end)
              (cdr x)
              (probabilistic-select-helper list rand (+ 1 pos) end))))))

;;; (probabilistic-select probability-table) -> void?
;;; probability-table: hash-table?
;;; chooses a word based on probabilities
(define random-tile-hash
  (lambda (probability-table)
    (let* ((rand (random))
           (hash-key-list (hash-keys probability-table))
           (probability-list (map (lambda (word) (cons (car (hash-ref probability-table word)) word)) hash-key-list))
           (list-sorted (sort probability-list #:key car <)))
      (probabilistic-select-helper list-sorted rand 0 0))))

;;; (create-string val) -> either string? or void?
;;; val: any?
;;; Displays corresponding event of `val`. If `val` is a string, returns `val`. 
(define create-string
  (lambda (val)
    (if (string? val)
        val
        (cond [(equal? val 'campaign-tile-function)
               (campaign-tile-function)]
              [(equal? val 'campaign-tile-outer-description)
               (campaign-tile-outer-description)]
              [(equal? val '(event-tile "field"))
               (event-tile "field")]
              [(equal? val '(event-tile "woods"))
               (event-tile "woods")]
              [(equal? val '(event-tile "lake"))
               (event-tile "lake")]
              [(equal? val '(event-tile "cabin"))
               (event-tile "cabin")]))))

;;; (look-around pos) -> void?
;;; pos : pair?
;;; Displays a description of tiles in cardinal directions
(define look-around
  (lambda ()
    (display (string-append "\nNorth: "
                            (create-string (fourth (hash-ref tiles (pos-generate 0 1))))
                            "\nSouth: "
                            (create-string (fourth (hash-ref tiles (pos-generate 0 -1))))
                            "\nEast: "
                            (create-string (fourth (hash-ref tiles (pos-generate 1 0))))
                            "\nWest: "
                            (create-string (fourth (hash-ref tiles (pos-generate -1 0))))
                            "\n\n"))))

;;; (run-tile-function) -> void?
;;; Displays the outcome when moving to a new tile.
(define run-tile-function
  (lambda ()
    (display (create-string (fifth (hash-ref tiles pos))))
    (newline)
    (newline)
    (create-string (second (hash-ref tiles pos)))
    ""))

;;; (event-tile tile-type) -> void?
;;; Displays the random event of given `tile-type`.
(define event-tile
  (lambda (tile-type)
    (let ([chosen-event-value (random 20)])
      (cond [(<= chosen-event-value 4)
             (unique-event)]
            [(<= chosen-event-value 8)
             (bad-event)]
            [(<= chosen-event-value 10)
             (shop)]
            [(<= chosen-event-value 12)
             (fight-normal-monster)]
            [else
             (good-event)]))))

; +--------------+---------------------------------------------------
; | Tiles - Good |
; +--------------+

;;; (good-event) -> void?
;;; Displays the good event of the given tile.
(define good-event
  (lambda ()
    (cond
      [(equal? (car (hash-ref tiles pos)) "woods")
       (woods-good-event)]
      [(equal? (car (hash-ref tiles pos)) "lake")
       (lake-good-event)]
      [(equal? (car (hash-ref tiles pos)) "field")
       (field-good-event)]
      [(equal? (car (hash-ref tiles pos)) "cabin")
       (cabin-good-event)])))

;;; (woods-good-event) -> void?
;;; Display the woods good event.
(define woods-good-event
  (lambda ()
    (let ([HP-chosen (random 10 50)])
      (begin (display (string-append "Suddenly a dough deer bursts from the trees.\nTheir meat is well known to have healing properties, and, acting quickly,\nyou manage to throw your sword into its side as it tries to run.\nYou’re feasting well tonight! You heal "
                                     (number->string HP-chosen)
                                     " HP!"))
             (stats-change HP-chosen 0 0)))))

;;; (lake-good-event) -> void?
;;; Displays the lake good event.
(define lake-good-event
  (lambda ()
    (begin (display (string-append "You see a faint glow from the middle of the lake, and a beautiful maenad bursts from the water\nYou kneel in reverence to the divine spirit, and you feel a calming magic reinvigorate your limbs.\nThe blessing of the maenad fully restores your HP!"))
           (stats-change (- 100 (HP)) 0 0))))

;;; (field-good-event) -> void?
;;; Displays the field good event.
(define field-good-event
  (lambda ()
    (let ([HP-chosen (random 1 5)])
      (begin (display (string-append "The field is full of flowers and dandelions.\nAs is customary for your people you pluck a dandelion and blow off the seeds, making a wish.\nYou feel refreshed! Seems the gods are on your side today!\nYou restore "
                                     (number->string HP-chosen)
                                     " HP!"))
             (stats-change HP-chosen 0 0)))))

;;; (cabin-good-event) -> void?
;;; Displays a cabin good event.
(define cabin-good-event
  (lambda ()
    (let ([HP-chosen (random 20 30)])
      (begin (display (string-append "Inside the dim house there is a dining table overflowing with food.\nNo one seems to be home, and the charred husk of a body on the floor seems to imply no one will be coming home anytime soon...\nYou completely stuff yourself, restoring "
                                     (number->string HP-chosen)
                                     " HP!"))
             (stats-change HP-chosen 0 0)))))

; +-------------+----------------------------------------------------
; | Tiles - Bad |
; +-------------+

;;; (bad-event) -> void?
;;; Displays the bad event of the given tile.
(define bad-event
  (lambda ()
    (cond
      [(equal? (car (hash-ref tiles pos)) "woods")
       (woods-bad-event)]
      [(equal? (car (hash-ref tiles pos)) "lake")
       (lake-bad-event)]
      [(equal? (car (hash-ref tiles pos)) "field")
       (field-bad-event)]
      [(equal? (car (hash-ref tiles pos)) "cabin")
       (cabin-bad-event)])))

;;; (woods-bad-event) -> void?
;;; Displays the woods bad event.
(define woods-bad-event
  (lambda ()
    (let ([HP-chosen (random 10 12)])
      (begin (display (string-append "You carefully pick your way through the trees, but suddenly, your foot catches on something.\nYou fall face first into a sticky white web.\nYou hear a deafening scuttling behind you, but the web is too strong for you to see what it is.\nYou feel a million tiny legs crawling up and down your flesh, biting every inch of exposed skin.\nIn one last desperate attempt, you manage to to flail your sword into the web, cutting your arm free.\nYou free yourself as fast as possible, but the spiders have done their damage.\nYou lost "
                                     (number->string HP-chosen)
                                     " HP."))
             (stats-change (- HP-chosen) 0 0)))))

;;; (lake-bad-event) -> void?
;;; Displays the lake bad event.
(define lake-bad-event
  (lambda ()
    (let ([HP-chosen (random 20 30)])
      (begin (display (string-append "You’ve been travelling for quite a while, with no supplies except your sword.\nAlthough you can barely see to its bottom, you decide to take the plunge and quench your thirst.\nMistake.\nIt doesn’t go down like water, more like a stew of fantastical and, unfortunately, magical aquatic parasites.\nThe parasites of the lake drain "
                                     (number->string HP-chosen)
                                     " HP."))
             (stats-change (- HP-chosen) 0 0)))))

;;; (field-bad-event) -> void?
;;; Displays the field bad event.
(define field-bad-event
  (lambda ()
    (begin (display  "This field is so sunny and beautiful,\nyou decide to pause your dragon-hunting journey and take a nap.\nYou wake up a few hours later somehow even more tired than when you fell asleep.\nAs you pull yourself to your feet, you feel the blades of grass you were resting on pull out from your back.\nTypical. Carnivorous grass.\nThat sapped half your hp.")
           (stats-change (- (/ (HP) 2)) 0 0))))

;;; (cabin-bad-event) -> void?
;;; Displays the bad cabin event.
(define cabin-bad-event
  (lambda ()
    (begin (display (string-append "As the door creaks open, you feel a wave of anxiety.\nYou wonder why this seemingly normal abode was left abandoned.\nThat’s when you start to hear the voices. Scratchy, hissing, terrible voices.\nYou break into a cold sweat and turn to escape out the door.\nBut the door is gone. Panicked you try to think of something as the voices close in.\nBut it’s no use. You squeeze your eyes shut, screaming in tandem with the angry spirits of the cabin.\nAt last, you open your eyes to find yourself alone in an empty field, no cabin in sight.\nYou feel horrible. You lose 35 HP!"))
           (stats-change -35 0 0))))

; +----------------+-------------------------------------------------
; | Tiles - Unique |
; +----------------+

;;; (unique-event) -> void?
;;; Displays the unique event of the given tile.
(define unique-event
  (lambda ()
    (cond
      [(equal? (car (hash-ref tiles pos)) "woods")
       (woods-unique-event)]
      [(equal? (car (hash-ref tiles pos)) "lake")
       (lake-unique-event)]
      [(equal? (car (hash-ref tiles pos)) "field")
       (field-unique-event)]
      [(equal? (car (hash-ref tiles pos)) "cabin")
       (cabin-unique-event)])))

;;; (response? prompt) -> boolean?
;;; prompt: any?
;;; Takes a prompt and returns user input. 
(define response?
  (lambda (prompt)
    (let ([input (get-input prompt)])
      (cond
        [(string-ci=? input "yes")
         #t]
        [(string-ci=? input "no")
         #f]
        [else
         (response? "Come on, [yes] or [no]. Don't try to be smart.")]))))

;;; (woods-unique-event) -> void?
;;; Displays the unique woods event.
(define woods-unique-event
  (lambda ()
    (cond
      [(not (response? "You come to a small clearing.\nThere’s a large tree lush in vibrant, differently colored fruits.\nThey don’t look too dangerous.\nProbably.\nDo you want to eat a fruit?"))
       (display "Better safe than sorry.\nYou walk right past the tree and off into the woods with a pit of regret in your empty stomach.")]
      [(response? "You step up to the tree and look over your options.\nThere are a few fruit close to the ground, but the ripest seem to be up in the highest branches.\nDo you want to risk the climb for those tastier fruit?")
       (if (response? "Very carefully you scale the tree.\nIt’s rough going, but surely the fruit at the top will be worth it.\nAt last you reach the branches at the very top.\nThere’s an acrid, green fruit that almost seems to be smoking, and a slightly more appealing glowing, warm, red fruit.\nMaybe you should take the red one?")
           (begin (display "You take a confident bite of the red fruit, and immediately your mouth catches on fire.\nYou spit it out, but in your panic you lose your balance and plummet towards the ground.\nThe fall isn’t too far, and you stagger away from the tree with a burning mouth and a sore back.\nYou lose 30 HP.")
                  (stats-change -30 0 0))
           (begin (display "Cautiously you pluck the green fruit and take a tentative bite.\nIt’s certainly sour, but it’s actually pretty delicious.\nAs you happily eat around the core you feel yourself getting stronger.\nYou drop back to the ground revitalized, and maybe even stronger than before.\nYou gain 75 HP.")
                  (stats-change 75 0 0)))]
      [(response? "You decide not to climb and look closer at the fruits you can reach.\nThere’s a rather sickly looking yellow fruit and an otherworldly purple fruit.\nNeither look particularly appealing, but the yellow one is closer.\nDo you take the yellow fruit?")
       (begin (display "You reluctantly take the yellow fruit and take a bite.\nIt’s a little grainy, but it’s not too bad.\nYou walk away unsatisfied, but unscathed.\nYou gain 5 HP.")
              (stats-change 5 0 0))]
      [else
       (display "Instead you reach for the purple one.\nAs you bite into it, your head swims a little.\nYou blink a few times and shake off the nausea.\nYou drop the half-eaten fruit and walk off.\nYou suppose it could have been worse.")])))
      
;;; (lake-unique-event) -> void?
;;; Displays the lake unique event.
(define lake-unique-event
  (lambda ()
    (cond
      [(not (response? "You see a lot of movement beneath the water and decide it might be a good fishing spot.\nYou take out your sword, cut a vine  next to you.\nThen you sheathe your sword and tie the vine around it, then tie a small berry to the end as bait.\nYou are confident it’ll serve your purposes, and cast the makeshift line into the water.\nYou get a bite, and pull up a puny fish.\nMaybe you could keep it on the line and get a bigger fish.\nDo you want to risk it?"))
       (display "You pull the small fish off the line and pause.\nWhat exactly are you supposed to do with this?\nYou decide you could eat it?\nYou filet it, and manage to burn the pathetic singular bite of meat.\nWell, that wasn’t worth it at all.")]
      [(not (response? "You cast the line back into the water.\nA long, arduous hour later, you finally get another tug on the vine.\nIt doesn’t feel that big, but...\nyou’re already in this deep, might as well let it ride even more right?"))
       (begin (display "No, you’re right, better safe than sorry.\nYou tug up what’s on the line to reveal a modestly sized fish.\nIt’ll make a nice dinner. You start a small cooking fire and roast the fish over it.\nIt’s surprisingly filling. You feel refreshed and revitalized.\nYou gain 25 HP.")
              (stats-change 25 0 0))]
      [(response? "Many, many hours later you finally get another tug.\nIt almost pulls you into the water, but you brace yourself, and begin to pull it in.\nOr... maybe you could... push your luck even further?\nYou see a big ripple in the water coming right for your hook.\nIf you can just hold out a little longer...\nWanna push your luck even further...?")
       (begin (display "As the ripple reaches your line, you feel another, massive tug.\nYou almost lose your only weapon to the murky lake.\nBut you manage to dig in your heels and heave the.. whatever it is, onto land.\nA massive, slimy, venomous serpent flops onto the shore.\nIt hisses at you, and attempts to writhe back into the water.\nDeftly you unwind your scabbard and stab your sword into its skull.\nThe venom from its fangs will make your sword much stronger.\nThe twelve hours you spent standing there were definitely worth it.\nYou gain 10 ATK.")
              (stats-change 0 10 0))]
      [else
       (display "You attempt to pull in your catch, but the ripple gets there first.\nYou strain against the new weight, and the vine snaps.\nYou fall backward, and lie on the ground in regret for a few minutes.\nAt last you get up and walk off.\nTen hours well spent.")])))

;;; (field-unique-event) -> void?
;;; Displays the field unique event.
(define field-unique-event
  (lambda ()       
    (if (string-ci=? (get-input "As you cross the field, a small, green goblin appears in front of you.\nHe lets out a cackle, rubbing his hands together.\nWanna have a little... battle of wits?\nWell, not that you have a say in the matter.\nOhohoho! You’ll never guess it, no one ever has.\nWhat does man love more than life, hate more than death or mortal strife;\nthat which contented men desire; the poor have, the rich require;\nthe miser spends, the spendthrift saves,\nand all men carry to their graves?")
                     "nothing")
        (begin (display "Well done! Well done!\nI thought you were another foolish hero, but you actually have a bit of a brain.\nWell, I’m a goblin of my word. Here, buy yourself something special.\nWith that, the goblin vanishes. Your pockets feel heavy with gold. Your TOKENS are doubled.")
               (stats-change 0 0 (TOKENS)))
        (begin (display "What a disappointment!\nTo be honest I expected more from you.\nAh well, I’ll just take my fee and be on my way. You lose 25 TOKENS.")
               (stats-change 0 0 -25)))))

;;; (cabin-unique-event) -> void?
;;; Displays a cabin unique event.
(define cabin-unique-event
  (lambda ()
    (let ([input (get-input "You step into the dark cabin, there is a staircase that goes to the cellar, there is also a door to the kitchen\nWould you like to go to the cellar or kitchen?\n~ Enter [cellar] or [kitchen] ~")])
      (cond
        [(string-ci=? input "cellar")
         (begin (display "You find lots of yummy food and gain 30 HP.")
                (stats-change 30 0 0))]
        [(string-ci=? input "kitchen")
         (begin (display "You slip on the ground and hit your head. You lose 30 HP.")
                (stats-change -30 0 0))]
        [else
         (display "You couldn't make up your mind. You stumble around in the dark briefly before deciding to leave.")]))))

; +--------------+---------------------------------------------------
; | Tiles - Shop |
; +--------------+

;;; (trade-TOKENS val) -> void?
;;; str: string?
;;; Changes stats by deducting tokens in exchange for gaining HP and/or ATK. Displays the outcome.
(define shop
  (lambda ()
    (letrec [(helper
              (lambda (str)
                (cond
                  [(string-ci=? str "pass")
                   (display "Come back another time!")]
                  [(< (TOKENS) 20)
                   (display "You don't even have 20 TOKENS! Go away, you poor!")]
                  [(string-ci=? str "small HP")
                   (begin (stats-change 10 0 -20)
                          (display "You traded 20 TOKENS for 10 HP."))]
                  [(string-ci=? str "small ATK")
                   (begin (stats-change 0 10 -20)
                          (display "You traded 20 TOKENS for 10 ATK."))]
                  [(string-ci=? str "small mix")
                   (begin (stats-change 5 5 -20)
                          (display "You traded 20 TOKENS for 5 HP and 5 ATK."))]
                  [(< (TOKENS) 35)
                   (helper (get-input "You only have enough TOKENS for a small potion.\n~ Enter [small HP/ATK/mix] or [pass] ~"))]
                  [(string-ci=? str "medium HP")
                   (begin (stats-change 20 0 -35)
                          (display "You traded 35 TOKENS for 20 HP."))]
                  [(string-ci=? str "medium ATK")
                   (begin (stats-change 0 20 -35)
                          (display "You traded 35 TOKENS for 20 ATK."))]
                  [(string-ci=? str "medium mix")
                   (begin (stats-change 10 10 -35)
                          (display "You traded 35 TOKENS for 10 HP and 10 ATK."))]
                  [(< (TOKENS) 50)
                   (helper (get-input "You only have enough TOKENS for a small or medium potion.\n~ Enter [small/medium HP/ATK/mix] or [pass] ~"))]
                  [(string-ci=? str "large HP")
                   (begin (stats-change 30 0 -50)
                          (display "You traded 50 TOKENS for 30 HP."))]
                  [(string-ci=? str "large ATK")
                   (begin (stats-change 0 30 -50)
                          (display "You traded 50 TOKENS for 30 ATK."))]
                  [(string-ci=? str "large mix")
                   (begin (stats-change 15 15 -50)
                          (display "You traded 50 TOKENS for 15 HP and 15 ATK."))]
                  [else
                   (helper (get-input "~ Enter [small/medium/large HP/ATK/mix] or [pass] ~"))])))]
      (helper (get-input "You come across a shop. A boisterous merchant greets you.\nWe got everything you could need: Health, Strength, or maybe a little of both!\nWhy don't you take a look yourself, eh? What do you want to buy?\n------------------------------------------------------------------\n|        |    HP   |    ATK   |         MIX        |     PRICE   |\n------------------------------------------------------------------\n| Small  | + 10 HP | + 10 ATK |  + 5 HP & + 5 ATK  | - 20 TOKENS |\n------------------------------------------------------------------\n| Medium | + 20 HP | + 20 ATK | + 10 HP & + 10 ATK | - 35 TOKENS |\n------------------------------------------------------------------\n| Large  | + 30 HP | + 30 ATK | + 15 HP & + 15 ATK | - 50 TOKENS |\n------------------------------------------------------------------\n~ Enter [small/medium/large HP/ATK/mix] or [pass] ~")))))

; +------------------+-----------------------------------------------
; | Tiles - Campaign |
; +------------------+

;;; campaign-tile-outer-description
(define campaign-tile-outer-description
  (lambda ()
    (cond
      [(= campaign-tracker 0)
       "On the horizon lies the infamous Lake Urodela. Legend tells of a massive serpent lurking in its depths."]
      [(= campaign-tracker 1)
       "There is a crossroads obscured by an ominous fog. You can barely see a figure leaning against a signpost. It’s definitely... unusual."]
      [(= campaign-tracker 2)
       "You see a grassy field."]
      [(= campaign-tracker 3)
       "There’s a small curio stand. A massive sign reads ‘The Magical Menagerie’. You wonder what a random merchant stand is doing in the middle of nowhere."]
      [(= campaign-tracker 4)
       "A massive castle materializes in front of you. This is it. You’re finally at the end."])))

;;; campaign tracker
(define campaign-tracker 0)

;;; (campaign-0) -> void?
;;; The first campaign event, the fight against the serpent.
(define campaign-0
  (lambda ()
    (begin (display "You approach the lake.\nThe surface begins to froth and the ground shakes.\nSuddenly, a massive serpent bursts forth from the lake with a bone chilling scream.\nYou draw your sword.")
           (fight-serpent)
           (set! campaign-tracker (+ campaign-tracker 1)))))

;;; (campaign-1) -> void?
;;; The second campaign event, Janus’ riddle.
(define campaign-1
  (lambda ()
    (display "As you approach the signpost, the fog begins to clear and you get a good look at the figure standing there.\nIt appears to be a two-faced man in a suit and top hat.\nBoth of his faces smile as you approach.\nHis left face begins to speak.\n\"Well friend, I’ve heard you’re looking for the magartifacts.\"\nThe other head chimes in.\n\"There’s one right down one of these paths, but you’ll have to figure out which.\nIt’s simple really. We’ll alternate giving you hints.\"")
    (let ([input (get-input "Left: Me, Left, always tells the truth, while Right tends to lie.\nRight: Hey, I’m the one that always tells the truth, and you always lie.\nLeft: Oh right, I had forgotten. It has been so long, you see. Anyways, it is not down the left path.\nRight: What?! Yes it is! It is down the left path!\nYou pause in confusion.\nLeft: Come on! You can trust me, for I always tell the truth, it is not down the right path.\nRight: Wait, I thought you just said it was down the left path? I was about to agree, for I believe it is down the left path.\nLeft: Oh dear, have we forgotten where the stone is? Right, you are always confusing me with your constant fibbing! Well traveler, perhaps you can help us. Which path is the rock down?")])
      (cond
        [(string-ci=? input "left")
         (begin (display "Both heads speak in unison.\nWrong path traveller! Here we shall repeat ourselves...\n\n")
                (campaign-1))]
        [(string-ci=? input "right")
         (begin (display "Both heads speak in unison.\nWrong path traveller! Here we shall repeat ourselves...\n\n")
                (campaign-1))]
        [(string-ci=? input "neither")
         (begin (display "Oh! That’s right!\nWe kept it in our pocket for safekeeping.\nWe BOTH lie. It has been so long traveller, we must sincerely apologize.\nWell, here it is.\nThe two-faced man smiles and hands you another beautiful stone.\nYou’ve obtained the second Magartifact!")
                (set! campaign-tracker (+ campaign-tracker 1)))]
        [else
         (begin (display "A direction traveller please! Either right or left or something!\nIt has to be around here somewhere.\nHere we shall speak again.\n\n")
                (campaign-1))]))))

;;; (campaign-2) -> void?
;;; The event associated with the third campaign event
(define campaign-2
  (lambda ()
    (begin (display "As you walk across yet another field, something catches your eye.\nIt’s a particularly shiny rock.\nWait a minute.\nIt can’t be.\nIt is.\nYou have stumbled across the third magartifact...!")
           (set! campaign-tracker (+ campaign-tracker 1)))))

;;; (campaign-3) -> void?
;;; The event associated with the fourth campaign event
(define campaign-3
  (lambda ()
    (letrec ([helper
              (lambda (str)
                (cond
                  [(< (TOKENS) 50)
                   (display "You don’t have nearly enough TOKENS fer this beauty.\nCome back when yer pockets aren’t so light?")]
                  [(string-ci=? str "yes")
                   (begin (display "Wowza! Ya actually got the coin fer it!\nWell, here ya go! A deal’s a deal!\nYou’ve obtained the fourth and final magartifact!!")
                          (stats-change 0 0 -50)
                          (set! campaign-tracker (+ campaign-tracker 1)))]
                  [(string-ci=? str "no")
                   (display "You don't know what you're missing!")]
                  [else
                   (helper (get-input "It's either [yes] or [no]. Opportunities don't wait."))]))])
      (helper (get-input "You approach the merchant.\nWhat’re ya lookin fer?\nWell, alls I got right now is this magic rock.\nOh, ya want it?\nWell, sure! Fer the right price o’ course.\nWhattya say to 50 TOKENS?\nIt's either [yes] or [no]. Opportunities don't wait.")))))                

;;; (campaign-4) -> void?
;;; The final confrontation
(define campaign-4
  (lambda ()
    (begin (display "You enter the sinister palace, and make your way down a dark and foreboding hallway.\nAt the end is a dimly lit throne room.\nTo your surprise you see a familiar figure sitting on the throne.\nWell hello there, old friend. How kind of you to bring me back my stones.\nIt’s Explicus MacGuffin, the friendly and memorable barkeep from the beginning of your journey!\nBut he seems different.\nHis eyes are hollow, and he holds a large scythe in his right hand.\nHe stares down at you menacingly.\nNow give me my stones, and I will take my rightful place as emperor of all of Zorkia!\nYou draw your sword; your heart broken with betrayal.\nVery well. Explicus sighs. I suppose I’ll have to destroy you myself.\nIn a flash of horrid green light, Lord Explicus of the fell Macguffin clan, Lich King, Lord of the Underworld, surges from his throne and slashes at your throat.\nWith tears in your eyes you call out to him.\nWAIT! It doesn’t have to be this way.\nHe pauses.\nJust because you are a lich king and want to kill everyone doesn’t mean we have to fight to the death over some dumb rocks.\nDon’t you remember all the good times we had together.\nLike that one time at the carnival, where I won back all of your money from that twisted carny and his scam of a game.\nOr that other time we drank together and sang the Zorkia national anthem on your birthday.\nHe lifts his scythe, but his hand wavers.\nNO! STOP! I am supposed to kill you and take my rightful place as king!\nYou throw aside your sword and raise your arms in supplication. WHY CAN’T WE JUST BE FRIENDS INSTEAD!!!\nHe lets out an agonized scream and collapses.\nYou embrace your best friend.\nBut, in your arms, he crumbles to bony dust.\nNot every story can have a happy ending.")                      
           (set! campaign-tracker (+ campaign-tracker 1)))))

;;; campaign-tile-function
(define campaign-tile-function
  (lambda ()
    (cond
      [(= campaign-tracker 0)
       (campaign-0)]
      [(= campaign-tracker 1)
       (campaign-1)]
      [(= campaign-tracker 2)
       (campaign-2)]
      [(= campaign-tracker 3)
       (campaign-3)]
      [(= campaign-tracker 4)
       (campaign-4)])))

; +-----------------+------------------------------------------------
; | Tiles - Cleared |
; +-----------------+

;;; (cleared-tile tile-type) -> void?
;;; Displays cleared description of the given `tile-type`.
(define cleared-tile
  (lambda (tile-type)
    (cond
      [(equal? (car (hash-ref tiles pos)) "woods")
       (woods-cleared)]
      [(equal? (car (hash-ref tiles pos)) "lake")
       (lake-cleared)]
      [(equal? (car (hash-ref tiles pos)) "field")
       (field-cleared)]
      [(equal? (car (hash-ref tiles pos)) "cabin")
       (cabin-cleared)])))

;;; (woods-cleared) -> void?
;;; Display cleared description of woods.
(define woods-cleared
  (lambda ()
    (display "While before these trees seemed claustrophobic and unnerving, now you are just grateful for the familiar terrain.\nYou’ve already been to this neck of the woods.")))

; (lake-cleared) -> string?
; calls cleared description of lake
(define lake-cleared
  (lambda ()
    (display  "You rest a moment by the lake, pondering the quiet ripples of fish below the surface, before journeying onwards.\nYou have been to this lake before.")))

;;; (field-cleared) -> void?
;;; Displays cleared description of field.
(define field-cleared
  (lambda ()
    (display "The clear air here is refreshing.\nYou take a moment to stop and smell the flowers before continuing on your quest.\nThis field has already been cleared.")))

;;; (cabin-cleared) -> void?
;;; Displays cleared description of cabin.
(define cabin-cleared
  (lambda ()
    (display  "Somehow even this rotting cabin is comforting.\nThis cabin is secure. You’re safe for now.")))

; +-------+----------------------------------------------------------
; | Stats |
; +-------+

;;; Player stats: HP, ATK, TOKENS
(define stats
  (vector 100 100 100 5 50))

;;; (HP) -> exact-nonnegative-integer?, (<= (HP) 100)
;;; Returns player's current HP. Cannot go below 0 or beyond 100. Always rounds down, takes the exact form.
(define HP
  (lambda ()
    (let ([HP (vector-ref stats 0)])
      (cond 
        [(< HP 0)
         0]
        [(> HP 100)
         100]
        [else
         (inexact->exact (floor HP))]))))

;;; (ATK) -> exact-nonnegative-integer? 
;;; Returns player's current ATK. Cannot go below 0. Always rounds down, takes the exact form.
(define ATK
  (lambda ()
    (let ([ATK (vector-ref stats 1)])
      (if (< ATK 0)
          0
          (inexact->exact (floor ATK))))))

;;; (TOKENS) -> exact-nonnegative-integer?
;;; Returns player's current TOKENS.
(define TOKENS
  (lambda ()
    (let ([TOKENS (vector-ref stats 2)])
      (cond
        [(< TOKENS 0)
         0]
        [else
         (inexact->exact (floor TOKENS))]))))

;;; (crit-rate) -> exact-nonnegative-integer?
;;; Returns player's current crit-rate.
(define crit-rate
  (lambda ()
    (let ([crit-rate (vector-ref stats 4)])
      (cond
        [(< crit-rate 0)
         0]
        [(> crit-rate 100)
         100]
        [else
         crit-rate]))))

;;; (crit-damage) -> exact-nonnegative-integer?
;;; Returns player's current crit-rate.
(define crit-damage
  (lambda ()
    (let ([crit-damage (vector-ref stats 4)])
      (if (< crit-damage 0)
          0
          crit-damage))))

;;; (stats-change HP-gained ATK-gained TOKENS-gained) -> void?
;;; HP-gained: number?
;;; ATK-gained: number?
;;; TOKENS-gained: number?
;;; Increases stats by `HP` HP, `ATK` ATK, and `TOKENS` TOKENS.
(define stats-change
  (lambda (HP-gained ATK-gained TOKENS-gained)
    (for-each (section vector-set! stats <> <>)
              '(0 1 2)
              (list (+ (HP) HP-gained)
                    (+ (ATK) ATK-gained)
                    (+ (TOKENS) TOKENS-gained)))))

;;; (stats-description) -> string?
;;; Returns a string describing player's current stats.
(define stats-description
  (lambda ()
    (let* ([magartifacts
            (if (> campaign-tracker 4)
                4
                campaign-tracker)]
           [inner (string-append "| HP: "
                                 (number->string (HP))
                                 " | ATK: "
                                 (number->string (ATK))
                                 " | TOKEN(S): "
                                 (number->string (TOKENS))
                                 " | MOVE(S): "
                                 (number->string moves)
                                 " | MAGARTIFACT(S): "
                                 (number->string magartifacts)
                                 " |")]
           [outer (make-string (string-length inner) #\-)])
      (string-append outer
                     "\n"
                     inner
                     "\n"
                     outer))))

; +---------------+--------------------------------------------------
; | Combat System |
; +---------------+

;;; (fight name-monster HP-min HP-max ATK-min ATK-max) -> void?
;;; name-monster: string?
;;; HP-min: exact-positive-integer?
;;; HP-max: exact-positive-integer?, (> HP-max HP-min)
;;; ATK-min: exact-positive-integer?
;;; ATK-max: exact-positive-integer? (> ATK-max ATK-min)
;;; Displays encountered monster’s stats and the outcome after the fight.
(define fight
  (lambda (name-monster HP-min HP-max ATK-min ATK-max des-after)
    (let* ([HP-monster (random HP-min HP-max)]
           [ATK-monster (random ATK-min ATK-max)]
           [hits-monster-die (ceiling (/ HP-monster (ATK)))]
           [hits-player-die (ceiling (/ (HP) ATK-monster))]
           [HP-lost (* (- hits-monster-die 1) ATK-monster)]
           [TOKENS-gained (floor (* (/ HP-monster (HP)) ATK-monster))]
           [des-before (string-append "You encounter "
                                      name-monster
                                      " with: "
                                      (number->string HP-monster)
                                      " HP, "
                                      (number->string ATK-monster)
                                      " ATK.\nYou fight. ")])
      (if (>= HP-lost (HP))
          (begin (stats-change (- HP-lost) 0 0)
                 (display (string-append des-before
                                         "You die. Your name shall be remembered, "
                                         name
                                         "!")))
          (begin (stats-change (- HP-lost) 0 TOKENS-gained)
                 (display (string-append des-before
                                         "\nYou lose "
                                         (number->string HP-lost)
                                         " HP and gain "
                                         (number->string TOKENS-gained)
                                         " TOKEN(S).\n"
                                         des-after)))))))

;;; (fight-normal-monster) -> void?
;;; Displays the event of a normal monster fight. 
(define fight-normal-monster
  (lambda ()
    (fight "a monster" 1 300 1 20 "The monster die.")))

;;; (fight-serpent) -> void?
;;; Displays the event of serpent campaign fight.
(define fight-serpent
  (lambda ()
    (fight "the PLEURODELINAE, serpent of the lake" 800 1200 2 6 "The serpent collapses to the ground, writhing in agony.\nAt last, it stops moving, and a small glowing rock falls from its mouth.\nAs you pick it up, Explicus’ voice echoes in your mind.\nYou’ve found your first Magartifact!")))

; +------+-----------------------------------------------------------
; | Play |
; +------+

;;; (play) -> void?
;;; Plays the entire game.
(define play
  (lambda ()
    (letrec ([game-loop
              (lambda ()
                (begin (tile-update)
                       (when (> (TOKENS) 500)
                         (begin (display "\n\nYou carry too many TOKENS for your 500 capacity bag. You lose 5 ATK.")
                                (stats-change 0 -5 0)))
                       (display (string-append "\n\n"
                                               (stats-description)
                                               "\n"))
                       (look-around)
                       (mover)
                       (display "Your new position is: ")
                       (display pos)
                       (newline)
                       (newline)
                       (run-tile-function)
                       (let* ([moves-str (number->string moves)]
                              [highest-score
                               (lambda (pos)
                                 (o string->number pos (section rex-find-matches (rex-repeat (rex-char-range #\0 #\9)) <>) car))]
                              [highest-score-HP (highest-score car)]
                              [highest-score-ATK (highest-score cadr)]
                              [highest-score-TOKENS (highest-score third)]
                              [highest-score-moves (highest-score last)]
                              [game-over-description (string-append "\n\nGAME OVER. THANKS FOR PLAYING!\n\n"
                                                                    (stats-description))]
                              [high-score-description (string-append name
                                                                     ": "
                                                                     (number->string (HP))
                                                                     " HP, "
                                                                     (number->string (ATK))
                                                                     " ATK, "
                                                                     (number->string (TOKENS))
                                                                     " TOKEN(S), "
                                                                     moves-str
                                                                     " MOVES.")])
                         (cond
                           [(<= (HP) 0)
                            (display game-over-description)]
                           [(> campaign-tracker 4)
                            (begin (display game-over-description)
                                   (letrec ([high-score-update!
                                             (lambda (lst)
                                               (cond
                                                 [(null? (file->lines "high-scores.txt"))
                                                  (string->file high-score-description "high-scores.txt")]
                                                 [(null? lst)
                                                  (string->file (string-append (file->string "high-scores.txt")
                                                                               "\n"
                                                                               high-score-description)
                                                                "high-scores.txt")]
                                                 [(or (> (HP) (highest-score-HP lst))
                                                      (and (= (HP) (highest-score-HP lst))
                                                           (> (ATK) (highest-score-ATK lst)))
                                                      (and (= (ATK) (highest-score-ATK lst))
                                                           (> (TOKENS) (highest-score-TOKENS lst)))
                                                      (and (= (TOKENS) (highest-score-TOKENS lst))
                                                           (> moves (highest-score-moves lst))))
                                                  (string->file (string-replace (file->string "high-scores.txt")
                                                                                (car lst)
                                                                                (string-append high-score-description
                                                                                               "\n"
                                                                                               (car lst)))
                                                                "high-scores.txt")]
                                                 [else
                                                  (high-score-update! (cdr lst))]))])
                                     (high-score-update! (file->lines "high-scores.txt"))))]
                           [else
                            (game-loop)]))))])
      (begin (set! name (get-name))
             (display (string-append "\nWelcome to Zorkia, "
                                     name
                                     "!

         You open your eyes to a sky filled with smoke.
         Groggily, you sit up and look around to see your hometown in flames.
         As you get your bearings you recognize a bearded man calmly cleaning a mug. He glances over to you then looks back to his mug.
         A gruff voice rumbles out from under his beard.
         Oh, yer finally awake.
         Don’t you recognize me? It’s me, Explicus MacGuffin, yer old friend and barkeep.
         That blasted dragon incinerated the whole town and you just collapsed. From the smoke or the drink I couldn’t tell ya.
         Took out me whole bar too. And all me customers.
         ...
         Ye always were the adventurous type lad. Why don’t cha get out there and kill yerself a dragon.
         Well, o course it’s got one o them magic barriers that prevents anyone from finding its lair, much less kill the beast.
         You’ll need the er... magartifacts to break it.
         Where are they? How should I know? Just wander round. Yer sure to find one of em eventually.
         One last thing. Ye can move around by shouting one o’ the cardinal directions out into the void.
         Y’know: North, South, East, and West.
         Good luck lad. Yer gonna need it."))
             (game-loop)))))
        
        
        
        
        
