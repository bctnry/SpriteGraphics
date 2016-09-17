#lang racket
(require racket/draw)
(require racket/gui)

(define Sprite<%>
  (interface ()
    getAction setAction resetAction
    getReaction setReaction resetReaction
    reflection))
(define IndependentSprite%
  (class* object% (Sprite<%>) (super-new)
    ; time -> bitmap * pos
    (define actionFunc (λ (t) (cons (make-object bitmap% 1 1) (cons 0 0))))
    ; time * key * mouse * env -> (bitmap -> bitmap) * (pos -> pos)
    (define reactionFunc
      (λ (t k e env) (cons (λ (b) b) (λ (p) p))))
    (define/public (getAction) actionFunc)
    (define/public (getReaction) reactionFunc)
    (define/public (resetAction)
      (set! actionFunc (λ (t) (void))))
    (define/public (resetReaction)
      (set! reactionFunc (λ (t k e env) (void))))
    (define/public (setAction t) (set! actionFunc t))
    (define/public (setReaction t) (set! reactionFunc t))
    (define/public (reflection) 'IndependentSprite)))
(define AccompanyingSprite%
  (class* object% (Sprite<%>) (super-new)
    (init [accompanying 'None]) ; any Sprite<%>
    (define/public (reflection) 'AccompanyingSprite)
    (define actionFunc (λ (t) (cons (make-object bitmap% 1 1) (cons 0 0))))
    (define reactionFunc
      (λ (t k e env) (cons (λ (b) b) (λ (p) p))))
    (define/public (getAction) actionFunc)
    (define/public (getReaction) reactionFunc)
    (define/public (resetAction) (set! actionFunc (λ (t) (void))))
    (define/public (resetReaction)
      (set! reactionFunc (λ (t k e env) (void))))
    (define/public (setAction t) (set! actionFunc t))
    (define/public (setReaction t) (set! reactionFunc t))
    (define accompanyingSprite accompanying)
    ; single, dynamic accompanying
    (define/public (getAccompanying) accompanyingSprite)
    (define/public (resetAccompanying) (set! accompanyingSprite 'None))
    (define/public (setAccompanying t) (set! accompanyingSprite t))))

(define (filterSplit f l)
  (letrec ([filterSplit_
            (λ (f l hold1 hold2)
              (if (list? l)
                  (cond ((null? l) (cons (reverse hold1) (reverse hold2)))
                        ((f (car l))
                         (filterSplit_ f (cdr l) (cons (car l) hold1) hold2))
                        (else (filterSplit_ f (cdr l) hold1
                                            (cons (car l) hold2))))
                  (error "Not a list.")))])
    (filterSplit_ f l '() '())))
(define (getRoot t)
  (filter (λ (x)
            (let* ([t0 (cadr x)] [type (send t0 reflection)])
              (or (equal? type 'IndependentSprite)
                  (and (equal? type 'AccompanyingSprite)
                       (equal? 'None (send t0 getAccompanying)))))) t))
(define (removeRoot t)
  (filter-not (λ (x)
            (let* ([t0 (cadr x)] [type (send t0 reflection)])
              (or (equal? type 'IndependentSprite)
                  (and (equal? type 'AccompanyingSprite)
                       (equal? 'None (send t0 getAccompanying)))))) t))
(define (retrDependentTree l)
  (letrec ([retrAccS
            (λ (l idL hold)
              (if (null? l) hold
                  (let* ([x (filterSplit (λ (t) (member (send (cadr t) getAccompanying) idL)) l)]
                         [x1 (car x)] [x2 (cdr x)])
                    (if (null? x1) hold
                        (retrAccS x2 (append idL (map car x1)) (append hold (list x1)))))))])
    (let ([root (getRoot l)]
          [dependant (removeRoot l)])
      (cons root (retrAccS dependant (map car root) '())))))
(define (calculateState t k e env sprite)
  (let ([actionRes ((send sprite getAction) t)]
        [offsetRes ((send sprite getReaction) t k e env)])
    (cons ((car offsetRes) (car actionRes))
          ((cdr offsetRes) (cdr actionRes)))))
(define (fetchStateNow t k e env id)
  (let ([s (assoc id env)])
    (if s (calculateState t k e env (cadr s))
        (error "No such sprites in this environment."))))

(define SGCanvas%
  (class canvas% (super-new)

    (define baseSpriteList '())
    (define spriteList '())
    (define/public (insert id sprite)
      (set! spriteList (cons (list id sprite) spriteList)))
    (define/public (retr id)
      (let ([res (assoc id spriteList)])
        (if res (second res) #F)))
    (define/public (delete id)
      (set! spriteList
        (filter (λ (t) (not (equal? (car t) id))) spriteList)))
    (define/public (setBaseSprites t) (set! baseSpriteList t))
    (define/public (resetSprite) (set! spriteList baseSpriteList))
    (define/public (env) spriteList)

    (define ticks 0)
    (define tickInterval (/ 60))
    (define/public (resetWorld) (set! ticks 0))
    (define/public (setWorldTime t) (set! ticks t))
    (define/public (setTickInterval t) (set! tickInterval t))
    (define/public (resetTickInterval) (set! tickInterval (/ 60)))
    (define/public (getTimeNow) ticks)
    
    (define keyEventBuffer (new key-event%))
    (define mouseEventBuffer
      (new mouse-event% [event-type 'enter]))
    (define dc (send this get-dc))
    (define/override (on-char e) (set! keyEventBuffer e))
    (define/override (on-event e) (set! mouseEventBuffer e))
    (define paintLock #F)
    (define pauseBitmap (make-object bitmap% 1 1))
    (define/override (on-paint)
      (if paintLock
        (let* ([paintSeq (retrDependentTree spriteList)]
               [bitmapL
                (for/list ([i paintSeq])
                  (for/list ([j i])
                    (calculateState ticks keyEventBuffer
                                    mouseEventBuffer spriteList (cadr j))))])
          
          (for ([i bitmapL])
            (for ([j i])
              (send dc draw-bitmap (car j) (cadr j) (cddr j)))))
        (send dc draw-bitmap pauseBitmap 0 0)))
    (define/public (getPauseBitmap) pauseBitmap)
    (define/public (setPauseBitmap t) (set! pauseBitmap t))

    ; plan : listof (exact-nonnegative-integer * command)
    ; command :
    ;   U ('DELETE symbol)
    ;     ('INSERT symbol Sprite<%>)
    (define plan '())
    (define/public (setPlan t) (set! plan t))
    (define/public (resetPlan) (set! plan '()))
    (define/public (addPlan t)
      (set! plan (sort (cons plan t) < #:key car)))
    (define/public (removePlanAt n)
      (set! plan (filter-not (λ (temp) (= (car temp) n)) plan)))
    (define (executePlan)
      (define (execute c)
        (match (car c)
          ['DELETE (send this delete (second c))]
          ['INSERT (send this insert (second c) (third c))]))
      (define (executePlan_ h)
        (if (list? h)
          (cond ((null? h) (void))
                ((equal? (caar h) ticks) (execute (cdar h)))
                ((> (caar h) ticks) (void))
                (else (executePlan_ (cdr h))))
          (error "A list is needed for a plan.")))
      (executePlan_ plan))

    (define (paintNow)
      (begin (send dc clear)
             (send this on-paint)
             (executePlan)
             (sleep/yield tickInterval)
             (set! ticks (add1 ticks))
             (paintNow)))
    (define mainThread
      (begin (set! paintLock #T)
             (thread paintNow)))

    (define/public (pause)
      (begin (set! paintLock #F)
             (thread-suspend mainThread)))
    (define/public (resume)
      (begin (set! paintLock #T)
             (thread-resume mainThread)))
    (define/public (destroy)
      (begin (set! paintLock #F)
             (kill-thread mainThread)))
    (define/public (revive)
      (begin (set! paintLock #T)
             (send this resetWorld)
             (set! mainThread (thread paintNow))))))
