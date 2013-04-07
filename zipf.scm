(require plot)
(plot-new-window? #t)

; Renvoit le mot suivant à lire dans in, converti en minuscule.
; Si le curseur de in est à la fin du fichier, renvoit eof.
(define read-word
   (lambda (in)
      (let ((c (read-char in)))
         (cond
            ((eof-object? c) c)
            ((and (char? c) (char-alphabetic? c)) (cons (char-downcase c) (read-word in)))
            (else '())))))

; Un trie est une liste d'un nombre quelconque de trieNodes.
; Un trieNode est une liste d'exactement trois éléments (c n t) tels que
;     c  est un caractère
;     n  est un entier
;     t  est un trie


; Si word est un mot, représenté comme une liste de caractères
; et trie est un trie, renvoit ce trie, dans lequel on a inséré le mot.

(define insert-word
   (lambda (word trie)
      (cond
         ((null? word) trie)
         ((null? trie) (list (list (car word) 1 (insert-word (cdr word) '()))))
         (else
            (let ((node (car trie)))
               (if (equal? (car word) (car node))
                  (cons (list (car node) (+ 1 (cadr node)) (insert-word (cdr word) (caddr node))) (cdr trie))
                  (cons node (insert-word word (cdr trie)))))))))



; Renvoit une paire pointée dont le car un trie contenant chaque mot du fichier et sa fréquence d'apparition
; et le cdr est le nombre de mots contenus dans le fichier in.

(define file-to-trie (lambda (in) (file-to-trie* in 0 '())))


; Si in est un fichier, count un entier et trie un trie,
; renvoit une paire pointée dont le car est le trie fourni en entrée dans lequel
; sont insérés tous les mots contenus dans in et le cdr est la somme de count et du nombre de mots
; contenus dans in.

(define file-to-trie*
   (lambda (in count trie)
      (let ((word (read-word in)))
         (cond
            ((eof-object? word) (cons trie count))
            ((eq? '() word) (file-to-trie* in count trie))     ; read-word renvoit des '(), on les ignore
            (else (file-to-trie* in (+ 1 count) (insert-word (append word '(eol)) trie)))))))


; Si trie est un trie, renvoit une liste de paires pointées dont le car est un mot du trie,
; le cdr le nombre d'occurences de ce mot dans le trie, et triées dans l'ordre décroissant selon ce nombre.

(define trie-to-list
   (lambda (trie)
      (trie-to-list* trie '() '())))

; Si trie est un trie, word une liste de caractères et top une liste de paires pointées triées dans l'ordre
; décroissant selon leur car, renvoit cette liste dans laquelle sont insérées des paires pointées dont
; les car sont toutes les concaténations possibles de word et des mots contenus dans le trie
; et les cdr les cdr les fréquences d'apparitions associées à ces mots du trie.

(define trie-to-list*
   (lambda (trie word top)
      (if (null? trie)
         top
         (let* ((node (car trie)) (newword (append word (list (car node)))))
            (cond
               ((eq? 'eol (car node)) (insert-sorted (cons (list->string word) (cadr node)) top))
               (else (trie-to-list* (cdr trie) word (trie-to-list* (caddr node) newword top))))))))

; si word est une paire pointée dont le premier élément est un réel, n un entier
; et ls une liste de paires pointées triées par ordre décroissant selon leur car réels,
; renvoit cette même liste auquel word a été ajouté si sa valeur est inférieure à celle du
; n-ième élément de ls.

(define insert-sorted
   (lambda (word ls)
      (cond
         ((null? ls) (list word))
         ((< (cdar ls) (cdr word)) (cons word ls))
         ((cons (car ls) (insert-sorted word (cdr ls)))))))

; Si ls est une liste de paires pointées et n un entier, renvoit la même liste de paires pointées
; dont les car sont remplacés par une suite croissante démarrant à n.
(define enumerate-pairs
   (lambda (ls n)
      (if (null? ls) '()
         (cons (list n (cdar ls)) (enumerate-pairs (cdr ls) (+ n 1))))))

;DEBUG
(define file (open-input-file "pg4300.txt"))
;(define file (open-input-file "simpletest"))

(define trie (file-to-trie file))
(define words (trie-to-list (car trie)))


(display (take words 100))

(parameterize (
      [plot-x-transform log-transform] [plot-x-ticks (log-ticks)]
      [plot-y-transform log-transform] [plot-y-ticks (log-ticks)]
      [plot-x-label "Rang du mot"]
      [plot-y-label "Fréquence du mot"] )
   (plot (points (enumerate-pairs words 1))))
