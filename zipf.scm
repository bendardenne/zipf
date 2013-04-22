(require plot)
(plot-new-window? #t)

; Renvoie le mot suivant à lire dans in, converti en minuscule.
; Si le curseur de in est à la fin du fichier, renvoie eof.
(define read-word
   (lambda (in)
      (let ((c (read-char in)))
         (cond
            ((eof-object? c) c)
            ((and (char? c) (char-alphabetic? c)) (cons (char-downcase c) (read-word in)))
            (else '())))))


; Un trie est une liste d'un nombre quelconque de trieNodes.
; Un trieNode est une liste d'exactement trois éléments (c n t) tels que
;     c  est un caractère     -- (car node)
;     n  est un entier        -- (cadr node)
;     t  est un trie          -- (caddr node)

; Si word est un mot, représenté comme une liste de caractères
; et trie est un trie, renvoie ce trie, dans lequel on a inséré le mot.

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


; Renvoie un trie contenant chaque mot du fichier et sa fréquence d'apparition.

(define file-to-trie (lambda (in) (file-to-trie* in '())))


; Si in est un fichier et trie un trie,
; renvoie le trie fourni en entrée dans lequel sont insérés tous les mots contenus dans in.

(define file-to-trie*
   (lambda (in trie)
      (let ((word (read-word in)))
         (cond
            ((eq? '() word) (file-to-trie* in trie))     ; read-word renvoit des '(), on les ignore
            ((eof-object? word) trie)
            (else (file-to-trie* in (insert-word (append word '(eol)) trie)))))))


; Si trie est un trie, renvoie une liste de paires pointées dont les car sont les mots du trie,
; les cdr le nombre d'occurences de ces mots dans le trie.

(define trie-to-list
   (lambda (trie)
      (trie-to-list* trie '() '())))

; Si trie est un trie, word une liste de caractères et top une liste de paires pointées,
; renvoie cette liste dans laquelle sont insérées des paires pointées dont
; les car sont toutes les concaténations possibles de word et des mots contenus dans le trie
; et les cdr sont les fréquences d'apparition associées à ces mots du trie.

(define trie-to-list*
   (lambda (trie word top)
      (if (null? trie)
         top
         (let* ((node (car trie)) (newword (append word (list (car node)))))
            (cond
               ((eq? 'eol (car node)) (cons (cons (list->string word) (cadr node)) top))
               (else (trie-to-list* (cdr trie) word (trie-to-list* (caddr node) newword top))))))))

; Si ls est une liste de paires pointées et n un entier, renvoie une liste de listes de 2 éléments
; dont les car sont une suite croissante démarrant à n et les cdr sont les cdr des paires de ls.
(define enumerate-pairs
   (lambda (ls n)
      (if (null? ls) '()
         (cons (list n (cdar ls)) (enumerate-pairs (cdr ls) (+ n 1))))))

(define file (open-input-file "pg4300.txt"))
(define trie (file-to-trie file))
(define words (sort (trie-to-list trie) (lambda (x y) (> (cdr x) (cdr y)))))

(parameterize (
      [plot-x-transform log-transform] [plot-x-ticks (log-ticks)]
      [plot-y-transform log-transform] [plot-y-ticks (log-ticks)]
      [plot-x-label "Rang du mot"]
      [plot-y-label "Fréquence du mot"] )
   (plot (points (enumerate-pairs words 1))))
