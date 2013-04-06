(define file (open-input-file "pg4300.txt"))   ; FIXME
;(define file (open-input-file "simpletest"))   ; FIXME

; Renvoit le mot suivant à lire dans in, converti en minuscule.
; Si le curseur de in est à la fin du fichier, renvoit eof.
(define read-word
   (lambda (in)
      (let ((c (read-char in)))
         (cond
            ((eof-object? c) c)
            ((and (char? c) (char-alphabetic? c)) (cons (char-downcase c) (read-word in)))
            (else '())
         ))))

; Un trie est une liste d'un nombre quelconque de trieNodes.
; Un trieNode est une liste d'exactement trois éléments (c n t) tels que
;     c  est un caractère
;     n  est un entier
;     t  est un trie


; Si word est un mot, représenté comme une liste de caractères
; et trie est un trie, renvoit ce trie, dans lequel on a inséré le mot

(define insert-word
   (lambda (word trie)
      (cond
         ((null? word) trie)
         ((null? trie) (list (list (car word) 1 (insert-word (cdr word) '()))))
         (else
            (let ((node (car trie)))
               (if (equal? (car word) (car node))
                  (cons (list (car node) (+ 1 (cadr node)) (insert-word (cdr word) (caddr node))) (cdr trie))
                  (cons node (insert-word word (cdr trie))))))
      )))



; Renvoit une paire pointée dont le car est le nombre de mots contenus dans le fichier in
; et le cdr un trie contenant le nombre d'occurences pour chaque mots du fichier.

(define file-to-trie (lambda (in) (file-to-trie* in 0 '())))


; Si in est un fichier, count un entier et trie un trie contenant "count" mots issus de in,
; renvoit une paire pointée indentique à celle décrite pour file-to-trie.

(define file-to-trie*
   (lambda (in count trie)
      (let ((word (read-word in)))
         (cond
            ((eof-object? word) (cons count trie))
            ((eq? '() word) (file-to-trie* in count trie))
            (else (file-to-trie* in (+ 1 count) (insert-word (append word '(eol)) trie)))
         ))))

(define get-top-100
   (lambda (trie)
      (get-top-100* trie '() '())))

(define get-top-100*
   (lambda (trie word top)
      (if (null? trie)
         top
         (let ((node (car trie)) (newword (append word (list (car (car trie))))))
            (cond
               ((eq? 'eol (car node)) (insert-top-100 (cons (cadr node) (list->string word)) 1 top))
               (else (get-top-100* (cdr trie) word (get-top-100* (caddr node) newword top))
            ))))))

(define insert-top-100
   (lambda (word pos ls)
      (cond
         ((null? ls) (list word))
         ((< (car (car ls)) (car word)) (cons word ls))
         (else (cons (car ls) (insert-top-100 word (+ 1 pos) (cdr ls)))))))

(define print-100
   (lambda (ls pos)
      (if (> pos 100)
         '()
         (cons (car ls) (print-100 (cdr ls))))))

;DEBUG
(define a (file-to-trie file))
(define b (get-top-100 (cdr a)))
