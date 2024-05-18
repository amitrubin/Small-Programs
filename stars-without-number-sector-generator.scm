;; Written by Amit Rubin, 2024
;; A system for generating sectors for the TTRPG
;;    S T A R S   W I T H O U T   N U M B E R
;; created for my own amusement :)
;;
;; Chez-Scheme code. It will probably require adjusting to run on Racket, or other scheme langs.
;;
;; To use simply load the file,
;;   then run
;;     (print-sector (generate-sector))
;;   and copy the result to a text editor.
;; note: you might need to run a few times to get a good map.


(define set-nth!
  (lambda (l i val)
    (set-car! (list-tail l i) val)))

(define nth
  (lambda (l i)
    (car (list-tail l i))))

(define sequential-map
  (lambda (f l)
    (letrec ((run
	      (lambda (l)
		(if (null? l)
		    '()
		    (cons (f (car l)) (run (cdr l)))))))
      (run l))))


(define ^member?
  (lambda (eq?)
    (lambda (a s)
      (ormap
       (lambda (b) (eq? a b))
       s))))
(define member? (^member? eq?))
(define member-equal? (^member? equal?))

(define get-attributes-list
  (lambda (l)
    (letrec ((run
	      (lambda (l acc)
		(if (null? l)
		    acc
		    (run (cddr l) (cons (car l)  acc))))))
      (run l '()))))

		      

(define make-standard-map
  (lambda () '(("  " "  " "  " "  " "  " "  " "  " "  ")
	       ("  " "  " "  " "  " "  " "  " "  " "  ")
	       ("  " "  " "  " "  " "  " "  " "  " "  ")
	       ("  " "  " "  " "  " "  " "  " "  " "  ")
	       ("  " "  " "  " "  " "  " "  " "  " "  ")
	       ("  " "  " "  " "  " "  " "  " "  " "  ")
	       ("  " "  " "  " "  " "  " "  " "  " "  ")
	       ("  " "  " "  " "  " "  " "  " "  " "  ")
	       ("  " "  " "  " "  " "  " "  " "  " "  ")
	       ("  " "  " "  " "  " "  " "  " "  " "  "))))



(define format-2d-hex-map " __    __    __    __ \n/~a\\__/~a\\__/~a\\__/~a\\__\n\\__/~a\\__/~a\\__/~a\\__/~a\\\n/~a\\__/~a\\__/~a\\__/~a\\__/\n\\__/~a\\__/~a\\__/~a\\__/~a\\\n/~a\\__/~a\\__/~a\\__/~a\\__/\n\\__/~a\\__/~a\\__/~a\\__/~a\\\n/~a\\__/~a\\__/~a\\__/~a\\__/\n\\__/~a\\__/~a\\__/~a\\__/~a\\\n/~a\\__/~a\\__/~a\\__/~a\\__/\n\\__/~a\\__/~a\\__/~a\\__/~a\\\n/~a\\__/~a\\__/~a\\__/~a\\__/\n\\__/~a\\__/~a\\__/~a\\__/~a\\\n/~a\\__/~a\\__/~a\\__/~a\\__/\n\\__/~a\\__/~a\\__/~a\\__/~a\\\n/~a\\__/~a\\__/~a\\__/~a\\__/\n\\__/~a\\__/~a\\__/~a\\__/~a\\\n/~a\\__/~a\\__/~a\\__/~a\\__/\n\\__/~a\\__/~a\\__/~a\\__/~a\\\n/~a\\__/~a\\__/~a\\__/~a\\__/\n\\__/~a\\__/~a\\__/~a\\__/~a\\\n   \\__/  \\__/  \\__/  \\__/")

(define standard-map->string
  (lambda (2d-map)
    (apply format format-2d-hex-map (apply append 2d-map))))


;; note - I didn't follow the rules regarding repeated coordinates,
;;  but instead chose to re-roll repeated coordinates. This should
;;  be fixed in the next iteration, if I have the patience.
(define ^generate-sector-map 
  (lambda (n)
      (letrec ((gen-coordinates
		(lambda ()
		  (cons (random 8) (random 10))))
	       (run
		(lambda (n acc)
		  (if (zero? n)
		      acc
		      (let ((c (gen-coordinates)))
			(if (member-equal? c acc)
			    (run n acc)
			    (run (- n 1) (cons c acc))))))))
	(letrec
	    ((id 10)
	     (coordinate-pairs (run n '())) 
	     (2d-map (map (lambda (_) (make-list 8 "  "))
			  (make-list 10)))
	     (run2
	      (lambda (x y)
		(if (= x 8)
		    (if (< y 9)
			(run2 0 (+ y 1)))
		    (if (member-equal? (cons x y) coordinate-pairs)
			(begin (set-nth! (list-ref 2d-map y) x id)
			       (set! id (+ id 1))
			       (run2 (+ x 1) y))
			(run2 (+ x 1) y))))))
	  (run2 0 0)
	  2d-map))))

(define generate-sector-map
  (lambda ()
    (^generate-sector-map (+ 21 (random 10)))))

(define ^generate-and-print-sector-map
  (lambda (n)
    (display (standard-map->string (^generate-sector-map n)))
    (newline)
    (display "note: the number of systems = max id number minus 10\n")
    ))

(define generate-and-print-sector-map
  (lambda ()
    (display (standard-map->string (generate-sector-map)))
    (newline)
    (display "note: the number of systems = max id number minus 10\n")
    ))


(define tags-list (list "Zombies" "Abandoned Colony" "Flying Cities" "Misandry/Misogyny" "Rigid Culture" "Alien Ruins" "Forbidden Tech" "Night World" "Rising Hegemon" "Altered Humanity" "Former Warriors" "Nomads" "Ritual Combat" "Anarchists" "Freak Geology" "Oceanic World" "Robots" "Anthropomorphs" "Freak Weather" "Out of Contact" "Seagoing Cities" "Area 51" "Friendly Foe" "Outpost World" "Sealed Menace" "Badlands World" "Gold Rush" "Perimeter Agency" "Secret Masters" "Battleground" "Great Work" "Pilgrimage Site" "Sectarians" "Beastmasters" "Hatred" "Pleasure World" "Seismic Instability" "Bubble Cities" "Heavy Industry" "Police State" "Shackled World" "Cheap Life" "Heavy Mining" "Post-Scarcity" "Societal Despair" "Civil War" "Hivemind" "Preceptor Archive" "Sole Supplier" "Cold War" "Holy War" "Pretech Cultists" "Taboo Treasure" "Colonized Population" "Hostile Biosphere" "Primitive Aliens" "Terraform Failure" "Cultural Power" "Hostile Space" "Prison Planet" "Theocracy" "Cybercommunists" "Immortals" "Psionics Academy" "Tomb World" "Cyborgs" "Local Specialty" "Psionics Fear" "Trade Hub" "Cyclical Doom" "Local Tech" "Psionics Worship" "Tyranny" "Desert World" "Major Spaceyard" "Quarantined World" "Unbraked AI" "Doomed World" "Mandarinate" "Radioactive World" "Urbanized Surface" "Dying Race" "Mandate Base" "Refugees" "Utopia" "Eugenic Cult" "Maneaters" "Regional Hegemon" "Warlords" "Exchange Consulate" "Megacorps" "Restrictive Laws" "Xenophiles" "Fallen Hegemon" "Mercenaries" "Revanchists" "Xenophobes" "Feral World" "Minimal Contact" "Revolutionaries"))

(define gen-tag-nums
  (lambda (n)
    (if (zero? n)
	'()
	(cons (cons (random 100) (random 100))
	      (gen-tag-nums (- n 1))))))



(define gen-unique-tag-nums
  (let ((seen '()))
    (lambda (n)
      (if (> n 50) (error 'max-tags "expected no more than 50 tag requests for uniqueness sake"))
      (if (zero? n)
	  (begin (set! seen '()) '())
	  (let ((a (random 100))
	   	(b (random 100)))
	    (if (or (member? a seen) (member? b seen) (eq? a b))
	   	(gen-unique-tag-nums n)
		(begin (set! seen (cons a (cons b seen)))
		       (cons (cons a b)
		       (gen-unique-tag-nums (- n 1))))))))))


(define gen-atmosphere
  (lambda ()
    (let ((2-d6 (+ 2 (random 6) (random 6))))
      (cond ((= 2-d6 2) "Corrosive, damaging to foreign objects")
	    ((= 2-d6 3) "Inert gas, useless for respiration")
	    ((= 2-d6 4) "Airless or thin to the point of suffocation")
	    ((and (>= 2-d6 5) (<= 2-d6 9)) "Breathable mix")
	    ((= 2-d6 10) "Thick, but breathable with a pressure mask")
	    ((= 2-d6 11) "Invasive, penetrating suit seals")
	    ((= 2-d6 12) "Both corrosive and invasive in its effects")
	    (else 'wtf)))))
	    

(define gen-temperature
  (lambda ()
    (let ((2-d6 (+ 2 (random 6) (random 6))))
      (cond ((= 2-d6 2) "Frozen, locked in perpetual ice")
	    ((= 2-d6 3) "Cold, dominated by glaciers and tundra")
	    ((or (= 2-d6 4) (= 2-d6 5))
	     "Variable cold with temperate places")
	    ((and (>= 2-d6 6) (<= 2-d6 8))
	     "Temperate, Earthlike in its ranges")
	    ((or (= 2-d6 9) (= 2-d6 10))
	     "Variable warm, with temperate places")
	    ((= 2-d6 11) "Warm, tropical and hotter in places")
	    ((= 2-d6 12) "Burning, intolerably hot on its surface")
	    (else 'wtf)))))



(define gen-biosphere
  (lambda ()
    (let ((2-d6 (+ 2 (random 6) (random 6))))
      (cond ((= 2-d6 2) "Remnant biosphere")
	    ((= 2-d6 3) "Microbial life forms exist")
	    ((or (= 2-d6 4) (= 2-d6 5))
	     "No native biosphere")
	    ((and (>= 2-d6 6) (<= 2-d6 8))
	     "Human-miscible biosphere")
	    ((or (= 2-d6 9) (= 2-d6 10))
	     "Immiscible biosphere")
	    ((= 2-d6 11) "Hybrid biosphere")
	    ((= 2-d6 12) "Engineered biosphere")
	    (else 'wtf)))))


(define gen-population
  (lambda ()
    (let ((2-d6 (+ 2 (random 6) (random 6))))
      (cond ((= 2-d6 2) "Failed colony")
	    ((= 2-d6 3) "Outpost")
	    ((or (= 2-d6 4) (= 2-d6 5))
	     "Fewer than a million inhabitants")
	    ((and (>= 2-d6 6) (<= 2-d6 8))
	     "Several million inhabitants")
	    ((or (= 2-d6 9) (= 2-d6 10))
	     "Hundreds of millions of inhabitants")
	    ((= 2-d6 11) "Billions of inhabitants")
	    ((= 2-d6 12) "Alien inhabitants")
	    (else 'wtf)))))


(define gen-tech-level
  (lambda ()
    (let ((2-d6 (+ 2 (random 6) (random 6))))
      (cond ((= 2-d6 2) "TL0, neolithic-level technology")
	    ((= 2-d6 3) "TL1, medieval technology")
	    ((or (= 2-d6 4) (= 2-d6 5))
	     "TL2, early Industrial Age tech")
	    ((and (>= 2-d6 6) (<= 2-d6 8))
	     "TL4, modern postech")
	    ((or (= 2-d6 9) (= 2-d6 10))
	     "TL3, tech like that of present-day Earth")
	    ((= 2-d6 11) "TL4+, postech with specialties")
	    ((= 2-d6 12) "TL5, pretech with surviving infrastructure")
	    (else 'wtf)))))



(define gen-planets
   (lambda (n)
     (let ((f gen-unique-tag-nums))
       (if (> n 50) (set! f gen-tag-nums))
       (let* ((tag-num-choices (f n))
 	      (tag-choices (map (lambda (m)
				  (cons
				   (nth tags-list (car m))
				   (nth tags-list (cdr m))))
 				tag-num-choices))
 	      )
	 (map (lambda (tag)
		`(tags: ,tag tech-level: ,(gen-tech-level) population: ,(gen-population) biosphere: ,(gen-biosphere) temperature: ,(gen-temperature) atmosphere: ,(gen-atmosphere) notes: 'empty))
	      tag-choices) ))))

(define get-planet-attribute
  (lambda (p key)
    (if (null? p)
	(error 'no-such-attribute (format "~a" key))
	(if (eq? (car p) key)
	    (cadr p)
	    (get-planet-attribute (cddr p) key)))))

(define planet->tags
  (lambda (p) (get-planet-attribute p 'tags:)))
(define planet->tech
  (lambda (p) (get-planet-attribute p 'tech-level:)))
(define planet->population
  (lambda (p) (get-planet-attribute p 'population:)))
(define planet->biosphere
  (lambda (p) (get-planet-attribute p 'biosphere:)))
(define planet->temperature
  (lambda (p) (get-planet-attribute p 'temperature:)))
(define planet->atmosphere
  (lambda (p) (get-planet-attribute p 'atmosphere:)))
(define planet->notes
  (lambda (p) (get-planet-attribute p 'notes:)))

(define set-planet-attribute
  (lambda (p key val)
    (letrec ((find-value-idx
	    (lambda (p idx)
	      (if (null? p)
		  (error 'no-such-attribute (format "~a" key))
		  (if (eq? (car p) key)
		      (+ idx 1)
		      (find-value-idx (cddr p) (+ idx 2)))))))
      (set-nth! p
		(find-value-idx p 0)
		val))))

(define set-planet->tags
  (lambda (p v) (set-planet-attribute p 'tags: v)))
(define set-planet->tech
  (lambda (p v) (set-planet-attribute p 'tech-level: v)))
(define set-planet->population
  (lambda (p v) (set-planet-attribute p 'population: v)))
(define set-planet->biosphere
  (lambda (p v) (set-planet-attribute p 'biosphere: v)))
(define set-planet->temperature
  (lambda (p v) (set-planet-attribute p 'temperature: v)))
(define set-planet->atmosphere
  (lambda (p v) (set-planet-attribute p 'atmosphere: v)))
(define set-planet->notes
  (lambda (p v) (set-planet-attribute p 'notes: v)))




(define generate-sector
  (lambda ()
    (let* ((gen-num-planets-per-system
	    (lambda ()
	      (let ((k (+ 1 (random 12))))
		(cond ((<= k 8) 1)
		      ((<= k 10) 2)
		      ((= k 11) 3)
		      ((= k 12) 4)))))
	   (num-systems (+ 21 (random 10)))
	   (2d-map (^generate-sector-map  num-systems))
	   (systems-list (make-list num-systems)))
      (letrec
	  ((run
	    (lambda (n)
	      (if (< n num-systems)
		  (begin (set-nth! systems-list
			    n
			    `(id:
			        ,(+ n 10)
			      planets:
			        ,(gen-planets
				  (gen-num-planets-per-system))))
			 (run (+ n 1)))))))
	(run 0)
	`(map-string:
	  ,(standard-map->string 2d-map)
	  systems:
	  ,systems-list)))))
		      
(define system->id
  (lambda (s) (get-planet-attribute s 'id:)))
(define system->planets
  (lambda (s) (get-planet-attribute s 'planets:)))
(define sector->map-string
  (lambda (s) (get-planet-attribute s 'map-string:)))
(define sector->systems
  (lambda (s) (get-planet-attribute s 'systems:)))

(define print-sector
  (lambda (sector)
    (display (sector->map-string sector))
    (newline)
    (display " SYSTEMS LIST \n--------------\n\n")
    (sequential-map
     (lambda (system)
       (display (format "SYSTEM ID: ~a.\n\n" (system->id system)))
       (display (format "PLANETS\n-------\n"))
       (sequential-map
	(lambda (planet)
	  (display (format "World tags: ~a, ~a.\n"
			   (car (planet->tags planet))
			   (cdr (planet->tags planet))))
	  (display (format "Atmosphere: ~a.\n" (planet->atmosphere planet)))
	  (display (format "Temperature: ~a.\n" (planet->temperature planet)))
	  (display (format "Biosphere: ~a.\n" (planet->biosphere planet)))
	  (display (format "Population: ~a.\n" (planet->population planet)))
	  (display (format "Tech-Level: ~a.\n" (planet->tech planet)))
	  (newline)
	  )
	(system->planets system)
	))
     (sector->systems sector))
    (newline)
    ))



