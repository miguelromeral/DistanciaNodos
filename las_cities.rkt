#lang racket
; ------------------------------------------
; INTERFAZ DE USUARIO
; ------------------------------------------
;
;(run lista_ciudades matriz_adyacencia metodo nombre_ciudad_inicio nombre_ciudad_fin)
;
;donde 0 < metodo < 5, N = longitud(lista_ciudades), matriz_adyacencia = matriz NxN,
;nombre_ciudad_inicio y nombre_ciudad_fin pertenece a lista_ciudades
;
; Ejemplo:
; (run (list "A" "B" "C" "D" "E") (list (list 0 1 7 inf 11)(list inf 0 6 3 inf)(list 5 6 0 2 2)(list inf inf 3 0 inf)(list 2 inf inf inf 0)) 2 "A" "E")
; ------------------------------------------
; ------------------------------------------
; Raúl Calvo Laorden
; &
; Miguel Ángel García Romeral
; ------------------------------------------

;Variable que guarda la matriz de adyacencia del problema actual.
(define adyacencia '())
;Lista de ciudades que representan la matriz de adyacencia.
(define cities '())
;Método para realizar la búsqueda. El rango está de 1 a 4 incluídos.
(define way 0)
;Variable que nos indica un valor infinito. Para no tener qu eponer +inf.0
(define inf +inf.0)
;Nombre de ciudad de inicio para buscar la ruta. "nac" = not a city.
(define begining "nac")
;Nombre de ciudad de fin para buscar la ruta. "nac" = not a city.
(define end "nac")
;Pone como lista de ciudades actual la lista que se le pasa por parámetro.
(define (set_cities lista) (set! cities lista))
;Pone como matriz de adyacencia actual la matriz que se le pasa por parámetro.
(define (set_adyacencia mat) (set! adyacencia mat))
;Comprueba que el método introducido es correcto (está entre 1 y 4), sino, lo pone a 0 indicando que hubo un error.
(define (set_way w)
  (if (> w 4)
      (set! way 0)
      (if (< w 1)
          (set! way 0)
          (set! way w)
      )
  )
)
;Indica si un elemento está en la lista.
(define (esta? sim lis)
  (if (null? lis)
      #f
      (if (string=? sim (car lis))
          #t
          (esta? sim (cdr lis))
      )
   )
)
;Coge la posición de un elemento en la lista únicamente si el elemento está en ella.
(define (get_posicion c l)
   (if (string=? c (car l))
        1
        (+ 1 (get_posicion c (cdr l)))      
   )
)
;Indica la posición de la ciudad en la lista. (El rango va de 1 a N). Si no está devuelve -1.
(define (posicion_ciudad c l)
  (if (esta? c l)
      (get_posicion c l)
      -1
      )
)
;Comprueba que la matriz de adyacencia es NxN con N = longitud de la lista de ciudades.
(define (comprobar_matriz mat list)
  ;Comprueba que hay N filas.
  (if (= (length list) (length mat))
      ;Pasa a comprobar que todas las filas tienen N columnas.
      (comprobar_matriz_cada_fila mat list)
      #f
      )
)
;Comprueba cada fila de la matriz si tiene la misma longitud.
(define (comprobar_matriz_cada_fila mat list)
  ;Si se ha llegado al final es true.
  (if (null? mat)
      #t
      ;Sino, se comprueba que el número de columnas de la fila (o longitud de la lista) es N.
      (if (= (length list) (length (car mat)))
          ;Y se miran las demás.
          (comprobar_matriz_cada_fila (cdr mat) list)
          #f
          )
   )
 )

;de : eje vertical
;a  : eje horizontal
;   \ A | B | C
;   |---+---+---
; A | 1 | 2 | 3
; B | 4 | 5 | 6
; C | 7 | 8 | 9
;
; (get_coord 1 3) --> 3
;
; Obtiene la posición (de,a) de la matriz de adyacencia. El rango va de 1 a N.
; Automáticamente resta uno para poder recorrer la lista de 0 a N-1 como funciona en Racket.
(define (get_coord de a)(list-ref (list-ref adyacencia (- de 1)) (- a 1)))

; Indica cuántas veces está un elemento en la lista.
; Sirve para comprobar que un camino no pasa dos veces por la misma ciudad, momento en que podamos la rama.
(define (repeticiones elemento lista)
  (if (null? lista)
      0
      ;Si coincide el elemento, sumamos uno y comprobamos el resto de la cola.
      (if (string=? elemento (car lista))
          (+ 1 (repeticiones elemento (cdr lista)))
          (repeticiones elemento (cdr lista))
          )
  )
)

; Coste del camino mínimo encontrado. Inicialmente es infinito.
(define vcamino_min inf)
; Lista de nodos a visitar en el camino de coste mínimo ordenadamente.
(define ruta_min '())
; Vacía las soluciones para que no se confundan con las de otra instancia del problema.
(define (vaciar_sol) (set! vcamino_min inf) (set! ruta_min '()))


; Comprueba que la solución encontrada es mejor que la anterior y si es así la pone como nueva.
(define (verificar_ruta ruta total)
  ;Si el coste del camino es menor, actualizamos la ruta y el coste total.
  (cond [(< total vcamino_min)(and (set! vcamino_min total) (set! ruta_min ruta))])
)

;------------------------------------------------------------------------------------------------------------------------------
;
;
;                    BÚSQUEDA EN ANCHURA
;
;
;------------------------------------------------------------------------------------------------------------------------------


; Devuelve una lista con los sucesores de un nodo a la lista actual para el método en anchura, ya que los pone al final de la lista de nodos a visitar.
; Se le pasan como parámetro un nodo auxiliar para recorrer por la lista (de 1 a N), la ruta actual, el nodo actual (de 1 a N),
; y el coste total de la ruta actual.
; Los nodos de la lista se representan mediante una tupla o lista de 4 elementos: (actual fin lista_visitados coste_camino)
(define (add_hijos queda ruta act total)
  ;Si el nodo auxiliar supera el número de ciudades, se corta el proceso de crear a los sucesores.
  (if (<= queda (length cities))
      ;Si la ciudad no está en la ruta aún y el coste de ir desde el actual al auxiliar es menor que infinito (es posible llegar allí)...
         (if (and (not (esta? (list-ref cities (- queda 1)) ruta))
                  (< (get_coord act queda) inf))
             ;Añadimos el nodo actual a una nueva ruta añadiendo su coste (lo metemos en una tupla con formato constante) y seguimos recorriendo
             ;los nodos sucesores.
                   (cons (list act queda (cons (list-ref cities (- queda 1)) ruta) (+ total (get_coord act queda)))
                   (add_hijos (+ 1 queda) ruta act total))
             ;Si no es posible llegar hasta ese nodo, se comprueban los demás nodos sucesores.
             (add_hijos (+ 1 queda) ruta act total)
          )
         '()
    )
  )

; Aplica el método búsqueda en anchura.
; Los nodos de la lista se representan mediante una tupla o lista de 4 elementos: (actual fin lista_visitados coste_camino)
; Se le pasan por parámetro el número de nodo fin (1 a N) y la lista de abiertos.
(define (anch fin rest)
  ;Si la lista de abiertos (rest) está vacía, se acaba la búsqueda.
  (cond [(not (null? rest))
         ;A partir de este momento, se cogerá siempre la cabeza de la lista de abiertos.
         ;Si la longitud ruta de la cabeza es mayor que la lista de ciudades quiere decir que hay repetidos o no se ha llegado al destino,
         ;entonces no haremos nada con ese nodo y pasamos a recorrer el resto de la lista.
         (if (<= (length (list-ref (car rest) 2)) (length cities))
             ;Si el nodo fin y el nodo actual de la cabeza coinciden y el coste es menor que infinito, se pasa a comprobar la solución
             ;y se sigue mirando el resto de la lista de abiertos.
             (if (and (= fin (list-ref (car rest) 1))
                  (< (get_coord (list-ref (car rest) 0)(list-ref (car rest) 1)) inf))
                 (and (verificar_ruta (list-ref (car rest) 2) (list-ref (car rest) 3))
                      (anch fin (cdr rest)))
                 ;Si no es solución, se aplica el método en anchura pero se le añaden la lista de sucesores del nodo actual.
                 (anch fin (append (cdr rest) (add_hijos 1 (list-ref (car rest) 2)  (list-ref (car rest) 1) (list-ref (car rest) 3))))
              )
             (anch fin (cdr rest))
           )
         ])
  )

; Se encarga de inicializar el método en anchura.
(define (anchura)
  (anch
   ;Llama a la función auxiliar con la posición de la ciudad de fin (de 1 a N) y añade un primer nodo con la posición del inicio (1 a N),
   ;la posición del nodo actual (1 a N), una lista con el nodo de inicio como ruta actual y el coste de llegar desde el inicio hasta el actual
   ;En la primera llamada, el nodo inicio y actual coinciden, pero luego el nodo inicio indica de dónde viene el nodo actual. Esto se hace
   ;para poder sumar el coste en las rutas.
   (posicion_ciudad end cities)
   (list (list (posicion_ciudad begining cities) (posicion_ciudad begining cities)
         (list begining) (get_coord (posicion_ciudad begining cities) (posicion_ciudad begining cities))))
   )
)

;------------------------------------------------------------------------------------------------------------------------------
;
;
;                    BÚSQUEDA EN PROFUNDIDAD Y PROFUNDIDAD LIMITADA
;
;
;------------------------------------------------------------------------------------------------------------------------------

; Crea la lista de sucesores y trata los nodos en orden para verificar que se cumple la búsqueda en profundidad.
; Se le pasa el nodo inicial, los costes de llegar desde el nodo inicial a los demás nodos, la ruta actual, el coste total, el nodo fin,
; rec_list: un indicador de qué nodo visitar en la lista de costes, y el tope para dejar de buscar.
(define (prof_aux actual lista_dest ruta total fin rec_list tope)  
  ;Si la lista de costes es nula, es decir, ya no hay hermanos del nodo actual, no hacemos nada.
   (cond [(not (null? lista_dest))
      ; Si el coste de ir desde el nodo actual hasta el auxiliar (rec_list) es menor que infinito, pasamos a tratar el nodo
      ; y después trataremos sus hermanos. Así se cumple el método en profundidad.
      (if (< (get_coord actual (+ 1 rec_list)) inf)
          ;Se llama a la función profundidad añadiendo a la ruta el nodo auxiliar y sumando su coste.
          (and (prof actual (+ 1 rec_list) fin
                     (cons (list-ref cities rec_list) ruta)
                     (+ total (get_coord actual (+ 1 rec_list))) tope)
               ;Después, coprobamos sus hermanos.
               (prof_aux actual (cdr lista_dest) ruta total fin (+ 1 rec_list) tope)
           )
          ;Si no es alcanzable, tratamos el nodo hermano si lo hubiese.
          (prof_aux actual (cdr lista_dest) ruta total fin (+ 1 rec_list) tope)
       )
   ])
  )


; Verifica que un nodo es solución en profundidad.
(define (prof ant inicio fin ruta total tope)
  ;Si el nodo actual no está repetido en la ruta y la ruta no supera el tope especificado, se pasa a tratar el nodo.
  (cond [(and (< (repeticiones (list-ref cities (- inicio 1)) ruta) 2)(<= (length ruta) tope))
          ;Si la posición de inicio es igual a fin, entonces tenemos una solución y pasamos a comprobar si es mejor que la anterior.
          (if (= inicio fin)
                    (verificar_ruta ruta total)
                    ;Si no es solución, se expande el nodo
                    (prof_aux inicio (list-ref adyacencia (- inicio 1)) ruta total fin 0 tope)
                 )]
   )
  )
  
; Aplica el método en profundidad.
(define (profundidad)
  ;Comienza con el nodo inicio y actual (de 1 a N). Coinciden al principio. Además, se le pasa el número de la ciudad de fin (1 a N)
  ;y el coste de ir desde inicio a actual. Además, se le pasa un tope para que no expanda un nodo cuando llegue a esa profundidad.
  (prof
   (posicion_ciudad begining cities)
   (posicion_ciudad begining cities)
   (posicion_ciudad end cities)
   (cons begining '())  ;Ruta inical
   (get_coord (posicion_ciudad begining cities)(posicion_ciudad begining cities))   ;Coste inicial
   (length cities)
   )
)
; Aplica el método en profundidad iterativa. Busca una posible solución con un tope. Si no ha encontrado
; ninguna solución, se busca de nuevo con un tope más grande. En el momento en que haya una solución, no
; buscará ninguna más, por lo que tienen prioridad las rutas con menos saltos que las de menor coste.
(define (profundidad_iterativa tope)
  (prof 
   (posicion_ciudad begining cities)
   (posicion_ciudad begining cities)
   (posicion_ciudad end cities)
   (cons begining '())  ;Ruta inical
   (get_coord (posicion_ciudad begining cities)(posicion_ciudad begining cities))   ;Coste inicial
   tope
   )
  ;Si el tope es menor que la lista de ciudades y aún no se ha encontrado una ruta mínima, busca de nuevo con
  ;un tope más alto.
  (cond [(and (<= tope (length cities))(null? ruta_min))
         (profundidad_iterativa (+ 1 tope))
      ])
  )

;------------------------------------------------------------------------------------------------------------------------------
;
;
;                    BÚSQUEDA COSTE UNIFORME
;
;
;------------------------------------------------------------------------------------------------------------------------------

;Encuentra el mínimo de dos números. Comprueba además por si son infinito, que no funcionarían las comparaciones.
(define (minimo a b)(if (infinite? a) b (if (infinite? b) a (if (< a b) a b))))

;Encuentra el elemento menor de una lista.
(define (menor lista)
  (if (null? lista)
      ;Si la lista es nula devolvemos inf.
      inf
      (if (= 1 (length lista))
          (car lista)
          (minimo (car lista)(menor (cdr lista)))
          )
     )
)

;Obtiene el índice de un símbolo en la lista (de 0 a N-1). Solo funciona si sí está el elemtno.
(define (index lista simb) (if (= simb (car lista)) 0 (+ 1 (index (cdr lista) simb))))
;Sustituye un elemento de la lista por otro en la posición indicada.
(define (sustituir lista posic elem) (if (= posic 0) (cons elem (cdr lista)) (cons (car lista)(sustituir (cdr lista) (- posic 1) elem))))

; Crea la lista de sucesores y trata los nodos en orden para verificar que se cumple la búsqueda de coste unfirme.
; Se le pasa el nodo inicial, los costes de llegar desde el nodo inicial a los demás nodos, la ruta actual, el coste total y el nodo fin.
; Cuando coge el mínimo coste, pone dicho coste a infinito para que, a la hora de expandir a sus hermanos, no lo vuelvan a coger.
(define (cu_aux actual lista_dest ruta total fin)
  ;Si el mínimo no es infinito...
  (cond [(not (infinite? (menor lista_dest)))
         ;Se expande el nodo que supone un coste mínimo y después expande a sus hermanos (habiendolos "eliminado" el camino anteriormente seleccionado.
         (and (cu actual (+ 1 (index lista_dest (menor lista_dest))) fin
             (cons (list-ref cities (index lista_dest (menor lista_dest))) ruta) (+ total (get_coord actual (+ 1 (index lista_dest (menor lista_dest))))))
              (cu_aux actual (sustituir lista_dest (index lista_dest (menor lista_dest)) inf) ruta total fin)
              )
       ])
)

; Verifica que un nodo es solución en coste uniforme.
(define (cu ant inicio fin ruta total)
  ;Si el nodo actual no está repetido en la ruta y la ruta no supera el máximo número de ciudades, se pasa a tratar el nodo y a expandir a sus hijos.
  (cond [(and (< (repeticiones (list-ref cities (- inicio 1)) ruta) 2)(<= (length ruta) (length cities)))
          (if (= inicio fin)
                    (verificar_ruta ruta total)
                    (cu_aux inicio (list-ref adyacencia (- inicio 1)) ruta total fin)
                 )]
   )
  )
  
; Aplica el método de coste uniforme.
(define (coste_uniforme)
  ;Comienza con el nodo inicio y actual (de 1 a N). Coinciden al principio. Además, se le pasa el número de la ciudad de fin (1 a N)
  ;y el coste de ir desde inicio a actual.
  (cu
   (posicion_ciudad begining cities)
   (posicion_ciudad begining cities)
   (posicion_ciudad end cities)
   (cons begining '())  ;Ruta inical
   (get_coord (posicion_ciudad begining cities)(posicion_ciudad begining cities))   ;Coste inicial
   )
)

;------------------------------------------------------------------------------------------------------------------------------

;Realiza el algoritmo que se le ha pasado.
(define (switch_algoritmo)
  (if (= way 1)
      (anchura)
      (if (= way 2)
      (profundidad)
      (if (= way 3)
      (profundidad_iterativa 2)
      (coste_uniforme)
      )))
)

;Vacía la instancia del problema.
(define (vaciar) (set_cities '()) (set_adyacencia '()) (set! begining "nac") (set! end "nac") (set_way 0))

;Ejecuta el programa pasándole la lista de ciudades, la matriz de adyacencia, el método de búsqueda y los nombres de las ciudades de inicio y final.
(define (run lista matriz busqueda inicio final)
  ;Lo primero es vaciar la solución.
  (vaciar_sol)
  ;Si el tamaño de la matriz (NxN) coincide con el de la lista (N), se añaden ambos al problema.
  (if (comprobar_matriz matriz lista)
    (and (set_cities lista)
      (set_adyacencia matriz)
      ;Ponemos el método de búsqueda.
      (set_way busqueda)
      ;Si el método de búsqueda es 0, entonces el método no es válido.
      (if (= way 0)
          ;Se muestra un mensaje indicando los posibles métodos de búsqueda.
          (and (vaciar) "El metodo debe ser 1 (Anchura), 2 (Profundidad), 3 (Profundidad Iterativa), 4 (Coste Uniforme)")
          ;Si es válido, se comprueban si están las ciudades de inicio y fin en la lista y se pasa a ejecutar el programa.
          (if (esta? inicio lista)
              (if (esta? final lista)
                  (run_aux inicio final)
                  (and (vaciar) "La ciudad de fin no se encuentra en la lista de ciudades.")
               )
              (and (vaciar) "La ciudad de inicio no se encuentra en la lista de ciudades.")
           )
       )
    )
    (and (vaciar) "La lista y la matriz de adyacencia no coinciden")
  )
)

;Indica por pantalla si hay una solución o no.
(define (run_aux i f)
  ;Pone como ciudad inicio y fin a las ciudades y ejecuta el algoritmo.
  (set! begining i)
  (set! end f)
  (switch_algoritmo)
  ;Si ha acabado la búsqueda y la ruta es nula, no se ha encontrado ninguna solución.
  (if (null? ruta_min)
      (display "Lamentablemente no hemos encontrado ninguna ruta.")
      ;Sino, se muestra por pantalla los datos de la solución encontrada.
      (and (display "La ruta es: ") (display (reverse ruta_min)) (display ", cuya longitud es: ") (display (length ruta_min)) (display " y un coste de: ")(display vcamino_min))  
  )
)

(define lis (list "A" "B" "C" "D" "E"))
(define ejemplo (list (list 0 3 20 20 inf)(list 3 0 5 inf 40)(list 20 5 0 10 inf)(list 20 inf 10 0 15)(list inf 40 inf 15 0)))