;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Practica_2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; ===========================================================
; Representamos la distancia y los puntos con numeros enteros.
; ===========================================================

;Ejercicio 1.
; Diseñe una función distancia-origen, que recibe dos números x e y,
; devolviendo como resultado la distancia al origen del punto (x,y).

; Recibe dos puntos (x,y) y devuelve la distancia al origen.
; Ejemplo
;  (distancia-origen 1 1) = (sqrt 2)

;distancia-origen: Number Number -> Number
(define (distancia-origen x y)
  (sqrt (+ (sqr y)(sqr x))))

(check-expect (distancia-origen 5 0) 5)
(check-expect (distancia-origen 0 3) 3)
(check-within (distancia-origen 1 1) (sqrt 2) 0.1)
;Ejercicio 2.
; Diseñe una función distancia-puntos, que recibe cuatro
; números x1, y1, x2 e y2 y devuelve la distancia entre los puntos (x1, y1) y (x2, y2).

; Recibe dos puntos (cuatro numeros) y devuelve la distancia entre ellos
; Ejemplo
;  (distancia-puntos 1 0 3 0) = 2
(define (distancia-puntos x1 y1 x2 y2)
  (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1))))
)

(check-expect (distancia-puntos 1 0 3 0) 2)

;Ejercicio 3.
; Diseñe la función vol-cubo que recibe la longitud de la arista de
; un cubo y calcula su volumen.


;Ejercicio 4.
; Diseñe la función area-cubo que recibe la longitud de la arista
; de un cubo y calcula su área.

;Ejercicio 5.
; Diseñe la función string-insert, que consume un string y un
; número i e inserta "-" en la posición i-ésima del string.


;Ejercicio 6.
; Diseñe la función string-last, que extrae el último caracter de
; una cadena no vacía.


;Ejercicio 7.
; Diseñe la función string-remove-last, que recibe una cadena y
; devuelve la misma cadena sin el último caracter.




;Ejercicio 9.
; Un Instituto de Portugués decide lanzar las siguientes
;  promociones buscando aumentar la cantidad de alumnos:
; Si se anotan 2 amigos, cada uno obtiene un 10% de descuento sobre el
;  valor de la cuota; mientras que si se anotan 3 o más el descuento alcanza el 20%
; Si al momento de pagar se decide abonar 2 meses juntos se recibe un
;  descuento del 15%; en caso de cancelar 3 o más meses a la vez la
;  reducción es del 25%
; Las promociones son combinables, pero nunca pueden superar el 35% de
;  descuento. El valor original de la cuota mensual es de $650.
; La administración del Instituto nos solicitó diseñar la función monto-persona,
;  la cual recibe la cantidad de personas que se están anotando y la cantidad de
;  meses que abonan (para que se aplique la promoción deben pagar la misma
;  cantidad de meses), y devuelve el monto que el Instituto debe cobrarle a
;  cada uno.
; Para desarrollar monto-persona es conveniente definir ciertas
;  constantes, ya que los precios pueden variar con el tiempo.

; Cuota mensual a pagar por persona sin descuentos
(define CUOTA 650)

; Descuento maximo
(define MAX-DESCUENTO 0.35)

; Recibe la cantidad de personas que se anotan,
;  cuantos meses abonan
; Retorna el total a pagar por persona
;monto-persona: Number Number -> Number
(define (monto-personas personas meses)
  (- (total meses) (* (total meses) (descuento-total personas meses)))
)

(check-expect (monto-personas 2 2) 975)
(check-expect (monto-personas 3 3) 1267.50)
(check-expect (monto-personas 1 5) 2437.50)

; Recibe la cantidad de meses que se abonan
; y retorna el total a pagar
; total: Number -> Number
(define (total meses) (* meses CUOTA))


; -----------------
; AUXILIARES
; -----------------

;descuento-persona: Number -> Number
(define (descuento-personas personas)
  (cond
    [(< personas 2) 0]
    [(= personas 2) 0.1]
    [(>= personas 3) 0.2]
  )
)

;descuento-meses: Number -> Number
(define (descuento-meses meses)
  (cond
    [(< meses 2) 0]
    [(= meses 2) 0.15]
    [(>= meses 3) 0.25]
  )
)

;descuento-total: Number Number -> Number
(define (descuento-total personas meses)
  (if (> (+ (descuento-personas personas) (descuento-meses meses)) MAX-DESCUENTO) MAX-DESCUENTO (+ (descuento-personas personas) (descuento-meses meses)))
)

;Ejercicio 10.
; Tomando como base los resultados obtenidos en un laboratorio
;  de análisis clínicos, un médico determina si una persona tiene anemia o no,
;  lo cual depende de su nivel de hemoglobina en la sangre y de su edad.
; Si el nivel de hemoglobina que tiene una persona es menor que el valor
;  mínimo que le corresponde de acuerdo a su edad, el resultado del análisis es
;  "anemia positivo" y en caso contrario es "anemia negativo".
; El médico se basa en los siguientes valores mínimos para cada grupo de edades:
;  edad <= 1 mes: nivel mínimo de hemoglobina normal 13 g/dl
;  1 mes < edad <= 6 meses: nivel mínimo de hemoglobina normal 10 g/dl
;  6 meses < edad <= 12 meses: nivel mínimo de hemoglobina normal 11 g/dl
;  1 año < edad <= 5 años: nivel mínimo de hemoglobina normal 11.5 g/dl
;  5 años < edad <= 10 años: nivel mínimo de hemoglobina normal 12.6 g/dl
;  10 años < edad: nivel mínimo de hemoglobina normal 13 g/dl

; Diseñe una función anemia que recibiendo la edad de una persona expresada
;  en meses y la hemoglobina en sangre expresada en g/dl devuelva #true si la
;  persona está anémica, #false en caso contrario.

;anemia: Number Number -> Boolean
(define (anemia edad hemoglobina)
  (has-anemia? hemoglobina (get-min-hm edad))
)

(check-expect (anemia 1 12) #true)
(check-expect (anemia 1 14) #false)
(check-expect (anemia 24 10) #true)


; --------------------
; FUNCIONES AUXILIARES
; --------------------


; Dada la edad del paciente,
; devuelve la cantidad mínima de hemoglobina aceptada
;get-min-hm: Number -> Number
(define (get-min-hm edad)
  (cond
    [(<= edad 1) 13]
    [(and (> edad 1) (<= edad 6)) 10]
    [(and (> edad 6) (<= edad 12)) 11]
    [(and (> edad (to-months 1)) (<= edad (to-months 5))) 11.5]
    [(and (> edad (to-months 5)) (<= edad (to-months 10))) 12.6]
    [(> edad (to-months 10)) 13]
  )
)

; Dada la hemoglobina del paciente y la minima aceptada
; devuelve #true si el paciente tiene anemia, #false caso contrario
;has-anemia?: Number Number -> Number
(define (has-anemia? hm min-hm)
  (<= hm min-hm)
)

; Expresa en meses una cantidad de años
;to-months: Number -> Number
(define (to-months years)
  (* years 12)
)


;Ejercicio 11.
; Decimos que una terna de números a,b,c es autopromediable si
;  uno de sus valores concide con el promedio de los otros dos.
; Por ejemplo, la terma (7,5,9) es autopromediable puesto que 7 es el
; promedio entre 5 y 9.
; Diseñe una función que dados tres números, devuelva el producto de ellos
;  en caso que formen una terna autopromediable, y la suma de los mismos en
;  caso contrario.
; Defina todas las constantes y funciones auxiliares que crea convenientes
; para obtener un buen diseño.

;autopromediable: Number Number Number -> Number
(define (autopromediable a b c)
  (cond
    [(terna? a b c) (* a b c)]
    [(terna? a c b) (* a b c)]
    [(terna? b c a) (* a b c)]
    [else (+ a b c)]
  )
)

(check-expect (autopromediable 7 5 9) (* 7 5 9))
(check-expect (autopromediable 7 5 1) (+ 7 5 1))

;promedio: Number Number -> Number
(define (promedio x y) (/ (+ x y) 2))

;terna?: Number Number Number -> Boolean
(define (terna? a b c) (= (promedio a b) c))


;Ejercicio 12.
; El consumo promedio de una Chevrolet Zafira modelo 2010 es
;  de 8km/l en ciudad y 11km/l en ruta. Es decir, ese modelo de auto utiliza
;  un litro de nafta para recorrer 8km en ciudad, pero en ruta la misma
;  cantidad de combustible alcanza para recorrer 11km.
; Estos valores fueron calculados utilizando nafta grado 2 (conocida como
;  "súper").
; Al cargar combustible con nafta grado 3 (conocida como
;  "premium"), el rendimiento mejora un 10%. Por lo tanto, con cada litro de
;  combustible grado 3 se puede recorrer un 10% más de distancia de la
;  especificada en el párrafo anterior.
; Diseñe una función autonomía , que dados los siguientes argumentos:
;  - La cantidad de litros restantes en el tanque de combustible, y
;  - La clase de combustible que se está utilizando,
;  devuelva un string indicando la autonomía del auto, tanto en ciudad como
;  en ruta.
; Por ejemplo, si quedan 20 litros de nafta grado 2 en el tanque de
;  combustible, se espera que el string que devuelva la función autonomía sea
;  "Autonomía en ciudad: 160km. Autonomía en ruta: 220km."
; En cambio, si quedan 20 litros de nafta grado 3, se espera que el string sea:
;  "Autonomía en ciudad: 176km. Autonomía en ruta: 242km."
; Utilice constantes y todas las funciones auxiliares que crea conveniente para
;  lograr un buen diseño.

; AUTONOMIA DEL AUTO EN CIUDAD CON NAFTA GRADO 2
(define CIUDAD-GRADO-2 8)

; AUTONOMIA DEL AUTO EN RUTA CON NAFTA GRADO 2
(define RUTA-GRADO-2 11)


; AUTONOMIA DEL AUTO EN CIUDAD CON NAFTA GRADO 3
(define CIUDAD-GRADO-3 (+ CIUDAD-GRADO-2 (* CIUDAD-GRADO-2 0.10)))

; AUTONOMIA DEL AUTO EN RUTA CON NAFTA GRADO 3
(define RUTA-GRADO-3 (+ RUTA-GRADO-2 (* RUTA-GRADO-2 0.10)))

;autonomia: Number Number -> String
(define (autonomia litros clase)
  (cond
    [(= clase 2) (autonomia-clase-2 litros)]
    [(= clase 3) (autonomia-clase-3 litros)]
    [else "Error: Clase de nafta incorrecto. Use 2 o 3."]
  )
)

;autonomia-clase-2: Number -> String
(define (autonomia-clase-2 litros)
  (string-append "Autonomía en ciudad: " (number->string (* litros CIUDAD-GRADO-2)) "km. Autonomía en ruta: " (number->string (* litros RUTA-GRADO-2)) "km.")
)

;autonomia-clase-3: Number -> String
(define (autonomia-clase-3 litros)
  (string-append "Autonomía en ciudad: " (number->string (* litros CIUDAD-GRADO-3)) "km. Autonomía en ruta: " (number->string (* litros RUTA-GRADO-3)) "km.")
)
  
(check-expect (autonomia 20 2) "Autonomía en ciudad: 160km. Autonomía en ruta: 220km.")
(check-expect (autonomia 20 3) "Autonomía en ciudad: 176km. Autonomía en ruta: 242km.")
(check-expect (autonomia 20 4) "Error: Clase de nafta incorrecto. Use 2 o 3.")