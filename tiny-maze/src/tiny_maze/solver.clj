(ns tiny-maze.solver)

(defn vector-rota
  "Rota un vector"
  [renglón]
  (conj (apply vector (rest renglón)) (first renglón)))


(defn cambia-último
  "Cambia el último elemento de un vector"
  [renglón nuevo]
  (conj (pop renglón) nuevo))


(defn reemplaza-x
  "Cambia el primer elemento de una matriz por :x"
  [matriz]
  (assoc matriz 0 (assoc (first matriz) 0 :x)))


(defn siguiente-derecha
  "Genera el siguiente laberinto cuando nos movemos a la derecha"
  [matriz]
  (apply vector (map vector-rota (reemplaza-x matriz))))


(defn siguiente-abajo
  "Genera el siguiente laberinto cuando nos movemos abajo"
  [matriz]
  (vector-rota (reemplaza-x matriz)))


(defn resuelve-auxiliar
  "Función auxiliar para resolver el laberinto recursivamente"
  [laberinto]
  (let [esquina (first (first laberinto))]
    (if (= esquina :E)
      [(vector-rota (map vector-rota (reemplaza-x laberinto))) true]
      (if (or (= esquina 1) (= esquina :x))
        [nil false]
        (let [[abajo abajo-llega] (resuelve-auxiliar (siguiente-abajo laberinto))
              [derecha derecha-llega] (resuelve-auxiliar (siguiente-derecha laberinto))]
          (if abajo-llega
            [abajo true]
            (if derecha-llega
              [derecha true]
              nil)))))))


(defn
  solve-maze
  [maze]
  (let [[solucion exito] (resuelve-auxiliar maze)]
    (when exito solucion)))
