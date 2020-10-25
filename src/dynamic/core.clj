(ns dynamic.core)

;; Newtons method
;; 

;; Newmark
;; GTR

(defn ** [x] (* x x))

(defn bn [system state]
  (let [{:keys [beta dt]}     system
        {:keys [x_n v_n a_n]} state]
    (+ x_n
       (* v_n dt)
       (* (** dt)
          beta
          (- 1 beta)
          a_n))))

(defn zeta
  [system]
  (let [{:keys [beta dt]} system]
    (* (** (- 1 beta))
       (** dt))))

(defn v_n+1 [system state]
  (let [{:keys [beta dt]}       system
        {:keys [v_n a_n a_n+1]} state]
    (+ v_n (* dt (+ (* beta a_n)
                    (* (- 1 beta) a_n+1))))))

(defn x_n+1 [system state]
  (let [{:keys [beta dt]}       system
        {:keys [x_n v_n v_n+1]} state]
    (+ x_n (* dt (+ (* beta v_n)
                    (* (- 1 beta) v_n+1))))))