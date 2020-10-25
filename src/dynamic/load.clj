(ns dynamic.load
  #_(:require))

(defn sin
  "
   Sinusoidal loading pattern
        $$P_0 \\sin(\\omega t + \\phi)$$
   "
  [t & {:keys [amplitude frequency phase-shift]
        :or   {amplitude 1
               frequency 1
               phase-shift 0}}]
  (* amplitude (Math/sin (+ (* frequency t) phase-shift))))

(comment
  (sin Math/PI :amplitude 1))