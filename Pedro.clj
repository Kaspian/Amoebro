(ns amoebas.competition2021.amoegos.core
    (:use amoebas.defs amoebas.lib amoebas.run)
)

(defn wack-target-selector  
    "picks a target with the lowest health score"
    [hs species env]   

    (let
        [shs (sort-by #(> 5 (:health (:occupant (env %)))) hs) ]
                
        (last shs)
    )
)

(defn weakest-target-selector  
    "picks a target with the lowest health score"
    [hs species env]   

    (let
        [shs (sort-by #(:health (:occupant (env %))) hs) ]
                
        (first shs)
    )
)

(defn most-energy-and-fuel-target-selector 
    "picks a target with the highest sum of stored energy and energy in the cell it is in"
    [hs species env]  
    
    (let
        [energy-and-fuel
            (fn [cell]
                (if (:occupant cell)
                    (+ (:fuel cell) (:energy (:occupant cell)))
                    (:fuel cell)
                )
            )
        ]

        (last (sort-by #(energy-and-fuel (env %)) hs))
    )
)

(defn sum
    [x]
    (reduce + x)
)

(defn create-pedro
    [low-energy divide-energy select-target]
    
    
    (fn [energy health species env data]
        (let
            [
                do-move (fn []
                            (let                                        ;; otherwise we gotta move...
                                [
                                    empty-nb     (empty-neighbors env)              ;; these are the empty neighbors
                                    by-fuel      (sections-by-fuel empty-nb env)
                                    hs  (hostiles species Neighbors env)    ;; this sorts them by the amount of fuel in the corresponding sections
                                ]

                                (if (empty? empty-nb)       ;; no empty neighbors?
                                    ;;(if (and (<= health 3) (not (empty? (hostiles species Neighbors env))))
                                        ;;(do 
                                            ;;(println "Kamikaze")
                                            ;;{:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env))}
                                        ;;)
                                    {:cmd :rest};;) ;; hunker down, we can't move --- FIXME: perhaps we should hit someone?
                                    {:cmd :move :dir (last by-fuel)}    ;; move toward the most fuel
                                )
                            )
                        )
                do-fuel (fn []
                            (if (< 5 (:fuel (env Here)))     ;; are we *at* a McDonald's?
                                {:cmd :rest}                                ;; chomp chomp
                                (do-move)                                   ;; otherwise, keep looking
                            )
                        )
                do-hit  (fn []
                            (let
                                [hs  (hostiles species Neighbors env)]      ;; hostile neighbors
                                
                                (if (empty? hs)                             ;; nobody to hit?
                                    (do-fuel)                               ;; eat
                                    {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env))}   ;; KAPOW!
                                )
                            )
                        )
                do-div  (fn [empty-nb]
                            ;(println (type (list empty-nb)))
                            (if (and (> 5 (count(sections-by-fuel empty-nb env))) (> 18 (sum (:fuel env (sections-by-fuel empty-nb env)))))
                            (do-move)
                            {:cmd :divide :dir (last (sections-by-fuel-density empty-nb env))}         ;; drops child where there is most energy
                            )
                        )
            ]

            (cond
                (not (empty? (hostiles species Neighbors env)))
                (do
                    (let                                        ;; otherwise we gotta move...
                        [
                         hs  (hostiles species Neighbors env)      ;; hostile neighbors
                         empty-nb     (empty-neighbors env)
                         weakest (wack-target-selector hs species env)              ;; these are the empty neighbors
                         ]

                        ;;(if (> 5 (:health weakest))
                        (if (not(= (type weakest) (type nil)))
                            (do
                                ;(println "weakwhack")
                                {:cmd :hit :dir (Neighbor-To-Dir (weakest-target-selector hs species env))}   ;; KAPOW!)
                            )
                        )
                    )
                )
                (< energy low-energy)
                (do-fuel)
                (< divide-energy energy)               ;; parenthood!
                (let
                    [empty-nb   (empty-neighbors env)]

                    (if (empty? empty-nb)       ;; nowhere to put that crib?
                        (do-hit)                ;; then screw parenthood, hit someone
                        (do-div empty-nb)       ;; oooh, look, it's... an amoeba :-(
                        )
                    )
                (not (= (list) (hostiles species Neighbors env)))            ;; someone looking at us funny?
                (do-hit)                    ;; whack 'em
                :else
                (do-fuel)                   ;; let's eat some more
                )
        )
    )
)

(def Evam (create-pedro 10 70 most-energy-and-fuel-target-selector))
