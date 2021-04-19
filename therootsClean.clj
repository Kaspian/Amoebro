(ns amoebas.competition2021.Amoegos.core
  (:use amoebas.defs amoebas.lib amoebas.run)
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

(def Dir-to-Neighbor1   ;; maps direction to the corresponding neighbor we need to move to/hit in/divide into
                        ;; it's the inverse map of Neighbors above
    { 0 [-1 -1] 1 [0 -1] 2 [1 -1] 3 [1 0] 4 [1 1] 5 [0 1] 6 [-1 1] 7 [-1 0] } 
)

;;
;; This is the first slightly more complicated amoeba function.
;; Note how the different sub-behaviors are made into their own little functions
;;

(defn sum
  [x]
  (reduce + x)
)

(def Neighbor-To-Dir2  
{ 0 [-1 -1] 1 [0 -1] 2 [1 -1] 3 [1 0] 4 [1 1] 5 [0 1] 6 [-1 1] 7 [-1 0] } 
)

(defn create-amoego
    [low-energy divide-energy select-target]
    
    
    (fn [energy health species env data]
        (let
            [
                ;; UNCHANGED - CAN IT BE IMPROVED?
                do-move (fn []
                            (let
                                [
                                    hs           (hostiles species Neighbors env)
                                    empty-nb     (empty-neighbors env)              
                                    by-fuel      (sections-by-fuel empty-nb env)    
                                    ;;by-enemies   (sections-by-hostiles empty-nb env species)
                                    enemS      (hostiles species Environment env)
                                ]

                                (if (empty? empty-nb)
                                    ;; COMMENTED CODE: IF ENEMIES ARE NEARBY, PRIORITIZE ATTACKING OVER RESTING (APPEARS TO BE INEFFECTIVE?)
                                    ;; (if (empty? enemS)
                                    ;; {:cmd :rest}
                                    ;; {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env))}
                                    ;; )
                                    {:cmd :rest}
                                    {:cmd :move :dir (last by-fuel)}
                                )
                            )
                        )
                ;; CHANGE MAXFUELINGENERGY TO MORE FITTING VALUE (5 FROM THEROOTS)
                do-fuel (fn []
                            ;;(if (< MaxFuelingEnergy (:fuel (env Here)))     
                            (if (< 5 (:fuel (env Here)))
                                {:cmd :rest}                               
                                (do-move)                                   
                            )
                        )
                ;; UNCHANGED - CAN IT BE IMPROVED?
                do-hit  (fn []
                            (let
                                [hs  (hostiles species Neighbors env)]
                                
                                (if (empty? hs)                             
                                    (do-fuel)             
                                    ;; {:cmd :hit :dir (Neighbor-To-Dir (sections-by-weakest hs species env))}               
                                    {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env))}
                                )
                            )
                        )
                ;; FROM THEROOTS - DIVIDE IN THE DIRECTION OF FOOD, NOT RANDOMLY
                do-div  (fn [empty-nb]
                            (let [hs (hostiles species Neighbors env)]
                                (if (and (< 10 (sum (:fuel env (sections-by-fuel empty-nb env)))) (< 5 (count(sections-by-fuel empty-nb env))) )
                                    {:cmd :divide :dir (last (sections-by-fuel empty-nb env))}
                                    (do-move)
                                )
                            )
                            ;;{:cmd :divide :dir (rand-nth empty-nb)}         ;; amoeba parenting: drop the child wherever...
                        )
            ]

            (cond
                (< energy low-energy)
                    (do-fuel)
                (< divide-energy energy)
                    (let
                        [empty-nb   (empty-neighbors env)]
                        
                        (if (empty? empty-nb)       
                            (do-hit)              
                            (do-div empty-nb)      
                        )
                    )           
                (not (= (list) (hostiles species Neighbors env))) ;; FROM THEROOTS, WHY?
                    (do-hit)                    
                :else
                    (do-fuel)                  
            )
        )
    )
)

;; LOW-ENERGY: 10, DIVIDE-ENERGY 70. TINKER?
(def Evam (create-amoego 10 70 most-energy-and-fuel-target-selector))