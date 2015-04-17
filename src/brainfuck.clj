(ns brainfuck)

(defn inc-data
  [{:keys [data dp] :as state}]
  (let [curval (get data dp 0)]
    (assoc state :data 
           (assoc data dp (if (= curval 255) 0 (inc curval))))))

(defn dec-data
  [{:keys [data dp] :as state}]
  (let [curval (get data dp 0)]
    (assoc state :data 
           (assoc data dp (if (= curval 0) 255 (dec curval))))))

(defn output
  [{:keys [data dp output] :as state}]
  (assoc state :output (conj output (char (get data dp 0)))))

(defn input
  [{:keys [input data dp] :as state}]
  (assoc state :data (assoc data dp (int (first input))) :input (rest input)))

(defn jump-fwd
  [{:keys [code input data ip dp] :as state}]
  state)

(defn jump-bwd
  [{:keys [code input data ip dp] :as state}]
  state)

(defn execute-current-instruction
  [{:keys [code ip dp] :as state}]

  (let [instruction (nth code ip)]
    (case instruction
      \> (assoc state dp (inc dp))
      \< (assoc state dp (dec dp))
      \+ (inc-data state)
      \- (dec-data state)
      \. (output state)
      \, (input state)
      \[ (jump-fwd state)
      \] (jump-bwd state)
      state)
    )
  )

(defn execute-step
  "Execute single BF instruction returning current state of the
  output"
  [{:keys [code input data ip dp] :as state} ]
  
  (if (< ip (count code))
    (let [new-state (execute-current-instruction state)]
      (println new-state)
      (recur (assoc new-state :ip (inc ip))))
    output))

(defn execute-string
  "Evaluate the Brainfuck source code in `source` using `input` as a
  source of characters for the `,` input command.

  Either returns a sequence of output characters, or `nil` if there
  was insufficient input."
  [source input]

  (println (str "Executing code: " source))
  (execute-step {:code source :input input :data {} :ip 0 :dp 0 :output []}))

