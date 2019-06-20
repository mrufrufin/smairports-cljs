(ns smairports.audio)

(def Tone js/Tone)
(def tdraw js/Tone.Draw.)
(def num-voices 3)
(def num-sounds 8)
(def num-fbdel 3)
(def synth-delay "+0.5")
(def tport-delay "+0.01")
(def sync? false)
(def with-fb true)
(def sfiles (mapv #(str "res/moa-arr" % "_mono.mp3")
                  (take num-sounds
                        (shuffle (concat (range 1 3) (range 10 58)))
                        ))
  )

(def num-snd (count sfiles))
(def ctx nil)
(def main-out nil)
(def master-gain nil)
(def max-db 0)
(def max-delay 2)
(def fb-val 0.95)
(def delay-wet 0.5)
(def dep1-adsr [0.5 0.1 0.5 1])
(def dep2-adsr [0.005 0.1 0.25 1])
(def synth-vol (/ 1.0 num-voices))
(def player-vol (* synth-vol 0.66))
(def dep1-nodes nil)
(def dep2-nodes nil)
(def transport? false)
(def fb-nodes nil)
(def fb2-nodes nil)
(def fb3-nodes nil)
(def dep1-curvoice (atom 0))
(def dep2-curvoice (atom 0))
(def player-nodes nil)
(def player-npart (atom 0))
(def player-curvoice (atom 0))
;; indices for sound players groupped by delay lines

(defn get-current-time []
  (-> Tone -> .-context .-currentTime)
  )

;; partition sources
(defn partition-srcs [cur-srcs part-sz]
  ;(js/console.log "partitioning")
  (let [cur-shuf (shuffle cur-srcs)
        cur-part (partition part-sz part-sz [nil] cur-shuf)
        cur-groups (mapv (fn [x] (vec
                       (filter (fn [y] ((comp not nil?) y)) x))
                ) cur-part)]
    ;(mapv #(js/console.log %) (flatten myparts))
    cur-groups
    )
  )

;;(defn get-current-time []
;;(.-currentTime ctx))

(defn set-main-volume [volume]
  (let [myvol (js/parseFloat volume)]
    ;(.log js/console "main-vol" myvol)
    (when ((comp not nil?) main-out)
      (.rampTo (.-volume main-out) myvol 0.01)
      )
    )
    
  )


(defn mute-main-out [mute?]
  (if (true? mute?)
    (do (set! (.-mute main-out) true)
        (.log js/console "muted"))
    (do (set! (.-mute main-out) false)
        (.log js/console "unmuted"))
    )
)

(defn master-init []
  (let [cur-master (.-Master Tone)]
    (set! main-out cur-master)
    (mute-main-out true)
    (set-main-volume max-db)
  ))



;;FB STUFF================================================================================
(defn set-fb-delaytime [idx delaytime]
  ;(js/console.log "setting fb time")
  (let [cur-fb (get fb-nodes idx)]
    (set! (.-delayTime cur-fb) delaytime)
    )
  )


(defn fbdel-init []
  (let [fbdel-params (js-obj "delayTime" 0.75 "maxDelay" max-delay)
        cur-fbs (vec (repeatedly num-voices #(Tone.FeedbackDelay. fbdel-params)))
        ]
    (mapv #(do (set! (.-feedback %) fb-val)
               (set! (.-wet %) delay-wet))
          cur-fbs)
    (mapv #(.toMaster % ) cur-fbs)
    (set! fb-nodes cur-fbs)
    )
  )

(defn set-fb2-delaytime [idx delaytime]
  ;(js/console.log "setting fb time")
  (let [cur-fb (get fb2-nodes idx)]
    (set! (.-delayTime cur-fb) delaytime)
    )
  )


(defn fbdel2-init []
  (let [fbdel-params (js-obj "delayTime" 0.75 "maxDelay" max-delay)
        cur-fbs (vec (repeatedly num-voices #(Tone.FeedbackDelay. fbdel-params)))
        ]
    (mapv #(do (set! (.-feedback %) fb-val)
               (set! (.-wet %) delay-wet))
          cur-fbs)
    (mapv #(.toMaster % ) cur-fbs)
    (set! fb2-nodes cur-fbs)
    )
  )

(defn set-fb3-delaytime [idx delaytime]
  (let [cur-fb (get fb3-nodes idx)]
    (set! (.-delayTime cur-fb) delaytime)
    )
  )


(defn fbdel3-init []
  (let [fbdel-params (js-obj "delayTime" 1.25 "maxDelay" max-delay)
        cur-fbs (vec (repeatedly num-voices #(Tone.FeedbackDelay. fbdel-params)))
        ]
    (mapv #(do (set! (.-feedback %) fb-val)
               (set! (.-wet %) delay-wet))
          cur-fbs)
    (mapv #(.toMaster %) cur-fbs)
    (set! fb3-nodes cur-fbs)
    )
  )

;; SOUND PLAYER STUFF ====================================================================================




(defn players-init []
  (js/console.log "players init")
  (let [cur-srcs (mapv #(Tone.Player. %) sfiles)
        part-sz (js/Math.ceil (/ (count cur-srcs) num-voices))
        cur-srcs-part (partition-srcs cur-srcs part-sz)
        ]

    (reset! player-npart (count cur-srcs-part))


        (if (true? with-fb)
           (doall (map-indexed
                   (fn [idx src-group]
                     (mapv (fn [src] (.connect src (get fb3-nodes idx))) src-group)
                     ) cur-srcs-part))

           (doall (map (fn [src] (.toMaster src)) (flatten cur-srcs-part)))
      )

    ;;(doall (map #(.sync %) cur-srcs))
            (set! player-nodes cur-srcs-part)


    )
  )


(defn trigger-arr-voice [cur-dur cur-delay cur-callback]
  (let [cur-grp-idx @player-curvoice
        cur-player (rand-nth (get player-nodes cur-grp-idx))
        cur-playdur (-> cur-player .-buffer .-duration)
        max-start-loc (max (- cur-playdur cur-dur) 0) ;;max start loc in buf considering wanted dur
        cur-start-loc (rand max-start-loc)
        actual-dur (min (- cur-playdur cur-start-loc) cur-dur) ;;actual duration of event
        half-dur (/ actual-dur 2.0)
        cur-start-params (js-obj "startTime" "+0" "duration" actual-dur)
        ]

    (if (>= cur-playdur 4.0)
      (do (set! (.-fadeIn cur-player) half-dur)
          (set! (.-fadeOut cur-player) half-dur))
      (do (set! (.-fadeIn cur-player) 0.05)
          (set! (.-fadeOut cur-player) 0.05))
      )

    (when (true? with-fb) (set-fb3-delaytime cur-grp-idx cur-delay))


      (.schedule (.-Transport Tone)
               (fn [cur-time]
                 (.restart cur-player cur-time cur-start-loc actual-dur)
                 (.schedule tdraw cur-callback cur-time)

                 ) synth-delay)
    (swap! player-curvoice #(mod (inc %) @player-npart))
    )
  )

(defn players-start [start?]
  (let [cur-srcs (flatten player-nodes)]
        (when (and (true? start?) (true? sync?))
          (mapv #(.sync %) cur-srcs)
          )
        )
  )

;;DEP1 STUFF=====================================================================================

(defn set-dep1-adsr [idx att decay sustain release]
  (let [cur-src (get-in dep1-nodes [:srcs idx])
        cur-env (.-envelope cur-src)]
    (set! (.-attack cur-env) att)
    (set! (.-decay cur-env) decay)
    (set! (.-sustain cur-env) sustain)
    (set! (.-release cur-env) release)
    (set! (.-attackCurve cur-env) "linear")
    (set! (.-releaseCurve cur-env) "exponential")

    )
  )
    
;;srcs -> fb nodes
(defn depvoices1-init []
  (let [carrier-type (js-obj "type" "sine")
        [att dec sus rel] dep1-adsr
;        half-dur (/ dep1-dur 2.0)
        env-type (js-obj "attack" att "decay" dec "sustain" sus "release" rel)
        ;env-type (js-obj "attack" 0.001 "decay" 0.1 "sustain" 0.5 "release" 1)
        ;mod-type (js-obj "type" "square")
        ;cur-harm 2
        ;cur-modidx 2
        ;cur-params (js-obj "oscillator" carrier-type "envelope" env-type "modulation" mod-type "harmonicity" cur-harm "modulationIndex" cur-modidx)
        cur-params (js-obj "oscillator" carrier-type "envelope" env-type)

        ;cur-srcs (vec (repeatedly num-voices #(Tone.FMSynth. cur-params)))
        cur-srcs (vec (repeatedly num-voices #(Tone.Synth. cur-params)))

        ]
    ;;(mapv #(.connect % master-gain) cur-srcs)
    (if (true? with-fb)
      (doall (map-indexed (fn [idx src] (.connect src (get fb-nodes idx))) cur-srcs))
      (doall (map (fn [src] (.toMaster src)) cur-srcs))
      )


    ;(mapv #(set! (-> % .-volume .-value) -6) cur-srcs)
    (set! dep1-nodes
          {:srcs cur-srcs
           :started false})
    (.log js/console "depvoice1 init")
    (mapv #(set-dep1-adsr % att dec sus rel) (range num-voices))
    )
  )

(defn trigger-dep1-voice [freq cur-delay cur-domevt]
    ;(js/console.log "trigger dep1")
  (let [cur-voice @dep1-curvoice
        [att dec sus rel] dep1-adsr

        cur-src (get-in dep1-nodes [:srcs cur-voice])]

    (when (true? with-fb) (set-fb-delaytime cur-voice cur-delay))
    (.schedule (.-Transport Tone)
               (fn [cur-time]
                 (.triggerAttackRelease cur-src freq (+ att dec) cur-time synth-vol)
                 (.schedule tdraw cur-domevt cur-time)

                 ) synth-delay)
    
    (swap! dep1-curvoice #(mod (inc %) num-voices))
    )
  )
    
(defn dep1-start [start?]
  (let [started? (get dep1-nodes :started)
        cur-srcs (get dep1-nodes :srcs)]
        (cond
          (and (false? started?) (true? start?))
          (do
            ;(mapv #(.start %) cur-srcs)
            (when (true? sync?) (mapv #(.sync %) cur-srcs))

            (set! dep1-nodes (assoc dep1-nodes :started true)))
          (and (true? started?) (false? start?))
          (do
            ;(mapv #(.stop %) cur-srcs)
            (set! dep1-nodes (assoc dep1-nodes :started false)))
      :else nil
      )))


;;DEP2 STUFF===================================================================================================

(defn set-dep2-adsr [idx att decay sustain release]
  (let [cur-src (get-in dep2-nodes [:srcs idx])
        cur-env (.-envelope cur-src)]
    (set! (.-attack cur-env) att)
    (set! (.-decay cur-env) decay)
    (set! (.-sustain cur-env) sustain)
    (set! (.-release cur-env) release)
      (set! (.-attackCurve cur-env) "exponential")
    (set! (.-releaseCurve cur-env) "exponential")
    )
  )
    
;;srcs -> fb nodes
(defn depvoices2-init []
  (let [carrier-type (js-obj "type" "triangle")
        ;;env-type (js-obj "attack" 0.75 "decay" 0.0 "sustain" 1 "release" 0.75)
        [att dec sus rel] dep2-adsr
        env-type (js-obj "attack" att "decay" dec "sustain" sus "release" rel)
        mod-type (js-obj "type" "square")
        ;cur-harm 3
        ;cur-modidx 3
        ;cur-params (js-obj "oscillator" carrier-type "envelope" env-type "modulation" mod-type "harmonicity" cur-harm "modulationIndex" cur-modidx)
        cur-params (js-obj "oscillator" carrier-type "envelope" env-type)

        ;cur-srcs (vec (repeatedly num-voices #(Tone.FMSynth. cur-params)))
        cur-srcs (vec (repeatedly num-voices #(Tone.Synth. cur-params)))

        ]
    ;;(mapv #(.connect % master-gain) cur-srcs)
       (if (true? with-fb)
      (doall (map-indexed (fn [idx src] (.connect src (get fb2-nodes idx))) cur-srcs))
      (doall (map (fn [src] (.toMaster src)) cur-srcs))
      )

    ;(mapv #(set! (-> % .-volume .-value) -6) cur-srcs)
    (set! dep2-nodes
          {:srcs cur-srcs
           :started false})
    (.log js/console "depvoice2 init")
    (mapv #(set-dep2-adsr % att dec sus rel) (range num-voices))
    )
  )

(defn trigger-dep2-voice [freq cur-delay cur-domevt]
  ;(js/console.log "trigger dep2")
  (let [cur-voice @dep2-curvoice
        [att dec sus rel] dep2-adsr
        cur-src (get-in dep2-nodes [:srcs cur-voice])]
    (when (true? with-fb) (set-fb2-delaytime cur-voice cur-delay))

     (.schedule (.-Transport Tone)
               (fn [cur-time]
                     (.triggerAttackRelease cur-src freq (+ att dec) cur-time synth-vol)
                 (.schedule tdraw cur-domevt cur-time)

                 ) synth-delay)
    
    (swap! dep2-curvoice #(mod (inc %) num-voices))
    )
  )
    
(defn dep2-start [start?]
  (let [started? (get dep2-nodes :started)
        cur-srcs (get dep2-nodes :srcs)]
        (cond
          (and (false? started?) (true? start?))
          (do
            ;(mapv #(do (.start %) (.sync %)) cur-srcs)
            (when (true? sync?) (mapv #(.sync %) cur-srcs))
            (set! dep2-nodes (assoc dep2-nodes :started true)))
          (and (true? started?) (false? start?))
          (do
            ;(mapv #(.stop %) cur-srcs)
            (set! dep2-nodes (assoc dep2-nodes :started false)))
      :else nil
      )))

    

(defn synths-init []
  (when (true? with-fb)
    (fbdel-init)
    (fbdel2-init)
    (fbdel3-init)
    )

  (depvoices1-init)
  (depvoices2-init)
  (players-init)
   )

(defn start-sources [start?]
  (dep1-start start?)
  (dep2-start start?)
  (players-start start?)
 )

(defn start-transport [start? delay-time]
  (.cancel (.-Transport Tone))
  (let [delay-str (str "+" delay-time)]
     (cond
      (and (true? start?) (false? transport?))
      (do (.start (.-Transport Tone) delay-str)
          (.log js/console "start transport")
          ;(start-sources true)
          (set! transport? true))
      (and (not (true? start?)) (true? transport?))
      (do (.stop (.-Transport Tone) delay-str)
          (.log js/console "stop transport")
          ;(start-sources false)
          (set! transport? false))
      :else nil
      )
    )
  )

(defn cleanup []
    ;; clear all callbacks, set main gain to 0
  (mute-main-out true)
  (start-transport false 0.01)
  )



(defn init-audio [win]
  (when (nil? ctx)
    (.log js/console "loading context")
    (set! ctx (if (.-AudioContext win) (win.AudioContext.) (win.webkitAudioContext.)))
    (if (nil? ctx) (js/alert "context not available") (.setContext Tone ctx))
    ;(set! ctx true)
    ;;(set! (-> Tone .-context .-latencyHint) "balanced")
    (set! (-> Tone .-context .-latencyHint) "playback")

    (master-init)
    (synths-init)
    ;;(doall (map osc-create (keys oscs)))
    ;;(osc-connect :carrier true nil nil)
  ;;(println oscs)
    )

  (not (nil? ctx))
)
