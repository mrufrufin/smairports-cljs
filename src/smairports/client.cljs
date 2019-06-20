(ns smairports.client
  (:require
  [smairports.audio :as au]
  [reagent.core :as r])
  )

(def nosleep (js/NoSleep.))
(def socket (js/io.))
(defonce wakelock-enabled false)
(def loaded? (atom false))
(def transport-delay 0.01)

(def win js/window)
(def cur-timeouts (atom []))
(def status-text (r/atom "press to load"))
(def progress-text (r/atom "socketMusic: airports"))
(def info-text (r/atom "make sure phone is unmuted and volume is up"))
(def bg-colors {:default "#000000" :dep1 "#cc0000" :dep2 "#ff6699" :arr "#0099ff"})

(def event-text "")
(def default-text "")
(def bg-flash-dur 1.25)
(def info-flash-dur 1.25)
(def bg-callback nil)
(def info-callback nil)
(def gain-val (r/atom 1))

(defn generate-str [flight-type words]
  (let [type-str (if (= flight-type :arr) "ARRIVAL" "DEPARTURE")
        word-vec (into [type-str] words)
        cleaned-words (map #(clojure.string/replace (str %) #"_" " ") word-vec)]
    (apply str (interpose "\r\n" cleaned-words))
    )
  )

(defn clear-timeouts []
  (doall (map #(js/clearTimeout %) @cur-timeouts))
  (reset! cur-timeouts [])
  )

(defn sched-timeout [timeout-func delay-time]
  (clear-timeouts)
  (let [new-timeout (js/setTimeout timeout-func (* 1000 delay-time))]
    (swap! cur-timeouts conj new-timeout)
    ))

(defn set-background-color [evt]
  (let [cur-color (get bg-colors evt)]
    (set! (-> js/document .-body .-style .-background) cur-color))
  )

(defn event-background-color [evt]
  (when (not (nil? bg-callback)) (.clearTimeout win bg-callback))
  (set-background-color evt)
  (set! bg-callback (js/setTimeout #(set-background-color :default)(* 1000 bg-flash-dur)))
  )

(defn event-info-text [cur-text]
  (when (not (nil? info-callback)) (.clearTimeout win info-callback))
  (reset! info-text cur-text)
  (set! info-callback (js/setTimeout #(reset! info-text default-text) (* 1000 info-flash-dur)))
  )

(defn flash-event-vis [evt cur-text]
  (event-background-color evt)
  (event-info-text cur-text)
  )

(defn set-event-vis [evt cur-text]
  (set-background-color evt)
  (reset! info-text cur-text)
  )

;; end piece/ cleanup

(defn end-piece []
  (set-event-vis :default "")
  (when (false? wakelock-enabled)
         (.disble nosleep)
         (set! wakelock-enabled false))
  (au/cleanup)
  )

(defn unload-callback [evt]
  (end-piece)
  )


;; rendering stuff
(defn render-gain-slider []
  [:input {:type "range" :min 0 :max 1 :default-value @gain-val
           :class "gain-slider"
           :on-change #(let [tval (-> % .-target .-value)]
                         (au/set-main-volume tval))
           :step 0.001}]
  )

(defn render-progress []
  [:span {:style {:text-align "center"} :id "progress-text"}
   @progress-text
   ])

(defn render-info []
  [:span {:style {:text-align "center"} :id "info-text"}
   @info-text
   ])
   

(defn render-button []
  [:button
   {:id "load-button"
    :on-click
    #(let [ctx-success? (au/init-audio win)]
       (when (false? wakelock-enabled)
         (.enable nosleep)
         (set! wakelock-enabled true)
         )
       (when (and (not (nil? ctx-success?)) (not @loaded?))
         (au/set-main-volume @gain-val)
         (au/start-transport true transport-delay)
         (reset! status-text "loaded")
         (reset! progress-text "close browser when done")
         (reset! loaded? true)
         ))}
    @status-text ])
  

(defn home-page []
  [:div
   [:div {:class "textbox"}
    (render-progress)
    ]
   [:div
    (render-button)
    ]
   [:div {:class "textbox"}
    "close browser when done"
    ]
   [:div {:class "gui"}
    "(gain)" [:br]
    (render-gain-slider)
    ]
   [:div {:class "textbox"}
    (render-info)
    ]
   ]
  )

(defn mount-components []
  (r/render-component [#'home-page] (.getElementById js/document "app")))


(defn set-socket-callbacks []
  (.on socket "main_gain"
       (fn [args]
         (let [gain (first args)]
             (when @loaded? (au/set-main-volume gain)))
         )
       )

  (.on socket "main_mute"
       (fn [args]
         (let [mute-flag (first args)]
           (when @loaded? (au/mute-main-out (= 1 mute-flag)))
           )
         )
       )

  (.on socket "departure"
       (fn [args]
         (let [[cur-city cur-airline cur-flight cur-gate cur-freq cur-inst cur-delayms] args
               cur-str (generate-str :dep [cur-city cur-airline cur-flight cur-gate])
               cur-evt (if (= cur-inst 0) :dep1 :dep2)
               cur-delay (/ cur-delayms 1000.0)
               cur-domevt (fn [] (flash-event-vis cur-evt cur-str))

               ]
           (when @loaded?
;             (flash-event-vis cur-evt cur-str)
             (if (= cur-inst 0)
               (au/trigger-dep1-voice cur-freq cur-delay cur-domevt)
               (au/trigger-dep2-voice cur-freq cur-delay cur-domevt)
               )
             )
             
           )
         
         )
       )

  (.on socket "ts"
       (fn [args]
         (let [timestring (str (first args))]
           (when @loaded?
             (reset! status-text timestring)
             )
           )
         )
       )
           
       

  (.on socket "arrival"
       (fn [args]
         (let [[cur-city cur-loc cur-airline cur-flight cur-durms] args
               cur-str (generate-str :arr [cur-city cur-airline cur-flight cur-loc])
               cur-dur (/ cur-durms 1000)
               cur-delay (/ (+ (rand-int 2250) 125) 1000.0)
               cur-callback (fn [] (flash-event-vis :arr cur-str))

               ]
           (when @loaded?
             (flash-event-vis :arr cur-str)
             (au/trigger-arr-voice cur-dur cur-delay cur-callback)
           )
           )
         )
       )
  )
  

(defn ^:dev/after-load start []
  (js/console.log "stat"))

(defn ^:export init []
  (js/console.log "init")
  (set! (.-onbeforeunloead win) unload-callback)
  (set-socket-callbacks)
  (mount-components)
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "stop"))
