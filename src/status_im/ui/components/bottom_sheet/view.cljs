(ns status-im.ui.components.bottom-sheet.view
  (:require [status-im.ui.components.react :as react]
            [status-im.ui.components.animation :as animation]
            [status-im.ui.components.bottom-sheet.styles :as styles]
            [reagent.core :as reagent]
            [re-frame.core :as re-frame]))

(def initial-animation-duration 300)
(def release-animation-duration 150)
(def cancellation-animation-duration 100)
(def swipe-opacity-range 100)
(def cancellation-height 180)
(def min-opacity 0.05)
(def min-velocity 0.1)

(defn- animate
  [{:keys [opacity new-opacity-value
           bottom new-bottom-value
           duration callback] :as opts}]
  (animation/start
   (animation/parallel
    [(animation/timing opacity
                       {:toValue         new-opacity-value
                        :duration        duration
                        :useNativeDriver true})
     (animation/spring bottom
                       {:toValue         new-bottom-value
                        :duration        duration
                        :tension         40
                        :friction        6
                        :useNativeDriver true})])
   (when (fn? callback) callback)))

(defn- on-move
  [{:keys [height bottom-value opacity-value]}]
  (fn [_ state]
    (let [dy (.-dy state)]
      (cond (pos? dy)
            (let [opacity (max min-opacity (- 1 (/ dy (- height swipe-opacity-range))))]
              (animation/set-value bottom-value dy)
              (animation/set-value opacity-value opacity))
            (neg? dy)
            (animation/set-value bottom-value (/ dy 2))))))

(defn- cancelled? [height dy vy]
  (or
   (<= min-velocity vy)
   (> cancellation-height (- height dy))))

(defn- on-release
  [{:keys [height bottom-value close-sheet opacity-value] :as opts}]
  (fn [_ state]
    (let [{:strs [dy vy]} (js->clj state)]
      (if (cancelled? height dy vy)
        (close-sheet)
        (animate {:bottom            bottom-value
                  :new-bottom-value  0
                  :opacity           opacity-value
                  :new-opacity-value 1
                  :duration          release-animation-duration})))))

(defn- swipe-pan-responder [opts]
  (.create
   react/pan-responder
   (clj->js
    {:onMoveShouldSetPanResponder (fn [_ state]
                                    (or (< 10 (js/Math.abs (.-dx state)))
                                        (< 5 (js/Math.abs (.-dy state)))))
     :onPanResponderMove          (on-move opts)
     :onPanResponderRelease       (on-release opts)
     :onPanResponderTerminate     (on-release opts)})))

(defn- pan-handlers [pan-responder]
  (js->clj (.-panHandlers pan-responder)))

(defn- on-open [{:keys [bottom-value opacity-value height internal-visible]}]
  (animate {:bottom            bottom-value
            :new-bottom-value  (- height)
            :opacity           opacity-value
            :new-opacity-value 1
            :duration          initial-animation-duration}))

(defn- on-close
  [{:keys [bottom-value opacity-value on-cancel
           internal-visible with-callback?]}]
  (animate {:bottom            bottom-value
            :new-bottom-value  0
            :opacity           opacity-value
            :new-opacity-value 0
            :duration          cancellation-animation-duration
            :callback          (fn []
                                 (reset! internal-visible false)
                                 (reagent/flush)
                                 (when (fn? on-cancel)
                                   (on-cancel)))}))

;; TODO: Replace with modal, firtstly should convert also popover
;; NOTE: onRequestClose of modal should close sheet
;; TODO: animate content-height change
;; TODO: add max-height
(defn bottom-sheet []
  (let [opacity-value    (animation/create-value 0)
        bottom-value     (animation/create-value 0)
        content-height   (reagent/atom 200) ; TODO: add a defualt value (half of screen height?) to have better init animation
        internal-visible (reagent/atom false)
        external-visible (reagent/atom false)]
    (fn [{:keys [content on-cancel disable-drag? show?]
          :or   {on-cancel #(re-frame/dispatch [:bottom-sheet/hide])}
          :as   opts}]
      (let [close-sheet (fn []
                          (on-close {:opacity-value    opacity-value
                                     :bottom-value     bottom-value
                                     :internal-visible internal-visible
                                     :on-cancel        on-cancel}))]
        (when-not (= @external-visible show?)
          (reset! external-visible show?)
          (reset! internal-visible show?))
        [react/modal {:visible          @internal-visible
                      :transparent      true
                      :on-request-close close-sheet}
         [react/animated-view {:style styles/container}
          [react/touchable-highlight {:style    styles/container
                                      :on-press close-sheet}
           [react/animated-view {:style (styles/shadow opacity-value)}]]

          [react/keyboard-avoiding-view {:pointer-events "box-none"
                                         :style          styles/sheet-wrapper}
           [react/animated-view (merge
                                 {:pointer-events "box-none"
                                  :style          (styles/content-container bottom-value)}
                                 (when-not disable-drag?
                                   (pan-handlers
                                    (swipe-pan-responder {:bottom-value  bottom-value
                                                          :opacity-value opacity-value
                                                          :height        @content-height
                                                          :close-sheet   close-sheet}))))
            [react/view {:style styles/content-header}
             [react/view styles/handle]]
            [react/animated-view {:on-layout
                                  (fn [evt]
                                    (let [height (+ (->> evt
                                                         .-nativeEvent
                                                         .-layout
                                                         .-height)
                                                    styles/border-radius
                                                    styles/bottom-padding
                                                    styles/top-padding)]
                                      (reset! content-height height)
                                      (on-open {:bottom-value     bottom-value
                                                :opacity-value    opacity-value
                                                :height           height
                                                :internal-visible internal-visible})))}
             [content]]]]]]))))

