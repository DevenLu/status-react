(ns status-im.ui.screens.wallet.navigation
  (:require [status-im.ui.screens.navigation :as navigation]
            [status-im.ui.screens.wallet.signing-phrase.views :as signing-phrase]))

(defmethod navigation/preload-data! :wallet-stack
  [db [event]]
  (let [wallet-set-up-passed? (get-in db [:multiaccount :wallet-set-up-passed?])
        sign-phrase-showed?   (get db :wallet/sign-phrase-showed?)]
    (if (or (= event :navigate-back) wallet-set-up-passed? sign-phrase-showed?)
      db
      (assoc db :popover/popover {:view [signing-phrase/signing-phrase]}
             :wallet/sign-phrase-showed? true))))

(defmethod navigation/preload-data! :wallet-add-custom-token
  [db [event]]
  (dissoc db :wallet/custom-token-screen))

(defmethod navigation/preload-data! :add-new-account
  [db [event]]
  (dissoc db :add-account))
