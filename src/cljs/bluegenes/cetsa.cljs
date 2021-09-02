(ns bluegenes.cetsa
  (:require [bluegenes.config :refer [server-vars]]
            [clojure.string :as str]))

(def api-endpoint (str/replace (:auth-api @server-vars) #"auth$" "api.php"))

