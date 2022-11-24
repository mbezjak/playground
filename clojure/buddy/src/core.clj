(ns core)

;; https://github.com/funcool/buddy
(require '[buddy.core.hash :as hash])
(require '[buddy.core.codecs :as codecs])
(require '[buddy.core.mac :as mac])
(require '[buddy.core.nonce :as nonce])
(require '[buddy.core.keys :as keys])

(hash/sha256 "foo bar")
(codecs/bytes->hex (hash/sha256 "foo bar"))
(codecs/bytes->str (hash/sha256 "foo bar"))
(-> (mac/hash "foo bar" {:key "mysecretkey" :alg :hmac+sha256})
    (codecs/bytes->hex))
(-> (mac/verify "foo bar" (codecs/hex->bytes "61849448bdbb67b39d609471eead667e65b0d1b9e01b1c3bf7aa56b83e9c8083")
                {:key "mysecretkey" :alg :hmac+sha256}))

(def key (nonce/random-bytes 32))
(def iv (nonce/random-bytes 16))
(-> (mac/hash "some-data" {:key key :iv iv :alg :poly1305+aes})
    (codecs/bytes->hex))

(def privkey (keys/private-key "/tmp/privkey.pem" "abcd"))
(def pubkey (keys/public-key "/tmp/pubkey.pem"))


(require '[buddy.sign.jwt :as jwt])
(jwt/sign {:userid 1} "secret")
(jwt/unsign (jwt/sign {:userid 1} "secret") "secret")


(require '[clj-time.core :as time])
(require '[clj-time.coerce :as c])
(def claims
  {:user 1 :exp (c/to-long (time/plus (time/now) (time/seconds 5)))})
(def token (jwt/sign claims "key"))
(jwt/unsign token "key" {:now (c/to-long (time/now))})
