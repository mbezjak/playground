(ns core)

;; http://doc/jdk/cur/java/security/Signature.html
;; https://docs.oracle.com/javase/tutorial/security/apisign/step3.html
(import java.security.Signature)
(import java.security.KeyPairGenerator)

(def bytes (.getBytes "abc" "UTF-8"))

(def gen (KeyPairGenerator/getInstance "DSA"))
(def key-pair (.generateKeyPair gen))
(def priv (.getPrivate key-pair))
(def pub (.getPublic key-pair))

;; sign
(def dsa (Signature/getInstance "SHA256withDSA"))
(.initSign dsa priv)
(.update dsa bytes)
(def signature (.sign dsa))
(format "%x" (new java.math.BigInteger signature))

;; verify
(def dsa (Signature/getInstance "SHA256withDSA"))
(.initVerify dsa pub)
(.update dsa bytes)
(.verify dsa signature)
