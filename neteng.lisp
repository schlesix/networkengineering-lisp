					; Funktionen für Network Engineers

					;(ql:quickload "iterate")
					;(use-package :iterate)

(ql:quickload "alexandria")

(defun cidr2ddn(cidr-number)
  "Gibt einen CIDR-Wert im DDN-Format zurück"
  (case cidr-number
    (32  (return-from cidr2ddn "255.255.255.255"))
    (31  (return-from cidr2ddn "255.255.255.254"))
    (30  (return-from cidr2ddn "255.255.255.252"))
    (29  (return-from cidr2ddn "255.255.255.248"))
    (28  (return-from cidr2ddn "255.255.255.240"))
    (27  (return-from cidr2ddn "255.255.255.224"))
    (26  (return-from cidr2ddn "255.255.255.192"))
    (25  (return-from cidr2ddn "255.255.255.128"))
    (24  (return-from cidr2ddn "255.255.255.0"))
    (23  (return-from cidr2ddn "255.255.254.0"))
    (22  (return-from cidr2ddn "255.255.252.0"))
    (21  (return-from cidr2ddn "255.255.248.0"))
    (20  (return-from cidr2ddn "255.255.240.0"))
    (19  (return-from cidr2ddn "255.255.224.0"))
    (18  (return-from cidr2ddn "255.255.192.0"))
    (17  (return-from cidr2ddn "255.255.128.0"))
    (16  (return-from cidr2ddn "255.255.0.0"))
    (15  (return-from cidr2ddn "255.254.0.0"))
    (14  (return-from cidr2ddn "255.252.0.0"))
    (13  (return-from cidr2ddn "255.248.0.0"))
    (12  (return-from cidr2ddn "255.240.0.0"))
    (11  (return-from cidr2ddn "255.224.0.0"))
    (10  (return-from cidr2ddn "255.192.0.0"))
    (9  (return-from cidr2ddn "255.128.0.0"))
    (8  (return-from cidr2ddn "255.0.0.0"))
    (7  (return-from cidr2ddn "254.0.0.0"))
    (6  (return-from cidr2ddn "252.0.0.0"))
    (5  (return-from cidr2ddn "248.0.0.0"))
    (4  (return-from cidr2ddn "240.0.0.0"))
    (3  (return-from cidr2ddn "224.0.0.0"))
    (2  (return-from cidr2ddn "192.0.0.0"))
    (1  (return-from cidr2ddn "128.0.0.0"))
    (0  (return-from cidr2ddn "0.0.0.0"))
    ))



(defun ddn2cidr(ddn-string)
  "Gibt einen CIDR-Wert im DDN-Format zurück"
  (alexandria:switch (ddn-string :test #'equal)
    ("255.255.255.255"  (return-from ddn2cidr 32))
    ("255.255.255.254"  (return-from ddn2cidr 31))
    ("255.255.255.252"  (return-from ddn2cidr 30))
    ("255.255.255.248"  (return-from ddn2cidr 29))
    ("255.255.255.240"  (return-from ddn2cidr 28))
    ("255.255.255.224"  (return-from ddn2cidr 27))
    ("255.255.255.192"  (return-from ddn2cidr 26))
    ("255.255.255.128"  (return-from ddn2cidr 25))
    ("255.255.255.0"  (return-from ddn2cidr 24))
    ("255.255.254.0"  (return-from ddn2cidr 23))
    ("255.255.252.0"  (return-from ddn2cidr 22))
    ("255.255.248.0"  (return-from ddn2cidr 21))
    ("255.255.240.0"  (return-from ddn2cidr 20))
    ("255.255.224.0"  (return-from ddn2cidr 19))
    ("255.255.192.0"  (return-from ddn2cidr 18))
    ("255.255.128.0"  (return-from ddn2cidr 17))
    ("255.255.0.0"  (return-from ddn2cidr 16))
    ("255.254.0.0"  (return-from ddn2cidr 15))
    ("255.252.0.0"  (return-from ddn2cidr 14))
    ("255.248.0.0"  (return-from ddn2cidr 13))
    ("255.240.0.0"  (return-from ddn2cidr 12))
    ("255.224.0.0"  (return-from ddn2cidr 11))
    ("255.192.0.0"  (return-from ddn2cidr 10))
    ("255.128.0.0"  (return-from ddn2cidr 9))
    ("255.0.0.0"  (return-from ddn2cidr 8))
    ("254.0.0.0"  (return-from ddn2cidr 7))
    ("252.0.0.0"  (return-from ddn2cidr 6))
    ("248.0.0.0"  (return-from ddn2cidr 5))
    ("240.0.0.0"  (return-from ddn2cidr 4))
    ("224.0.0.0"  (return-from ddn2cidr 3))
    ("192.0.0.0"  (return-from ddn2cidr 2))
    ("128.0.0.0"  (return-from ddn2cidr 1))
    ("0.0.0.0"  (return-from ddn2cidr 0))
    (t  (return-from ddn2cidr nil))
    ))


(defun ip-string-to-number (ip-string)
  "Wandelt eine IP-Adresse in String-Form in eine Ganzzahl um"
  (let ((octets nil)(zahl 0))
					; String in Oktette zerlegen. Separator .
    (setq octets (uiop:split-string ip-string :separator "."))
					; Es müssen vier Oktette sein
    (if (/= (length octets) 4) nil (progn
					; Oktette entspreched wichten und aufaddieren
				     (Setq zahl (+  (ash (Parse-integer (first octets)) 24)
						    (ash (parse-integer (second octets)) 16)
						    (ash (parse-integer (third octets)) 8)
						    (parse-integer (fourth octets))
						    )
					   )
				     
				     ))
    ))



(defun ip-number-to-string (ip-number)
  "IP-Adresse im Integer-Format in String umwandeln"
  (let ((ip-string nil) (oktettwert 0))
					; Okttette jeweils isolieren und dann als String zusammenbauen
    (setq oktettwert (floor ip-number 16777216))
    (Setq ip-string (write-to-string oktettwert))
    (setq ip-number (- ip-number (* oktettwert 16777216)))
    (setq oktettwert (floor ip-number 65536))
    (setq ip-string (concatenate 'string ip-string "." (write-to-string oktettwert)))
    (Setq ip-number (- ip-number (* oktettwert 65536)))
    (setq oktettwert (floor ip-number 256))
    (setq ip-string (concatenate 'string ip-string "." (write-to-string oktettwert)))
    (setq ip-number (- ip-number (* oktettwert 256)))
    (setq oktettwert ip-number)
    (setq ip-string (concatenate 'string ip-string "." (write-to-string oktettwert)))
    ))


(defun test ()
  "Diese Funktion tetstet die Library"
  (let ((ip-wert 0) (ip-string ""))
    (setq ip-wert (ip-string-to-number "255.254.253.252"))
    (setq ip-string (ip-number-to-string ip-wert))
    )
  )




(defun get-subnetdata (subnet-string &optional printout)
  "Subnet-Kennzahlen für CIDR-String berechnen, z. B. '192.168.2.17/24'"
  (let ((ip-address nil) (cidr nil) (ip-cut nil) (ip-subnet) (ip-broadcast nil) (i nil) (divisor nil) (hostanteil nil) (invalid-netmask 1))
					; Prüfen auf CIDR
    (setq ip-cut (uiop:split-string subnet-string :separator "/"))
    (if (= (length ip-cut) 2) (progn
				(Setq cidr (parse-integer (second ip-cut)))
				(setq invalid-netmask 0)))
					; Falls kein CIDR gefunden, auf DDN prüfen
    (if (= invalid-netmask 1) (progn
				(setq ip-cut (uiop:split-string subnet-string :separator " "))
				(if (= (length ip-cut) 2) (progn
							    (setq cidr (ddn2cidr (second ip-cut)))
					; Gültigkeit CIDR prüfen (1-32)
							    (if (and (> cidr 0) (< cidr 33)) 
								(setq invalid-netmask 0)
								)

							    ))
				))
					; Falls Netmask ermittelt werden konnte, Kenndaten ausrechnen
    (if (= invalid-netmask 0) (progn
				(Setq ip-address (ip-string-to-number (first ip-cut)))
				(setq divisor (expt 2 31))
				(setq i 0)
				(dotimes (n  cidr )
				  (setq i (+ i divisor))
				  (setq divisor (/ divisor 2)))
				(setq ip-subnet (logand ip-address i))
				(setq hostanteil (- (expt 2 (- 32 cidr)) 1))
				(setq ip-broadcast (+ ip-subnet hostanteil))
				(Setq ip-broadcast (ip-number-to-string ip-broadcast))
				(setq ip-subnet (ip-number-to-string ip-subnet))
				(if (eq printout t)
				    (progn
				      (format t "Subnet...: ~A~%CIR......: /~d~%DDN......: ~d~%Broadcast: ~A~%~%" ip-subnet cidr (cidr2ddn cidr) ip-broadcast)
				      (return-from get-subnetdata nil))
			            (Values ip-subnet cidr ip-broadcast))
				)))
  )


(defun format-mac (mac-address)
  "Formatiert eine MAC-Addresse im Cisco-Format"
					; Prüfen, ob String übergeben wurde. Falls nicht: Abbruch
  (if (not (stringp mac-address)) (return-from format-mac nil))
					; Für den String wird jedes Zeichen durchiteriert. Alles, was zulässig ist (0-9, a-f), wird zu einem neuen String formatted-mac zusammengebaut
  (let ((formatted-mac nil))
					; Alles in Kleinbuchstaben
    (setq mac-address (string-downcase  mac-address))
					; Gültige Zeichen 0-9 und a-f ausfiltern
    (loop for c across mac-address do
      (if (or (and (char>= c #\a) (char<= c #\f))  (and (char>= c (digit-char 0)) (char<= c (digit-char 9))))
	  (setf formatted-mac
		(concatenate 'string formatted-mac (list c))))     
	  )
					; Wenn eine gültige MAC-Adresse vorliegt (=12 Zeichen), weitermachen. Ansonsten: Abbruch.
    (if (/= (length formatted-mac) 12)
	(return-from format-mac nil)
	(return-from format-mac (uiop:strcat  (subseq formatted-mac 0 4) "." (subseq formatted-mac 4 8) "." (subseq formatted-mac 8 12))))
    )

  )

(defun get-help ()
  (print "format-mac {mac-adresse} - Formatiert MAC-Adresse im Cisco-Format")
  (print "get-subnetdata '192.168.2.54/24'")
  (print "ddn2cidr {DDN netmask} - Wandelt Netmask von DDN zu CIDR um")
  (print "cidr2ddn {CIDR netmask} - Wandelt Netmask von CIDR zu DDN um")
  (print "ip-string-to-number {ip-string} - Wandelt IP-Adresse von String in Integer um")
  (print "ip-number-to-string {interger} - Wandelt Interger in IP-Adresse um")
  (return-from get-help nil)
  )

