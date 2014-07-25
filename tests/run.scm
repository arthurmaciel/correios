(use test correios)

(define ship-req (make-request 
		  service: (list 'SEDEX 'PAC)
		  from-zip: "05412002" 
		  to-zip: "90035120"))

(define responses (process-request ship-req))

(for-each (lambda (r)
	    (test-assert "Object is a correios response" (response? r)))
	  responses)

(test-exit)
