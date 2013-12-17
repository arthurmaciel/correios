(use test 
     (prefix correios correios:))

(define ship-req (make-request 
		  service: (list 'SEDEX 'PAC)
		  from-zip: "05412002" 
		  to-zip: "90035120"))

(define responses (process-request ship-req))

(for-each (lambda (r)
	    (test-assert "process-request returns a list of responses"
			 (reponse? r)))
	  responses)

;; (let loop ((responses (process-request ship-req)))
;;   (cond ((null? responses) 
;; 	 (printf "Finished processing responses.~N"))
;; 	(else
;; 	 (let ((response (car responses)))
;; 	   (if (valid-response? response)
;; 	       (printf "Service ~A - Cost: ~A   Estimated delivery (days): ~A~N" 
;; 		 (response-service response)
;; 		 (response-cost response)
;; 		 (response-delivery-time response))
;; 	       (printf "Service ~A - Error:  ~A" 
;; 		 (response-service response)
;; 		 (response-error-msg response))))
;; 	 (loop (cdr responses)))))

(test-exit)
