;;
;; Correios - Brazillian post office service shipping tax calculation
;; http://www.correios.com.br/webServices/PDF/SCPP_manual_implementacao_calculo_remoto_de_precos_e_prazos.pdf
;;
; Copyright (c) 2013, Arthur Maciel
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
;
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
; 3. Neither the name of the author nor the names of its
;    contributors may be used to endorse or promote products derived
;    from this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
; COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGE.
;
; Please report bugs and suggestions to arthurmaciel at gmail dot com

(module correios 
 (process-request
  valid-response?

  make-request request?
  request-service request-service-set!
  request-from-zip request-from-zip-set!
  request-to-zip request-to-zip-set!
  request-company request-company-set!
  request-password request-password-set!
  request-pkg-weight request-pkg-weight-set!
  request-pkg-format request-pkg-format-set!
  request-pkg-length request-pkg-length-set!
  request-pkg-height request-pkg-height-set!
  request-pkg-breadth request-pkg-breadth-set!
  request-pkg-diameter request-pkg-diameter-set!	  
  request-receiver-id-check request-receiver-id-check-set!
  request-return-url request-return-url-set!
  request-declared-value request-declared-value-set!
  request-return-receipt request-return-receipt-set!

  make-response response?
  response-service response-service-set! 
  response-cost response-cost-set! 
  response-delivery-time response-delivery-time-set! 
  response-receiver-id-check-cost response-receiver-id-check-cost-set!
  response-return-receipt-cost response-return-receipt-cost-set! 
  response-declared-value-cost response-declared-value-cost-set! 
  response-home-delivery response-home-delivery-set!
  response-sunday-delivery response-sunday-delivery-set! 
  response-error response-error-set! 
  response-error-msg response-error-msg-set!
  )

(import chicken scheme)
(use srfi-1 defstruct http-client ssax uri-common)

(define host "ws.correios.com.br")
(define path '("" "calculador" "CalcPrecoPrazo.aspx"))
(define service-types
  '((PAC . 41106)
    (SEDEX . 40010)
    (SEDEX_10 . 40215)
    (SEDEX_HOJE . 40290)
    (SEDEX_A_COBRAR . 40045)))

(define (service-name->number name) 
  (let ((result (assoc name service-types)))
    (if result
	(cdr result)
	(error "Unkown posting service name"))))

(define (service-number->name number) 
  (let* ((number (string->number number))
	 (result (find (lambda (s) (eq? (cdr s) number)) 
		       service-types)))
    (if result
	(car result)
	(error "Unkown posting service number"))))

(defstruct request
  service from-zip to-zip (company "") (password "")  
  (pkg-weight 0.3) (pkg-format 1) (pkg-length 16) 
  (pkg-height 2) (pkg-breadth 11) (pkg-diameter 0) 
  (receiver-id-check #f) (declared-value 0) 
  (return-receipt #f) (return-url "XML"))

(define-record-printer (request sr out)
  (fprintf out
      "#<request service: ~S from-zip: ~S
       to-zip: ~S company: ~S password: ~S pkg-weight: ~S
       pkg-format: ~S pkg-length: ~S pkg-height: ~S
       pkg-breadth: ~S pkg-diameter: ~S receiver-id-check: ~S
       declared-value: ~S return-receipt: ~S return-url: ~S>"
    (request-service sr)
    (request-from-zip sr)
    (request-to-zip sr)
    (request-company sr)
    (request-password sr)
    (request-pkg-weight sr)
    (request-pkg-format sr)
    (request-pkg-length sr)
    (request-pkg-height sr)
    (request-pkg-breadth sr)
    (request-pkg-diameter sr)	  
    (request-receiver-id-check sr)
    (request-declared-value sr)
    (request-return-receipt sr)
    (request-return-url sr)))

(defstruct response
  service cost delivery-time receiver-id-check-cost
  return-receipt-cost declared-value-cost home-delivery
  sunday-delivery error error-msg)

(define-record-printer (response sr out)
  (fprintf out
      "#<response service: ~S cost: ~S delivery-time: ~S 
       receiver-id-check-cost: ~S return-receipt-cost: ~S 
       declared-value-cost: ~S home-delivery: ~S sunday-delivery: ~S 
       error: ~S error-msg: ~S>"
    (response-service sr)
    (response-cost sr)
    (response-delivery-time sr)
    (response-receiver-id-check-cost sr)
    (response-return-receipt-cost sr)
    (response-declared-value-cost sr)
    (response-home-delivery sr)
    (response-sunday-delivery sr)
    (response-error sr)
    (response-error-msg sr)))

(define (boolean->correios-string bool)
  (if bool "S" "N"))

(define (correios-string->boolean string)
  (if (string=? string "S") #t #f))

(define (compose-url req)
  (form-urlencoded-separator "&")
  (make-uri 
   scheme: 'http
   host: host 
   path: path
   query:
   `((nCdEmpresa . ,(request-company req))
     (sDsSenha . ,(request-password req))
     (nCdServico . ,(let* ((service (request-service req))
			   (code-list (if (atom? service) 
					  (list service) 
					  service)))
		      (string-intersperse 
		       (map (lambda (s)
			      (->string (service-name->number s))) 
			    code-list)  
		       ",")))
     (sCepOrigem . ,(request-from-zip req))
     (sCepDestino . ,(request-to-zip req))
     (nVlPeso . ,(request-pkg-weight req))
     (nCdFormato . ,(request-pkg-format req))
     (nVlComprimento . ,(request-pkg-length req))
     (nVlAltura . ,(request-pkg-height req))
     (nVlLargura . ,(request-pkg-breadth req))
     (nVlDiametro . ,(request-pkg-diameter req))
     (sCdMaoPropria . ,(boolean->correios-string
			(request-receiver-id-check req)))
     (nVlValorDeclarado . ,(request-declared-value req))
     (sCdAvisoRecebimento . ,(boolean->correios-string
			      (request-return-receipt req)))
     (StrRetorno . ,(request-return-url req)))))

(define (get-value key alist)
  (let ((v (cdr (assoc key alist))))
    (if (null? v) #f v)))

(define (service-alist->response service)
  (make-response 
   service: (service-number->name (get-value 'Codigo service))
   cost: (get-value 'Valor service)
   delivery-time: (get-value 'PrazoEntrega service)
   receiver-id-check-cost: (get-value 'ValorMaoPropria service)
   return-receipt-cost: (get-value 'ValorAvisoRecebimento service)
   declared-value-cost: (get-value 'ValorValorDeclarado service)
   home-delivery: (get-value 'EntregaDomiciliar service)
   sunday-delivery: (correios-string->boolean (get-value 'EntregaSabado service))
   error: (get-value 'Erro service)
   error-msg: (get-value 'MsgErro service)))

(define (response->proper-alist response)
  (map (lambda (service)
	 (map (lambda (item)
		(cons (car item)
		      (if (equal? (cdr item) '())
			  '()
			  (cadr item))))
	      (cdr service)))
       response))

(define (process-request req)
  (let* ((url (compose-url req))
	 (data
	  (let-values (((xml uri resp) 
			(with-input-from-request url #f read-string)))
	    (call-with-input-string xml
				    (lambda (port)
				      (ssax:xml->sxml port '()))))))
    (map service-alist->response 
	 (response->proper-alist 
	  ;; from XML response return only the list of services
	  (cdaddr data))))) 

(define (valid-response? response)
  (string=? "0" (response-error response)))

) ;; End of module correios
