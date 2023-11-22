; additional superscript and subscript
; unicode characters with C-x 8

(defun more-ctl-x-8-superscript ()
  "Add more unicode superscript characters to C-x 8."
  (define-key 'iso-transl-ctl-x-8-map "^0" [#x2070])
  ;; the next three keys are already defined in emacs
  ;; (define-key 'iso-transl-ctl-x-8-map "^1" [#x00B9])
  ;; (define-key 'iso-transl-ctl-x-8-map "^2" [#x00B2])
  ;; (define-key 'iso-transl-ctl-x-8-map "^3" [#x00B3])
  (define-key 'iso-transl-ctl-x-8-map "^4" [#x2074])
  (define-key 'iso-transl-ctl-x-8-map "^5" [#x2075])
  (define-key 'iso-transl-ctl-x-8-map "^6" [#x2076])
  (define-key 'iso-transl-ctl-x-8-map "^7" [#x2077])
  (define-key 'iso-transl-ctl-x-8-map "^8" [#x2078])
  (define-key 'iso-transl-ctl-x-8-map "^9" [#x2079])

  (define-key 'iso-transl-ctl-x-8-map "^+" [#x207A])
  (define-key 'iso-transl-ctl-x-8-map "^-" [#x207B])
  (define-key 'iso-transl-ctl-x-8-map "^=" [#x207C])
  (define-key 'iso-transl-ctl-x-8-map "^(" [#x207D])
  (define-key 'iso-transl-ctl-x-8-map "^)" [#x207E])
  (define-key 'iso-transl-ctl-x-8-map "^n" [#x207F])
  (define-key 'iso-transl-ctl-x-8-map "^i" [#x2071]))

(defun more-ctl-x-8-subscript ()
  "Add more unicode subscript characters to C-x 8."
  (define-key 'iso-transl-ctl-x-8-map "_0" [#x2080])
  (define-key 'iso-transl-ctl-x-8-map "_1" [#x2081])
  (define-key 'iso-transl-ctl-x-8-map "_2" [#x2082])
  (define-key 'iso-transl-ctl-x-8-map "_3" [#x2083])
  (define-key 'iso-transl-ctl-x-8-map "_4" [#x2084])
  (define-key 'iso-transl-ctl-x-8-map "_5" [#x2085])
  (define-key 'iso-transl-ctl-x-8-map "_6" [#x2086])
  (define-key 'iso-transl-ctl-x-8-map "_7" [#x2087])
  (define-key 'iso-transl-ctl-x-8-map "_8" [#x2088])
  (define-key 'iso-transl-ctl-x-8-map "_9" [#x2089])

  (define-key 'iso-transl-ctl-x-8-map "_+" [#x208A])
  (define-key 'iso-transl-ctl-x-8-map "_-" [#x208B])
  (define-key 'iso-transl-ctl-x-8-map "_=" [#x208C])
  (define-key 'iso-transl-ctl-x-8-map "_(" [#x208D])
  (define-key 'iso-transl-ctl-x-8-map "_)" [#x208E])
  (define-key 'iso-transl-ctl-x-8-map "_n" [#x2099]))

(more-ctl-x-8-superscript)
(more-ctl-x-8-subscript)


(provide 'gg-unicode)
