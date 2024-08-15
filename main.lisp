(ql:quickload :ltk)
(use-package :ltk)

(defun buttons()
  (with-ltk ()
   (let* ((f (make-instance 'frame))
          (umlauts '(("Ä" "ä") ("Ö" "ö") ("Ü" "ü") ("Ë" "ë"))))
     
     (pack f)

     (dolist (row umlauts)
       (let ((row-frame (make-instance 'frame :master f))) 
         (dolist (char row)
           (let ((button (make-instance 'button
                                        :master row-frame
                                        :text char
                                        :command (lambda ()
                                                   (progn
                                                      ;; Clipboard
                                                      (format-wish "clipboard clear; clipboard append ~S" char))))))
             (pack button :side :left)))
         (pack row-frame :side :top)))

     (configure f :borderwidth 3)
     (configure f :relief :sunken))))

(buttons)
