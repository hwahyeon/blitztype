(ql:quickload :ltk)
(use-package :ltk)

(defun buttons()
  (with-ltk ()
    (wm-title *tk* "Umlaut Buttons")

    (let* ((f (make-instance 'frame))
           (info-label (make-instance 'label :master f :text "Click a button to copy")))

      (dolist (row '(("Ä" "ä") ("Ö" "ö") ("Ü" "ü") ("Ë" "ë")))
        (let ((row-frame (make-instance 'frame :master f)))
          (dolist (char row)
            (let ((button (make-instance 'button
                                         :master row-frame
                                         :text char
                                         :command (lambda ()
                                                    (format-wish "clipboard clear; clipboard append ~S" char)))))
              (pack button :side :left)))
          (pack row-frame :side :top)))

      (pack info-label :side :bottom :fill :x)
      (configure info-label :anchor :center)

      (pack f :side :top :fill :both :expand t))))

(buttons)
