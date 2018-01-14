◊(local-require racket/file racket/system)
◊(define latex-source ◊(make-latex-source doc))
◊(define working-directory
    (build-path (current-directory) "pollen-latex-work"))
◊(unless (directory-exists? working-directory)
    (make-directory working-directory))
◊(define temp-ltx-path (build-path working-directory "temp.ltx"))
◊(display-to-file latex-source temp-ltx-path #:exists 'replace)
◊(define command (format "pdflatex -interaction=nonstopmode -output-directory '~a' '~a'"
  working-directory temp-ltx-path))
◊(if (system command)
    (file->bytes (build-path working-directory "temp.pdf"))
    (error "pdflatex: rendering error"))
