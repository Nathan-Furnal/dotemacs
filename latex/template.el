;;; template.el --- A file with my LaTeX templates & classes. -*- lexical-binding: t; -*-

;;; Commentary:

;; This package provides my most used LaTeX files and other classes
;; I'll try to add over time.

;;; Code:

(require 'ox-latex)

  (add-to-list 'org-latex-classes
	       '("notes"
		 "\\documentclass[french]{article}
\\usepackage{minted}
\\usepackage[round]{natbib}
\\usepackage{babel}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{mathptmx}
\\usepackage{booktabs}
\\usepackage{longtable}
\\usepackage[skip=1.\\baselineskip]{caption}
\\usepackage{soul}
\\usepackage[usenames,dvipsnames,svgnames]{xcolor}
\\usepackage{parskip}
\\usepackage[many]{tcolorbox}
\\usepackage{hyperref}
\\usepackage[export]{adjustbox}
\\usepackage{subcaption}
\\hypersetup{
    colorlinks=true,
    linkcolor={PineGreen!30!black},
    citecolor={Bittersweet!50!Sepia},
    urlcolor={blue!80!black}}
\\usepackage{geometry}
\\geometry{a4paper,left=2.5cm,top=1.2cm,right=2.5cm,bottom=1.5cm,marginparsep=7pt, marginparwidth=.6in}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
	       '("notes_en"
		 "\\documentclass{article}
\\usepackage{minted}
\\usepackage[round]{natbib}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{mathptmx}
\\usepackage{booktabs}
\\usepackage{longtable}
\\usepackage[skip=1.\\baselineskip]{caption}
\\usepackage{soul}
\\usepackage[usenames,dvipsnames,svgnames]{xcolor}
\\usepackage{parskip}
\\usepackage[many]{tcolorbox}
\\usepackage{hyperref}
\\hypersetup{
    colorlinks=true,
    linkcolor={PineGreen!30!black},
    citecolor={Bittersweet!50!Sepia},
    urlcolor={blue!80!black}}
\\usepackage[export]{adjustbox}
\\usepackage{subcaption}
\\usepackage{geometry}
\\geometry{a4paper,left=2.5cm,top=1.2cm,right=2.5cm,bottom=1.5cm,marginparsep=7pt, marginparwidth=.6in}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;; Footer
(provide 'template)
;;; template.el ends here
