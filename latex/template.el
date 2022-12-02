;;; template.el --- A file with my LaTeX templates & classes. -*- lexical-binding: t; -*-

;;; Commentary:

;; This package provides my most used LaTeX files and other classes
;; I'll try to add over time.

;;; Code:

(require 'ox-latex)

  (add-to-list 'org-latex-classes
	       '("notes"
		 "\\documentclass[french, 12pt]{article}
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
\\usepackage[backref=page]{hyperref}
\\usepackage{adjustbox}
\\usepackage{subcaption}
\\hypersetup{
    colorlinks=true,
    linkcolor={PineGreen!50!black},
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
		 "\\documentclass[12pt]{article}
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
\\usepackage[backref=page]{hyperref}
\\hypersetup{
    colorlinks=true,
    linkcolor={PineGreen!30!black},
    citecolor={Bittersweet!50!Sepia},
    urlcolor={blue!80!black}}
\\usepackage{adjustbox}
\\usepackage{subcaption}
\\usepackage{geometry}
\\geometry{a4paper,left=2.5cm,top=1.2cm,right=2.5cm,bottom=1.5cm,marginparsep=7pt, marginparwidth=.6in}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
	     '("el-notes"
	       "[NO-DEFAULT-PACKAGES]
\\documentclass[11pt, leqno]{article}
\\usepackage[round]{natbib}
\\usepackage{fvextra}
\\usepackage{csquotes}
\\usepackage{parskip}
\\usepackage[p,osf]{ETbb} % osf in text, tabular lining figures in math
\\usepackage[scaled=.95,type1]{cabin} % sans serif in style of Gill Sans
\\usepackage[varqu,varl]{zi4}% inconsolata typewriter
\\usepackage[T1]{fontenc} % LY1 also works
\\usepackage[libertine,vvarbb]{newtxmath}
% --- Align title, author and dates left
\\usepackage{titling}
\\pretitle{\\begin{flushleft}\\huge\\rmfamily\\itshape}
\\posttitle{\\par\\end{flushleft}\\vskip 0.5em}
\\preauthor{\\begin{flushleft}\\normalsize}
\\postauthor{\\par\\end{flushleft}\\vskip 0.5em}
\\predate{\\begin{flushleft}\\small}
\\postdate{\\par\\end{flushleft}\\vskip 0.5em}
% ---
\\usepackage{sectsty}
\\sectionfont{\\LARGE\\itshape} % Italic heading
\\subsectionfont{\\Large}
\\subsubsectionfont{\\large}
\\paragraphfont{\\large}
\\usepackage[usenames,dvipsnames,svgnames]{xcolor}
\\usepackage{caption}
\\captionsetup{font={small, sc, onehalfspacing}}
\\usepackage[backref=page]{hyperref}
\\hypersetup{
    colorlinks=true,
    linkcolor={RoyalPurple},
    citecolor={Bittersweet!50!Sepia},
    urlcolor={CornflowerBlue!50!Blue}}
\\usepackage{adjustbox}
\\usepackage{mparhack} % Fixes some margins issues
\\usepackage{geometry}
\\geometry{a4paper,left=1cm,top=2cm,right=4cm,bottom=2cm,marginparsep=0.5cm, marginparwidth=3.5cm}"
	      ("\\section{%s}" . "\\section*{%s}")
	      ("\\subsection{%s}" . "\\subsection*{%s}")
	      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
	     '("el-book"
	       "[NO-DEFAULT-PACKAGES]
\\documentclass[11pt, leqno, oneside]{book}
\\usepackage[round]{natbib}
\\usepackage{listings}
\\usepackage{fvextra}
\\usepackage{csquotes}
\\usepackage{parskip}
\\usepackage[p,osf]{ETbb} % osf in text, tabular lining figures in math
\\usepackage[scaled=.95,type1]{cabin} % sans serif in style of Gill Sans
\\usepackage[varqu,varl]{zi4}% inconsolata typewriter
\\usepackage[T1]{fontenc} % LY1 also works
\\usepackage[libertine,vvarbb]{newtxmath}
% --- Align title, author and dates left
\\usepackage{titling}
\\pretitle{\\begin{flushleft}\\huge\\rmfamily\\itshape}
\\posttitle{\\par\\end{flushleft}\\vskip 0.5em}
\\preauthor{\\begin{flushleft}\\normalsize}
\\postauthor{\\par\\end{flushleft}\\vskip 0.5em}
\\predate{\\begin{flushleft}\\small}
\\postdate{\\par\\end{flushleft}\\vskip 0.5em}
% ---
\\usepackage{sectsty}
\\sectionfont{\\LARGE\\itshape} % Italic heading
\\subsectionfont{\\Large}
\\subsubsectionfont{\\large}
\\paragraphfont{\\large}
\\usepackage[usenames,dvipsnames,svgnames]{xcolor}
\\usepackage{caption}
\\captionsetup{font={small, sc, onehalfspacing}}
\\usepackage[backref=page]{hyperref}
\\hypersetup{
    colorlinks=true,
    linkcolor={RoyalPurple},
    citecolor={Bittersweet!50!Sepia},
    urlcolor={CornflowerBlue!50!Blue}}
\\usepackage{adjustbox}
\\usepackage{mparhack} % Fixes some margins issues
\\usepackage{geometry}
\\geometry{a4paper,left=1cm,top=2cm,right=4cm,bottom=2cm,marginparsep=0.5cm, marginparwidth=3.5cm}"
	      ("\\chapter{%s}" . "\\chapter*{%s}")
	      ("\\section{%s}" . "\\section*{%s}")
	      ("\\subsection{%s}" . "\\subsection*{%s}")
	      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
  ;; beamer class, for presentations
  '("el-presentation"
     "\\documentclass[11pt]{beamer}
      \\usetheme[progressbar=frametitle]{metropolis}
      \\setbeameroption{show notes}
      \\setbeamertemplate{section in toc}[sections numbered]
      \\usepackage[utf8]{inputenc}
      \\usepackage{appendixnumberbeamer}
      \\usepackage[T1]{fontenc}
      \\usepackage{hyperref}
      \\usepackage{color}
      \\usepackage{verbatim}"
     
     ("\\section{%s}" . "\\section*{%s}")
     
     ("\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}"
       "\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}")))
;;; Footer
(provide 'template)
;;; template.el ends here
