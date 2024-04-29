(use-package ox
  :custom
  (org-export-with-toc nil)
  (org-export-with-tags 'not-in-toc)
  (org-export-with-drawers nil)
  (org-export-with-priority nil)
  (org-export-with-footnotes nil)
  (org-export-with-smart-quotes nil)
  (org-export-with-section-numbers t)
  (org-export-with-sub-superscripts '{})
  (org-export-with-date nil)
  (org-export-with-author nil)
  ;; `org-export-use-babel' set to nil will cause all source block header arguments to be ignored This means that code blocks with the argument :exports none or :exports results will end up in the export.
  ;; See:
  ;; https://stackoverflow.com/questions/29952543/how-do-i-prevent-org-mode-from-executing-all-of-the-babel-source-blocks
  (org-export-use-babel nil)
  (org-export-headline-levels 9)
  (org-export-coding-system 'utf-8)
  (org-export-with-broken-links 'mark)
  (org-export-default-language "zh-CN") ; 默认是en
  ;; (org-ascii-text-width 72)
  )

(use-package ox-latex
  :config
  ;; 将 latex 转为 pdf 的程序，多次执行是为了解决交叉引用问题
  ;; -interaction nonstopmode 是尽可能多的编译
  ;; (setq org-latex-pdf-process
  ;;       '("xelatex -interaction nonstopmode -output-directory %o %f"
  ;;         "bibtex %b"
  ;;         "xelatex -interaction nonstopmode -output-directory %o %f"
  ;;         "xelatex -interaction nonstopmode -output-directory %o %f"
  ;;         "rm -fr %b.out %b.log %b.tex %b.brf %b.bbl auto"
  ;;         ))
  (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
  ;; 生成 PDF 后清理辅助文件
  ;; https://answer-id.com/53623039
  (setq org-latex-logfiles-extensions
        (quote ("lof" "lot" "tex~" "tex" "aux"
                "idx" "log" "out" "toc" "nav"
                "snm" "vrb" "dvi" "fdb_latexmk"
                "blg" "brf" "fls" "entoc" "ps"
                "spl" "bbl" "xdv")))
  ;; 不要自动创建备份文件
  (setq make-backup-files nil)
  ;; 将默认格式设置为 cn-article
  (setq org-latex-default-class "cn-article")
  (add-to-list 'org-latex-classes
               '("cn-article"
                 "
\\documentclass[12pt, a4paper]{article}
\\usepackage{fontenc}
\\usepackage[slantfont, boldfont]{xeCJK}

% set up English font
\\setmainfont[BoldFont=Sarasa Mono SC Bold]{Source Han Serif CN}
\\setmonofont{JetBrainsMono Nerd Font Mono}
\\setsansfont{Comic Sans MS}

% set up Chinese font, the font must be valid on your system
\\setCJKmainfont[BoldFont=Sarasa Mono SC Bold]{TsangerJinKai05}
\\setCJKmonofont{JetBrainsMono Nerd Font Mono}
\\setCJKsansfont{YouYuan}

% correct line break for chinese
\\XeTeXlinebreaklocale \"zh\"
\\XeTeXlinebreakskip = 0pt plus 1pt

\\linespread{1.2}
%页边距
\\usepackage[a4paper]{geometry}
\\geometry{verbose,
  tmargin=2cm,% 上边距
  bmargin=2cm,% 下边距
  lmargin=2cm,% 左边距
  rmargin=2cm % 右边距
}
\\usepackage{fvextra}
\\fvset{tabsize=4}

\\usepackage{booktabs}
\\usepackage{tikz}
\\let\\oldtexttt\\texttt
\\renewcommand{\\texttt}[1]{\\tikz[baseline=(MeNode.base)]{\\node[rounded corners, fill=gray!20](MeNode){#1};}}

\\usepackage{indentfirst}
\\setlength{\\parindent}{2em}

\\usepackage{zhnumber}
\\usepackage{titlesec} %自定义多级标题格式的宏包
\\titleformat{\\section}[block]{\\Large\\bfseries}{\\zhnum{section}、}{0em}{}[]
\\titleformat{\\subsection}[block]{\\Large\\bfseries}{\\arabic{section}.\\arabic{subsection}}{0.5em}{}[]
\\titleformat{\\subsubsection}[block]{\\normalsize\\bfseries}{\\arabic{section}.\\arabic{subsection}.\\arabic{subsubsection}}{0.5em}{}[]
\\titleformat{\\paragraph}[block]{\\normalsize\\bfseries}{\\arabic{section}.\\arabic{subsection}.\\arabic{subsubsection}.\\arabic{paragraph}}{0.5em}{}[]

\\usepackage{titling}
\\pretitle{
  \\vspace{-2em}
  \\begin{center}
    \\Huge\\bfseries
}%, make the fonts bigger, make the title (only) bold
\\posttitle{%
  \\end{center}%
  \\noindent\\vrule height 1.5pt width \\textwidth
  \\vspace{-4em}
}
\\renewcommand\\maketitlehooka{
  \\noindent\\vrule height 1.5pt width \\textwidth
}

\\usepackage{enumitem}
\\setenumerate[1]{itemsep=0pt,partopsep=0pt,parsep=\\parskip,topsep=5pt}
\\setitemize[1]{itemsep=0pt,partopsep=0pt,parsep=\\parskip,topsep=5pt}
\\setdescription{itemsep=0pt,partopsep=0pt,parsep=\\parskip,topsep=5pt}
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; 如果 org 设置了#+LATEX_HEADER: \usepackage{minted} 的话 则支持代码高亮和代码格式化
  (setq org-latex-src-block-backend 'engraved)
  ;; (setq org-latex-default-table-environment "tabu")

  ;; 使用 Listings 宏包格式化源代码(只是把代码框用 listing 环境框起来，还需要额外的设置)
  ;; (setq org-latex-listings t)
  ;; (add-to-list 'org-latex-packages-alist '("minted"))
  ;; Options for \lset command（reference to listing Manual)
  )

(use-package engrave-faces
  :straight t)

;; (use-package ox-pandoc
;;   :straight t)

;; (with-eval-after-load 'ox-latex
;;   ;; http://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export
;;   ;; latexmk runs pdflatex/xelatex (whatever is specified) multiple times
;;   ;; automatically to resolve the cross-references.
;;   (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
;;   (add-to-list 'org-latex-classes
;;                '("elegantpaper"
;;                  "\\documentclass[lang=cn]{elegantpaper}
;;                    [NO-DEFAULT-PACKAGES]
;;                    [PACKAGES]
;;                    [EXTRA]"
;;                  ("\\section{%s}" . "\\section*{%s}")
;;                  ("\\subsection{%s}" . "\\subsection*{%s}")
;;                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;;
;;   ;; 如果 org 设置了#+LATEX_HEADER: \usepackage{minted} 的话 则支持代码高亮和代码格式化
;;   (setq org-latex-listings 'minted)
;;   ;; Windows 加了这句的话 org 里 C-c C-x C-l 无法预览公式
;;   ;; 为了兼容性就不加这句 然后 org 里加上 #+LATEX_HEADER: \usepackage{minted} 即可
;;   ;; (add-to-list 'org-latex-packages-alist '("minted"))
;;
;;   ;; 解决 org 文件里面执行  org-latex-preview 生成的 .tex 格式不对的问题
;;   ;; 通过 log 可以看到错误信息 ! Package minted Error: You must invoke LaTeX with the -shell-escape flag
;;   )
