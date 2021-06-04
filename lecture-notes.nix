with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/278bcdce1f0da616661a6205161b13bd89a2f3bf.tar.gz) {};

let  
     myEmacs = emacsWithPackages (epkgs: with epkgs; [org dash]);
     myEmacsConfig = writeText "default.el" ''
       ; (require 'org-ref) # gah
       (require 'org)
       (require 'ox-latex)
       (load-file "org-hacks.el")
       ; (setq org-latex-subtitle-separate t)
       ; (setq org-latex-subtitle-format "%s")  ; insane hack for short title
       (setq org-latex-listings t)
       (setq org-latex-custom-lang-environments '((haskell "spec") (latex "comment")))
       (add-to-list 'org-latex-classes
       '("article" "\\documentclass[english,utf8]{article}"
          ("\\section{%s}" . "\\section*{%s}")
          ("\\subsection{%s}" . "\\subsection*{%s}")
          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
          ("\\paragraph{%s}" . "\\paragraph*{%s}")
          ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
     '';
in stdenv.mkDerivation {
  shellHook = ''
    export MYEMACSLOAD=${myEmacsConfig}
  '';
  name = "docsEnv";
  buildInputs = [ haskellPackages.lhs2tex
                  # python3Packages.pygments
                  myEmacs
                  biber
                  libertinus
                  inkscape
                  # zip
                  (texlive.combine {
                       inherit (texlive)
                       algorithm2e
                       biblatex
                       boondox
                       catchfile
                       comment
                       cleveref
                       environ
                       fontaxes
                       framed
                       fvextra
                       harvard
                       ifplatform
                       ifsym
                       inconsolata
                       kastrup
                       latexmk
                       libertine
                       listings
                       logreq
                       minted
                       makecell
                       multirow
                       mweights
                       ncclatex
                       ncctools
                       newtx
                       newtxsf
                       newtxtt
                       newunicodechar
                       prftree
                       relsize
                       scheme-small wrapfig marvosym wasysym
                       soul
                       stackengine
                       stmaryrd
                       svg
                       lazylist polytable # lhs2tex
                       todonotes
                       totpages
                       transparent
                       trimspaces
                       thmtools
                       ucs
                       wasy cm-super unicode-math filehook lm-math capt-of
                       xargs
                       xstring ucharcat
                       xypic
                       xifthen
                       ifmtarg
                       ;
                     })
                ];
}
