default:
	nix-shell lecture-notes.nix --pure --run "make ESSLLI.pdf"

%.pdf: %.tex *.svg
	latexmk -xelatex -shell-escape $<

%.tex: %.lhs
	lhs2TeX -o $@ $<

%.lhs: %.org
	emacs  --batch --eval="(load \"${MYEMACSLOAD}\")" $< -f org-latex-export-to-latex --kill
	mv -f $*.tex $@

clean:
	rm *.pdf ESSLLI.tex


