all: submodule figures pdf clear

submodule:
	git submodule update --init ./images/

figures:
	make -C ./figures/

pdf:
	pdflatex 2018_09_14-trueSkill.tex
	pdflatex 2018_09_14-trueSkill.tex
	pdflatex 2018_09_14-trueSkill.tex
	pdflatex 2018_09_14-trueSkill.tex

clean: 
	- rm -f *.log
	- rm -f *.soc
	- rm -f *.toc
	- rm -f *.aux
	- rm -f *.out
	- rm -f main.idx
	- rm -f *.bbl
	- rm -f *.bbg
	- rm -f *.dvi
	- rm -f *.blg
	- rm -f *.lof
	- rm -f *.nav
	- rm -f *.snm
	- rm -f *~



