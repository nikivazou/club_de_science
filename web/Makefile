all:
	ghc --make site
	./site rebuild

clean:
	rm -rf *.hi *.o .*.swp .*.swo site _site/ _cache/

update:
	cp css/syntax-rj.css _site/css/syntax.css
	cp -r slides _site/
	cp -f lectures/*.lhs _site/lectures/
	scp -r _site/* nvazou@goto.ucsd.edu:public_html/club_de_science15/

