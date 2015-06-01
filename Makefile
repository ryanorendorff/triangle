# This is to make the stupid expansion in clean work.
SHELL=/bin/bash

#######################################################################
#                               Targets                               #
#######################################################################
.PHONY: pdf tex wiki run zip clean

pdf: triangle.pdf
triangle.pdf: Triangle.lhs template.tex Makefile
	@echo "Making pdf"
	@pandoc --template=template.tex Triangle.lhs -o triangle.pdf \
		--latex-engine=xelatex --variable monofont='PragmataPro'

tex: triangle.tex
triangle.tex: Triangle.lhs template.tex Makefile
	@echo "Making tex"
	@pandoc --template=template.tex Triangle.lhs -o triangle.tex \
		--latex-engine=xelatex --variable monofont='PragmataPro'

wiki: triangle.wiki zip
triangle.wiki: Triangle.lhs Makefile
	@echo "Making wiki"
	@pandoc Triangle.lhs -t markdown | sed "s/~~~~ {.*/\`\`\`haskell/g" | \
		sed "s/~~~~/\`\`\`/g" | sed 's/\\long\\def\\ignore.*//g' | \
		tail -n +20 > triangle.wiki

run:
	@runhaskell main.hs

zip: triangle.zip
triangle.zip: triangle.pdf Makefile main.hs template.tex test.triangle craigslist.triangle
	@echo "Making zip"
	@mkdir triangle
	@cp Makefile Triangle.lhs main.hs template.tex test.triangle \
		craigslist.triangle triangle
	@zip -r -FSr triangle.zip triangle > /dev/null
	-@rm -R triangle

clean:
	-rm -Rf triangle{,.zip,.wiki,.md,.tex,.pdf}
