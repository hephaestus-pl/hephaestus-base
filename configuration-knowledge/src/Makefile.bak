all:
	happy -gca ConfigurationKnowledge/Parsers/ParCK.y
	alex -g ConfigurationKnowledge/Parsers/LexCK.x
	(cd ConfigurationKnowledge/Parsers/; latex DocCK.tex; dvips DocCK.dvi -o DocCK.ps)
	ghc --make ConfigurationKnowledge/Parsers/TestCK.hs -o ConfigurationKnowledge/Parsers/TestCK
clean:
	-rm -f ConfigurationKnowledge/Parsers/*.log ConfigurationKnowledge/Parsers/*.aux ConfigurationKnowledge/Parsers/*.hi ConfigurationKnowledge/Parsers/*.o ConfigurationKnowledge/Parsers/*.dvi
	-rm -f ConfigurationKnowledge/Parsers/DocCK.ps
distclean: clean
	-rm -f ConfigurationKnowledge/Parsers/DocCK.* ConfigurationKnowledge/Parsers/LexCK.* ConfigurationKnowledge/Parsers/ParCK.* ConfigurationKnowledge/Parsers/LayoutCK.* ConfigurationKnowledge/Parsers/SkelCK.* ConfigurationKnowledge/Parsers/PrintCK.* ConfigurationKnowledge/Parsers/TestCK.* ConfigurationKnowledge/Parsers/AbsCK.* ConfigurationKnowledge/Parsers/TestCK ConfigurationKnowledge/Parsers/ErrM.* ConfigurationKnowledge/Parsers/SharedString.* ConfigurationKnowledge/Parsers/CK.dtd ConfigurationKnowledge/Parsers/XMLCK.* Makefile*
	-rmdir -p ConfigurationKnowledge/Parsers/
