#!/bin/bash
## Compile docx using pandoc
pandoc -s --bibliography=bibliography.bib --csl=plos.csl superlearner_vs_clinicians_manuscript.tex -o superlearner_vs_clinicians_manuscript.docx
## Clean directory
rm superlearner_vs_clinicians_manuscript.aux superlearner_vs_clinicians_manuscript.bbl superlearner_vs_clinicians_manuscript.blg superlearner_vs_clinicians_manuscript.log superlearner_vs_clinicians_manuscript.out
