#!/bin/bash
## Compile docx using pandoc
pandoc -s --bibliography=bibliography.bib --csl=plos.csl superlearner_vs_clinicians_manuscript.tex -o superlearner_vs_clinicians_manuscript.docx
## Clean directory
rm superlearner_vs_clinicians_manuscript_*.aux superlearner_vs_clinicians_manuscript_*.bbl superlearner_vs_clinicians_manuscript_*.blg superlearner_vs_clinicians_manuscript_*.log superlearner_vs_clinicians_manuscript_*.out
