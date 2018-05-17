#!/bin/bash
## Get version
FILE=$1
FILESTUB=${FILE%.tex*}
VERSION=${FILESTUB##*_}
echo $VERSION
## Compile docx using pandoc
pandoc -s --bibliography=bibliography.bib --csl=plos.csl $FILE -o superlearner_vs_clinicians_manuscript_$VERSION.docx
## Clean directory
rm superlearner_vs_clinicians_manuscript_*.aux superlearner_vs_clinicians_manuscript_*.bbl superlearner_vs_clinicians_manuscript_*.blg superlearner_vs_clinicians_manuscript_*.log superlearner_vs_clinicians_manuscript_*.out
