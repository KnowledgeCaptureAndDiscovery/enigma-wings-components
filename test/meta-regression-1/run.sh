#!/bin/bash
BASEDIR=`dirname $0`
TEMP=$(mktemp -d)

# Copy scripts to temp directory
cp ../../visualizations/shiny-only-forest.R $TEMP/app.R
cp ../../visualizations/publish.R $TEMP/publish.R
cp ../data/mergedFile-2 $TEMP/data.csv
cp input_var.txt $TEMP/input_var.txt

# Name of the application to be published. Must start with /
# Use a variable if you want different visualizations per execution.
APPNAME="/forestplot_$(date +%s)"

#Creating the image:
#I've changed this name as is a image that includes more packages (see Dockerfile)
#docker build -t shiny-extras $BASEDIR
#Should be commented after creating the shiny-extras image.

effect='EFFECT'
demographic='HasAgeMean'
NonEuro=(GOBS,IMH,UNICAMP,Meth-CT,MIRECC,Meth-CT,MIRECC,UKBB_NonEuropean,OSAKA,PING_NonEuropean,UKBB,ABCD)
cohort_size='N'
sample_size='SAMPLE_SIZE'
CI_LBB='CI_LB'
CI_UBB='CI_UB'
cols=($effect,$sample_size,$demographic,$CI_LBB,$CI_UBB,N,TOTAL_N,PCT)
Area='Precentral'
Trait=(Surface,Area)
SNP='rs1080066'
min_val=0
max_val=120
demographic_annot=(Mean,Age)


#Running the container. Coping tmp to appname and publishing everithing on $TEMP
docker run -v $TEMP:$APPNAME --rm shiny-extras /bin/sh -c "cd $APPNAME; Rscript publish.R ${effect} ${demographic} ${NonEuro} ${cohort_size} ${cols} ${sample_size} ${CI_LBB} ${CI_UBB} ${Area} ${Trait} ${SNP} ${min_val} ${max_val} ${demographic_annot} > publish.log"

#creating shiny config
echo "{\"url\": \"$(tail -1 $TEMP/publish.log | sed 's/^.* //g')\"}" > shinyViz.txt
cat shinyViz.txt
