#!/bin/bash
BASEDIR=`dirname $0`
TEMP=$(mktemp -d)

# Copy scripts to temp directory
cp ../../visualizations/shiny-forest-no-demographic.R $TEMP/app.R
cp ../../visualizations/publish.R $TEMP/publish.R
cp ../data/mergedFile-1 $TEMP/data.csv

# Name of the application to be published. Must start with /
# Use a variable if you want different visualizations per execution.
APPNAME="/forestplot_$(date +%s)"

#Creating the image:
#I've changed this name as is a image that includes more packages (see Dockerfile)
docker build -t shiny-extras $BASEDIR
#Should be commented after creating the shiny-extras image.

#Running the container. Coping tmp to appname and publishing everithing on $TEMP
docker run -v $TEMP:$APPNAME --rm shiny-extras /bin/sh -c "cd $APPNAME; Rscript publish.R > publish.log"

#creating shiny config
echo "{\"url\": \"$(tail -1 $TEMP/publish.log | sed 's/^.* //g')\"}" > $OUTPUTS1
cat $OUTPUTS1
