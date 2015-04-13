#!/bin/bash
mkdir -p ~/.bintray
eval "echo \"$(< ./project/bintray.template)\"" > ~/.bintray/.credentials
#openssl des3 -d -salt -in ./publish/sonatype.asc.enc -out ./publish/sonatype.asc -k "$SECRET"

#CORE_JSON=$(wget -qO- https://api.bintray.com/packages/stacycurl/repo/pimpathon-core/versions/_latest --user=stacycurl --password=$BINTRAY_API_KEY)
#LATEST_VERSION=$(echo $CORE_JSON | sed -e 's/\"//g' | sed -e 's/:/,/g' | cut -d, -f 2)
#NEXT_VERSION=$(echo $LATEST_VERSION | awk -F. -v OFS=. 'NF==1{print ++$NF}; NF>1{if(length($NF+1)>length($NF))$(NF-1)++; $NF=sprintf("%0*d", length($NF), ($NF+1)%(10^length($NF))); print}')
#echo $NEXT_VERSION > ~/.bintray/.version