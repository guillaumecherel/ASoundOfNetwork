#!/bin/sh

bower install && \
pulp browserify -t www/ASoundOfNetwork.js --standalone aSoundOfNetwork
