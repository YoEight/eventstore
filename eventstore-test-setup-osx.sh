#!/bin/sh

curl -o EventStore-OSS-MacOSX-v3.9.3.tar.gz http://download.geteventstore.com/binaries/EventStore-OSS-MacOSX-v3.9.3.tar.gz
tar -xf EventStore-OSS-MacOSX-v3.9.3.tar.gz
EventStore-OSS-MacOSX-v3.9.3/run-node.sh --mem-db &