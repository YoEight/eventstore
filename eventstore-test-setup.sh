#!/bin/sh

wget http://download.geteventstore.com/binaries/EventStore-OSS-Ubuntu-v3.3.0.tar.gz
tar -xf EventStore-OSS-Ubuntu-v3.3.0.tar.gz
EventStore-OSS-Ubuntu-v3.3.0/run-node.sh --mem-db &
