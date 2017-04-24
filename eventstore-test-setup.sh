#!/bin/sh

wget http://download.geteventstore.com/binaries/EventStore-OSS-Ubuntu-14.04-v3.9.3.tar.gz
tar -xf EventStore-OSS-Ubuntu-14.04-v3.9.3.tar.gz
EventStore-OSS-Ubuntu-14.04-v3.9.3/run-node.sh --mem-db &