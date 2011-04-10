#!/bin/sh
rm *.adb.html
gnat make adamain.adb && \
./adamain adamain.adb
