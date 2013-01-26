#!/bin/sh

for i in $@
do
	echo $i
	case $i in
		cpp)
			c++ -O2 1330.cpp -o cpp.bin;;
		go)
			go build -o go.bin 1330.go;;
		hs)
			ghc -rtsopts -O2 -o hs.bin 1330.hs;;
	esac
done
