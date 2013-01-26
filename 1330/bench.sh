#!/bin/sh

for i in $@
do
	echo $i
	if [ "$i" = "scala" ]
	then
		exe="scala-2.10.0/bin/scala Problem1330"
	else
		exe="./$i.bin"
	fi
	output=output.$i.txt
	$exe < input.txt > $output
	md5 $output
done