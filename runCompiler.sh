#!/bin/sh

compile=0

Usage()
{
	echo "Usage: runCompiler.sh [-c] file.ff"
	echo ""
	echo "-c: Run C++ compiler on output C++ file, and then run the executable."
	echo "-h: Help"
	echo ""
	echo "This will run the Firefly3D compiler on file.ff, and generate an output cpp file."
}



while getopts ch o
do
	case $o in
	c) # Set compile flag to true (this will let us run C++ compiler and execute)
		compile=1
		;;
	h) # Show Usage information
		Usage
		;;
	esac
done

shift `expr $OPTIND - 1`

if [ $# -eq 1 ]
then
	./firefly3D.byte < $1
	filename=${1%.ff}
	mv output.cpp $filename.cpp
	
	if [ $compile = 1 ]
	then
		./cppCompile.sh $filename.cpp
	else
		:
	fi
else
	:
fi