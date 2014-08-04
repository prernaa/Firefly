#!/bin/sh

keep=0
delete=0
numFail=0
numPass=0
failure=0
globallog=testAll.log
TESTDIR="tests"
EXPDIR="tests/exp"

Usage()
{
	echo "-d: Delete intermediate files (.cpp, .diff) on failure."
	echo "-h: Display this help message."
	echo "-k: Keep intermediate files (.cpp, .diff) on success."
}

Check() 
{
	failure=0 # reset failed test status
	basename=`basename $1`
	basename="${basename%.*}"
	basedir=`dirname $1`
		
	./runCompiler.sh $1
	
	Compare $basedir/$basename.cpp $EXPDIR/$basename.out $basedir/$basename.diff
	
	if [ $failure -eq 1 ]
	then
		numFail=`expr $numFail + 1`
		echo "FAILED $basename.cpp differs from $basename.out" 1>&2
		if [ $delete -eq 1 ]
		then
			rm -f $basedir/$basename.cpp $basedir/$basename.diff
		fi
	else
		numPass=`expr $numPass + 1`
		if [ $keep -eq 0 ]
		then
			rm -f $basedir/$basename.cpp $basedir/$basename.diff
		fi
	fi
}

Compare()
{
	# Compare input file to golden copy, and output any differences to a .diff file.
	diff -b "$1" "$2" > "$3" 2>&1 || {
    failure=1
    }
}

Cleanup()
{
	# Cleans up any pre-existing .cpp and .diff files in the test directory.
	`find $TESTDIR -name "*.cpp" | xargs rm -f`
	`find $TESTDIR -name "*.diff" | xargs rm -f`
}

Cleanup

while getopts kdh c
do
    case $c in
    k) # Keep intermediate files
        keep=1
        ;;
	d) # delete intermediate files on fail
        delete=1 
        ;;
    h) # Help
        Usage
        ;;
	esac
done

shift `expr $OPTIND - 1`

if [ $# -ge 1 ]
then
    files=$@
else
	files=`find $TESTDIR -name "*.ff"`
fi

for file in $files
do
	Check $file 
done

echo "Passed: $numPass"
echo "Failed: $numFail"