#!/bin/sh

compileWin=0
compileMac=0
delete=0
help=0
silent=0
filename=""
dirname=""


Usage()
{
	echo "Usage: runCompiler.sh [OPTIONS] file.ff"
	echo ""
	echo "-c: Run C++ compiler on generated .cpp file, and then run the executable. (Windows only)"
	echo "-d: Delete all C++ files (*.cpp, *.obj, *.exe, etc.) related to source .ff file at the end of the script."
	echo "-h: Display help menu."
	echo "-p: Run C++ compiler on generated .cpp file, and then run the executable. (Mac OS X only)"
	echo "-s: Use with '-c' or '-p' option. This will suppress output from C++ compilation and execution."
	echo ""
	echo "This will run the Firefly3D compiler on file.ff, and generate an output cpp file."
}

DeleteCpp()
{
	rm -f $filename.cpp
	rm -f $filename.obj
	rm -f $filename.exe
	rm -f $filename.o
	rm -f $filename.out
	rm -f $filename
}

while getopts cdhps o
do
	case $o in
	c) # Run Windows C++ compiler and execute the executable file.
		compileWin=1
		;;
	d) # Delete all C++ files related to source .ff file. Deletion happens at the end of the script.
		delete=1
		;;
	h) # Show Usage information.
		help=1
		;;
	p) # Run Mac OS X C++ compiler and execute the executable file.
		compileMac=1
		;;
	s) # If the "-c" or "-p" option is used, suppress all output from C++ compilation & execution.
		silent=1
		;;
	esac
done

shift `expr $OPTIND - 1`

if [ $# -eq 1 ] # User supplied one argument, just as expected.
then
	
	filename=${1%.ff}
	dirname=${1%/*}
	
	if [ "$dirname" == "$1" ]
	then
		dirname="./"
	else
		:
	fi
	
	# Before compiling, delete any pre-existing C++ files (*.cpp, *.obj, etc.) related to source .ff file.
	DeleteCpp
	
	# Compile the source .ff file.
	./firefly3D.byte < $1
	mv output.cpp "$filename".cpp
	
	# If user chose "-c" option, run C++ compiler on Windows and execute the resulting .exe file.
	if [ $compileWin = 1 ]
	then
		if [ $silent = 1 ] 
		then
			start //B //WAIT cppCompile.bat "$filename" >nul 2>nul
		else
			start //B //WAIT cppCompile.bat "$filename"
		fi
	else
		:
	fi
	
	# PRERNA : FEEL FREE TO INSERT YOUR OWN C++ COMPILE & RUN SCRIPT/CODE HERE.
	if [ $compileMac = 1 ]
	then
		if [ $silent = 1 ] # Place code here to suppress C++ compilation & execution output. Useful for batch testing.
		then
			g++ -w -o "$filename" "$filename".cpp -framework GLUT -framework OpenGL
            ./"$filename"
		else
            g++ -w -o "$filename" "$filename".cpp -framework GLUT -framework OpenGL
            ./"$filename"
		fi
	else
		:
	fi
	
	# If user selected "-d" option, delete all C++ files (*.cpp, *.obj, etc.) related to source .ff file.
	if [ $delete = 1 ]
	then
		DeleteCpp
	else
		:
	fi
	
elif [ $# -eq 0 ] # User didn't supply any arguments. This is only allowed if they're just doing -h.
then
	if [ $help = 1 -a `expr $compileWin + $compileMac + $delete` = 0 ]
	then	
		: 
	else 
		echo "Invalid number of arguments. There should be a single Firefly3D source file supplied."
		echo ""
	fi
	
else # User supplied more than one argument. This is never allowed.
	echo "Invalid number of arguments. There should be a single Firefly3D source file supplied."
	echo ""
fi

# Show help menu.
if [ $help = 1 ]
then
	Usage
else
	:
fi