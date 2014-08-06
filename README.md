Firefly
=========

An Educational Vector Graphics Language (RT)	

=========
How to run on a Mac OS X

1. Open Terminal
2. Type "make clean"
3. Type "make"
4. Type "./runCompiler.sh -d <filename>.ff" 
5. Type "./runCompiler.sh -p <filename>.ff" 

Troubleshooting

1. "Permission Denied" when you run ./runCompiler.sh
=> Type: "sudo chmod 755 'runCompiler.sh' "
2. "ld: symbol(s) not found for architecture x86_64" OR "GL/glut.h" not found, etc
=> Type: "sudo ln /System/Library/Frameworks/OpenGL.framework/Versions/A/Headers/* /usr/local/include/GL"
		and "sudo ln /System/Library/Frameworks/GLUT.framework/Versions/A/Headers/* /usr/local/include/GL"