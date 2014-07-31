open Ast
open Printf

let file = "output.cpp"
let oc = open_out file

let initCppFile = function 
	_ ->	
			fprintf oc "%s\n\n" "#include <iostream>";
			fprintf oc "%s\n\n" "using namespace std;";
			fprintf oc "%s\n" "int main()";
			fprintf oc "%s\n" "{"
		
let closeCppFile = function
	_ ->	
			fprintf oc "\n";
			fprintf oc "}";
			close_out oc
				
					
let translate = function
	Print ->	initCppFile ();
				fprintf oc "\tcout << \"Hello!\" << endl;";
				closeCppFile ()

				