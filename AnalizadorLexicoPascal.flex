/* Sección de declaraciones de JFlex */
%%
%public
%class AnalizadorLexicoPascal
%{
 
 /* Código personalizado */
 
 // Se agregó una propiedad para verificar si existen tokens pendientes
 private boolean _existenTokens = false;
 
 public boolean existenTokens(){
 return this._existenTokens;
 }
 
%}
 
 /* Al utilizar esta instrucción, se le indica a JFlex que devuelva objetos del tipo TokenPersonalizado */
%type TokenPersonalizado
 
%init{
 /* Código que se ejecutará en el constructor de la clase */
%init}
 
%eof{
 
 /* Código a ejecutar al finalizar el análisis, en este caso cambiaremos el valor de una variable bandera */
 this._existenTokens = false;
 
%eof}
 
/* Inicio de Expresiones regulares */
 
FinDeLinea = [\n|\r|\r\n]
//Comentarios =  (\(\*|\{)([^\*\}]|\*+[^\)\}])*(\*+\)|\})
EspacioEnBlanco = {FinDeLinea}|[\t|\f| ]
Ignorar = {EspacioEnBlanco}
Letra = [a-z|A-Z]
digito = [0-9]
espacio = " "

suma = "+"
resta = "-"
multiplicacion = "*"
division = "/"
DivEntera = [Dd][Ii][Vv]
Modulo = [Mm][Oo][Dd]
Shl = [Ss][Hh][Ll]
Shr = [Ss][Hh][Rr]

LT = "<"
LE = "<="
GT = ">"
GE = ">="
IGUAL = "="
DIFERENTE = "<>"

AND = [Aa][Nn][Dd]
OR = [Oo][Rr]
NOT = [Nn][Oo][Tt]

punto = "."
doblePunto = ".."
coma = ","
dosPuntos = ":"
puntoComa = ";"
elevado = "^"
asignacion = ":="
parentesisApertura = "("
parentesisCierre = ")"
corcheteApertura = "["
corcheteCierre = "]"
comillaSimple = \'
comillaDoble = \"
llaveApertura = "{"
llaveCierre = "}"
NQUOTE = [^']
barraSeparadora = "/"
Chr = "'"{NQUOTE}"'"
True = [Tt][Rr][Uu][Ee]
False = [Ff][Aa][Ll][Ss][Ee]
enteros = 0|[1-9][0-9]*
reales = {digito}+(\.{digito}+)?([E|e](\+|-)?{digito}+)?
cadenas = "'"{NQUOTE}+"'"
Comentarios = {llaveApertura}(({Letra}|{digito}|{espacio}|punto|diblePunto|coma|dosPuntos|puntoComa|elevado|asignacion|parentesisApertura|parentesisCierre|corcheteApertura|corcheteCierre|comillaSimple|comillaDoble|NQUOTE)*({FinDeLinea})*)*{llaveCierre}
Comentarios2 = {parentesisApertura}{multiplicacion}(({Letra}|{digito}|{espacio}|punto|diblePunto|coma|dosPuntos|puntoComa|elevado|asignacion|parentesisApertura|parentesisCierre|corcheteApertura|corcheteCierre|comillaSimple|comillaDoble|NQUOTE)*({FinDeLinea})*)*{multiplicacion}{parentesisCierre}
Comentarios3 = {barraSeparadora}{barraSeparadora}({Letra}|{digito}|{espacio}|punto|diblePunto|coma|dosPuntos|puntoComa|elevado|asignacion|parentesisApertura|parentesisCierre|corcheteApertura|corcheteCierre|comillaSimple|comillaDoble|NQUOTE)*({FinDeLinea})*

Boolean = [Bb][Oo][Oo][Ll][Ee][Aa][Nn]
Char = [Cc][Hh][Aa][Rr]
Integer = [Ii][Nn][Tt][Ee][Gg][Ee][Rr]
Real = [Rr][Ee][Aa][Ll]
String = [Ss][Tt][Rr][Ii][Nn][Gg]

/* Palabras reservadas */
Array = [Aa][Rr][Rr][Aa][Yy]
Asm = [Aa][Ss][Mm]
Begin = [Bb][Ee][Gg][Ii][Nn]
Case = [Cc][Aa][Ss][Ee]
Const = [Cc][Oo][Nn][Ss][Tt]
Constructor = [Cc][Oo][Nn][Ss][Tt][Rr][Uu][Cc][Tt][Oo][Rr]
Destructor = [Dd][Ee][Ss][Tt][Rr][Uu][Cc][Tt][Oo][Rr]
Do = [Dd][Oo]
Downto = [Dd][Oo][Ww][Nn][Tt][Oo]
Else = [Ee][Ll][Ss][Ee]
End = [Ee][Nn][Dd]
File = [Ff][Ii][Ll][Ee]
For = [Ff][Oo][Rr]
Foward = [Ff][Oo][Ww][Aa][Rr][Dd]
Function = [Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn]
Goto = [Gg][Oo][Tt][Oo]
If = [Ii][Ff]
Implementation = [Ii][Mm][Pp][Ll][Ee][Mm][Ee][Nn][Tt][Aa][Tt][Ii][Oo][Nn]
In = [Ii][Nn]
Inline = [Ii][Nn][Ll][Ii][Nn][Ee]
Interface = [Ii][Nn][Tt][Ee][Rr][Ff][Aa][Cc][Ee]
Label = [Ll][Aa][Bb][Ee][Ll]
Nil = [Nn][Ii][Ll]
Object = [Oo][Bb][Jj][Ee][Cc][Tt]
Of = [Oo][Ff]
Packed = [Pp][Aa][Cc][Kk][Ee][Dd]
Procedure = [Pp][Rr][Oo][Cc][Ee][Dd][Uu][Rr][Ee]
Program = [Pp][Rr][Oo][Gg][Rr][Aa][Mm]
Record = [Rr][Ee][Cc][Oo][Rr][Dd]
Repeat = [Rr][Ee][Pp][Ee][Aa][Tt]
Set = [Ss][Ee][Tt]
Then = [Tt][Hh][Ee][Nn]
To = [Tt][Oo]
Type = [Tt][Yy][Pp][Ee]
Unit = [Uu][Nn][Ii][Tt]
Until = [Uu][Nn][Tt][Ii][Ll]
Uses = [Uu][Ss][Ee][Ss]
Var = [Vv][Aa][Rr]
While = [Ww][Hh][Ii][Ll][Ee]
With = [Ww][Ii][Tt][Hh]
Xor = [Xx][Oo][Rr]

idVariables = {Letra}({Letra}|{digito})*

%%

/* Finaliza la sección de declaraciones de JFlex */
 
/* Inicia sección de reglas */
 
// Cada regla está formada por una {expresión} espacio {código}



{punto}			             { TokenPersonalizado t = new TokenPersonalizado(yytext(), "PUNTO");
 								this._existenTokens = true;
 								return t; }
    {doblePunto}					 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "DOBLE PUNTO");
 								this._existenTokens = true;
 								return t;  }
    {coma}		        	     { TokenPersonalizado t = new TokenPersonalizado(yytext(), "COMA");
 								this._existenTokens = true;
 								return t;  }
    {dosPuntos}			             { TokenPersonalizado t = new TokenPersonalizado(yytext(), "DOS PUNTOS");
 								this._existenTokens = true;
 								return t;  }
    {puntoComa}		             { TokenPersonalizado t = new TokenPersonalizado(yytext(), "PUNTO Y COMA");
 								this._existenTokens = true;
 								return t;  }
    {comillaSimple}                { TokenPersonalizado t = new TokenPersonalizado(yytext(), "COMILLA SIMPLE");
 								this._existenTokens = true;
 								return t;  }
    {comillaDoble}                { TokenPersonalizado t = new TokenPersonalizado(yytext(), "COMILLA DOBLE");
 								this._existenTokens = true;
 								return t;  }
    {elevado}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "ELEVADO");
 								this._existenTokens = true;
 								return t;  }
    {asignacion}                     { TokenPersonalizado t = new TokenPersonalizado(yytext(), "ASIGNACION");
 								this._existenTokens = true;
 								return t;  }
    {suma}                       { TokenPersonalizado t = new TokenPersonalizado(yytext(), "SUMAR");
 								this._existenTokens = true;
 								return t; }
    {resta}			             { TokenPersonalizado t = new TokenPersonalizado(yytext(), "RESTAR");
 								this._existenTokens = true;
 								return t;  }
    {multiplicacion}      		     { TokenPersonalizado t = new TokenPersonalizado(yytext(), "MULTIPLICAR");
 								this._existenTokens = true;
 								return t;  }
    {division}        		     { TokenPersonalizado t = new TokenPersonalizado(yytext(), "DIVIDIR");
 								this._existenTokens = true;
 								return t; }
    {parentesisApertura}	        		     { TokenPersonalizado t = new TokenPersonalizado(yytext(), "PARENTESIS APERTURA");
 								this._existenTokens = true;
 								return t;  }
    {parentesisCierre}	        		     { TokenPersonalizado t = new TokenPersonalizado(yytext(), "PARENTESIS CIERRE");
 								this._existenTokens = true;
 								return t;  }
    {corcheteApertura}		        	     { TokenPersonalizado t = new TokenPersonalizado(yytext(), "CORCHETE APERTURA");
 								this._existenTokens = true;
 								return t;  }
    {corcheteCierre}      			     { TokenPersonalizado t = new TokenPersonalizado(yytext(), "CORCHETE CIERRE");
 								this._existenTokens = true;
 								return t;  }
    {LT}        			     { TokenPersonalizado t = new TokenPersonalizado(yytext(), "MENOR QUE");
 								this._existenTokens = true;
 								return t;  }
    {LE}	        		     { TokenPersonalizado t = new TokenPersonalizado(yytext(), "MENOR O IGUAL QUE");
 								this._existenTokens = true;
 								return t;  }
    {GT}        			     { TokenPersonalizado t = new TokenPersonalizado(yytext(), "MAYOR QUE");
 								this._existenTokens = true;
 								return t;  }
    {GE}        			     { TokenPersonalizado t = new TokenPersonalizado(yytext(), "MAYOR O IGUAL QUE");
 								this._existenTokens = true;
 								return t;  }
    {IGUAL}		        	     { TokenPersonalizado t = new TokenPersonalizado(yytext(), "IGUAL QUE");
 								this._existenTokens = true;
 								return t;  }
    {DIFERENTE}			             { TokenPersonalizado t = new TokenPersonalizado(yytext(), "DISTINTO QUE");
 								this._existenTokens = true;
 								return t;  }

    {enteros}                     { TokenPersonalizado t = new TokenPersonalizado(yytext(), "NUM ENTERO");
 								this._existenTokens = true;
 								return t;  }
    {reales}					 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "NUM REAL");
 								this._existenTokens = true;
 								return t;  }
    {Real}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "REAL");
 								this._existenTokens = true;
 								return t;  }
    {Boolean}                    { TokenPersonalizado t = new TokenPersonalizado(yytext(), "BOOLEAN");
 								this._existenTokens = true;
 								return t;  }
    {Integer}                    { TokenPersonalizado t = new TokenPersonalizado(yytext(), "INTEGER");
 								this._existenTokens = true;
 								return t;  }
    {Chr}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "CHR");
 								this._existenTokens = true;
 								return t;  }
    {cadenas}              			 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "CADENA");
 								this._existenTokens = true;
 								return t;  }

    {AND}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "AND");
 								this._existenTokens = true;
 								return t;  }
    {Array}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "ARRAY");
 								this._existenTokens = true;
 								return t;  }
    {Asm}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "ASM");
 								this._existenTokens = true;
 								return t;  }
    {Begin}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "BEGIN");
 								this._existenTokens = true;
 								return t; }
    {Case}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "CASE");
 								this._existenTokens = true;
 								return t;  }
    {Char}                       { TokenPersonalizado t = new TokenPersonalizado(yytext(), "CHAR");
 								this._existenTokens = true;
 								return t;  }
    {Const}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "CONST");
 								this._existenTokens = true;
 								return t;  }
    {Constructor}				 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "CONSTRUCTOR");
 								this._existenTokens = true;
 								return t;  }
    {Destructor}				 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "DESTRUCTOR");
 								this._existenTokens = true;
 								return t;  }
    {DivEntera}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "DIV PARTE ENTERA");
 								this._existenTokens = true;
 								return t;  }
    {Do}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "DO");
 								this._existenTokens = true;
 								return t;  }
    {Downto}					 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "DOWNTO");
 								this._existenTokens = true;
 								return t;  }
    {Else}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "ELSE");
 								this._existenTokens = true;
 								return t;  }
    {End}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "END");
 								this._existenTokens = true;
 								return t;  }
    {False}                      { TokenPersonalizado t = new TokenPersonalizado(yytext(), "FALSE");
 								this._existenTokens = true;
 								return t;  }
    {File}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "FILE");
 								this._existenTokens = true;
 								return t;  }
    {For}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "FOR");
 								this._existenTokens = true;
 								return t;  }
    {Foward}					 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "FOWARD");
 								this._existenTokens = true;
 								return t;  }
    {Function}					 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "FUNCTION");
 								this._existenTokens = true;
 								return t;  }
    {Goto}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "GOTO");
 								this._existenTokens = true;
 								return t;  }
    {If}                         { TokenPersonalizado t = new TokenPersonalizado(yytext(), "IF");
 								this._existenTokens = true;
 								return t;  }
    {Implementation}			 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "IMPLEMENTATION");
 								this._existenTokens = true;
 								return t;  }
    {In}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "IN");
 								this._existenTokens = true;
 								return t;  }
    {Inline}					 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "INLINE");
 								this._existenTokens = true;
 								return t;  }
    {Interface}					 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "INTERFACE");
 								this._existenTokens = true;
 								return t;  }
    {Label}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "LABEL");
 								this._existenTokens = true;
 								return t;  }
    {Modulo}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "OPERACION MODULO");
 								this._existenTokens = true;
 								return t;  }
    {Nil}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "NULL");
 								this._existenTokens = true;
 								return t;  }
    {NOT}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "NOT");
 								this._existenTokens = true;
 								return t;  }
    {Object}					 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "OBJECT");
 								this._existenTokens = true;
 								return t;  }
    {Of}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "OF");
 								this._existenTokens = true;
 								return t; }
    {OR}                         { TokenPersonalizado t = new TokenPersonalizado(yytext(), "OR");
 								this._existenTokens = true;
 								return t;  }
    {Packed}					 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "PACKED");
 								this._existenTokens = true;
 								return t;  }
    {Procedure}					 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "PROCEDURE");
 								this._existenTokens = true;
 								return t;  }
    {Program}					 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "PROGRAM");
 								this._existenTokens = true;
 								return t;  }
    {Record}					 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "RECORD");
 								this._existenTokens = true;
 								return t;  }
    {Repeat}					 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "REPEAT");
 								this._existenTokens = true;
 								return t;  }
    {Set}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "SET");
 								this._existenTokens = true;
 								return t;  }
    {Shl}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "SHL");
 								this._existenTokens = true;
 								return t;  }
    {Shr}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "SHR");
 								this._existenTokens = true;
 								return t;  }
    {String}					 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "STRING");
 								this._existenTokens = true;
 								return t;  }
    {Then}                       { TokenPersonalizado t = new TokenPersonalizado(yytext(), "THEN");
 								this._existenTokens = true;
 								return t;  }
    {To}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "TO");
 								this._existenTokens = true;
 								return t;  }
    {True}                       { TokenPersonalizado t = new TokenPersonalizado(yytext(), "TRUE");
 								this._existenTokens = true;
 								return t;  }
    {Type}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "TYPE");
 								this._existenTokens = true;
 								return t;  }
    {Unit}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "UNIT");
 								this._existenTokens = true;
 								return t;  }
    {Until}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "UNTIL");
 								this._existenTokens = true;
 								return t;  }
    {Uses}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "USES");
 								this._existenTokens = true;
 								return t;  }
    {Var}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "VAR");
 								this._existenTokens = true;
 								return t;  }
    {While}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "WHILE");
 								this._existenTokens = true;
 								return t;  }
    {With}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "WITH");
 								this._existenTokens = true;
 								return t;  }
    {Xor}						 { TokenPersonalizado t = new TokenPersonalizado(yytext(), "XOR");
 								this._existenTokens = true;
 								return t;  }

    {idVariables}                         { TokenPersonalizado t = new TokenPersonalizado(yytext(), "ID DE VARIABLE");
 								this._existenTokens = true;
 								return t;  }

 	{Comentarios} {
 				TokenPersonalizado t = new TokenPersonalizado(yytext(), "COMENTARIOS");
 				this._existenTokens = true;
 				return t;
	}
	{Comentarios2} {
 				TokenPersonalizado t = new TokenPersonalizado(yytext(), "COMENTARIOS");
 				this._existenTokens = true;
 				return t;
	}
	{Comentarios3} {
 				TokenPersonalizado t = new TokenPersonalizado(yytext(), "COMENTARIOS");
 				this._existenTokens = true;
 				return t;
	}
	{Ignorar} {
 TokenPersonalizado t = new TokenPersonalizado(yytext(), "ESPACIO BLANCO");
 this._existenTokens = true;
 return t;
}

