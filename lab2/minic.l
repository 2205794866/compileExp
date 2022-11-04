%{
#include "string.h"
#include "def.h"

extern int yycolumn;

#define YYSTYPE yylval
extern "C"
{	
	int yywrap(void);
	int yylex(void);
}

void flexout(std::string name,std::string value){
  std::cout<<name<<"\t:"<<value<<std::endl;
}

void flexerr(std::string errmsg, std::string value){
  std::cout<<"Error type A at Line "<< yylineno << ": "<< errmsg << " \'" << value << '\'' << std::endl;
}

//flex为bison提供行号信息
%}
%option yylineno 
DIGIT ((\-)?([1-9][0-9]*))|0
ID [a-zA-Z_][a-zA-Z_0-9]*
FLOAT (\-)?{DIGIT}+\.{DIGIT}*(e{DIGIT})?

OCTAL 0[0-7]+
ILOCTAL 0[0-7]*[89\w\W]+[0-7]*

HEX 0(x|X)[A-Fa-f0-9]+
ILHEX 0(x|X)[A-Fa-f0-9]*[g-zG-Z]+[A-Fa-f0-9]*

%%

"int"         {flexout("TYPE","int");}
"float"       {flexout("TYPE","float");}
"char"        {flexout("TYPE","char");}
";"           {flexout("SEMI", ";");}
","           {flexout("COMMA", ",");}
"=="          {flexout("RELOP", "==");}
"+="          {flexout("PLUSASS", "+=");}
"-="          {flexout("MINUSASS", "-=");}
"="           {flexout("ASSIGNOP", "=");}
"++"          {flexout("PLUSPLUS", "++");}
"+"           {flexout("PLUS", "+");}
"--"          {flexout("MINUSMINUS", "--");}
"-"           {flexout("MINUS", "-");}
"*"           {flexout("STAR", "*");}
"/"           {flexout("DIV", "/");}
"&"           {flexout("AND", "&");}
"|"           {flexout("OR", "|");}
"."           {flexout("DOT", ".");}
"!"           {flexout("NOT", "!");}
"("           {flexout("LP", "(");}
")"           {flexout("RP", ")");}
"["           {flexout("LB", "[");}
"]"           {flexout("RB", "]");}
"{"           {flexout("LC", "{");}
"}"           {flexout("RC", "}");}
"struct"      {flexout("STRUCT", "struct");}
"return"      {flexout("RETURN", "return");}
"if"          {flexout("IF", "if");}
"else"        {flexout("ELSE", "else");}
"while"       {flexout("WHILE", "while");}


{DIGIT}       {flexout("INT", yytext);}
{OCTAL}       {flexout("INT", yytext);}
{HEX}         {flexout("INT", yytext);}
{FLOAT}       {flexout("FLOAT", yytext);}

{ILOCTAL}     {flexerr("Illegal octal number", yytext);}
{ILHEX}       {flexerr("Illegal hexadecimal number", yytext);}

{ID}          {flexout("ID", yytext);}

[\n]		{yycolumn=1;}
[ \r\t]		{/*printf("过滤空格等字符\n");*/}

.         {flexerr("Mysterious characters", yytext);}
%%

int yywrap()
{
    return 1;
}