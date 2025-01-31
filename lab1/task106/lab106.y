%{
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
int yylex(void);
void yyerror(char const *);
%}


%define api.value.type {double}
/* Tokens */
%token NUM
%token EOL
%token ADD SUB MUL DIV


 /* begin */
%token LP RP

 /* end */
%% 
  /* Grammar rules and actions follow.  */
 /* begin */
calclist:
	%empty
	|calclist exp EOL {printf("=%.10g\n",$2);}

exp     :term           { $$ = $1; }
        |exp ADD term   { $$ = $1 + $3;}
        |exp SUB term   { $$ = $1 - $3;}
	    ;

term    :factor         { $$ = $1;}
        |term MUL factor { $$ = $1 * $3;}
        |term DIV factor { $$ = $1 / $3;}
	    ;

factor  : NUM           {$$ = $1;}
        |LP exp RP      {$$ = $2;}
        |SUB factor     {$$ = -$2;}
        |ADD factor     {$$ = $2;}
        ;

 /* end */
%%

/* The lexical analyzer returns a double floating point
   number on the stack and the token NUM, or the numeric code
   of the character read if not a number.  It skips all blanks
   and tabs, and returns 0 for end-of-input.  */

/* begin */
int yylex(void)
{
	int c;
	while((c=getchar())==' '||c=='\t')
	continue;
	if(c=='.'||isdigit(c))
	{
	ungetc(c,stdin);
	if(scanf("%lf",&yylval)!=1)
		abort();
	return NUM;
	}
	switch(c){
	case EOF: return YYEOF;
	case '\n':return EOL;
	case '+': return ADD;
    case '-': return SUB;
	case '*': return MUL;
	case '/': return DIV;
    case '(': return LP;
    case ')': return RP;
	default:
		return c;
		
	}
	
}
/* end */

int main (int argc, char** argv)
{
   yyparse();
   return 0;
}


/* Called by yyparse on error.  */
void yyerror (char const *s)
{
  fprintf (stderr, "%s\n", s);
}