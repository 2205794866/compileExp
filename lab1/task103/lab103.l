%%
a*b             {printf("1");}
ca              {printf("2");}
a*ca*            {printf("3");}
%%
int main( argc, argv )
int argc;
char **argv;
{
    ++argv, --argc; /* skip over program name */
    if ( argc > 0 )
            yyin = fopen( argv[0], "r" );
    else
            yyin = stdin;
            yylex();
}    