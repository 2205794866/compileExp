        int num_lines=0,num_chars=0;
%%
\n ++num_lines;
.  ++num_chars;
%%
int main(int argc, char *argv[])
{
    if(argc > 1)
    {
        yyin = fopen(argv[1], "r");
    }
    yylex();
    printf("Lines=%d,Chars=%d\n",num_lines,num_chars);
}