%locations
//bison的.y文件中，加入％locations，这样bison加入－d编译后的.h符号表文件中会多出一个YYLTYPE结构体定义和一个该结构体类型的全局变量yylloc，这样，flex的.l文件include该.h文件后，该结构体类型就可以被flex知道，且flex可以向yylloc里面填入信息。

%{
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "def.h"

#include <bits/stdc++.h>
extern int yylineno;
extern char *yytext;
extern FILE *yyin;
char myerror[255];
// extern NProgram *p;
extern int allerror;
void myyyerror();

extern "C"{
void yyerror(const char* fmt, ...);
extern int yylex(void);
}
%}

%union {
    int *p;
    std::string *text;
    int    type_int;
    float  type_float;
    int    type_char;
    char   type_id[32];
}
//union的默认结构体类型为YYSTYPE，相当于自己把YYSTYPE重新定义为union类型。所以相应的词法分析中yylval也变为union类型。
//这个union类型-d选项编译时会放在头文件中
//% type  用于定义非终结符的语义值类型
%type  <p> program ExtDefList ExtDef Specifier ExtDecList DecList VarDec FunDec CompSt DefList VarList ParamDec Dec Def StmtList Exp Stmt Args OptTag  Tag StructSpecifier
//% token 用于定义终结符的语义值类型
%token <type_int> INT                   //指定INT的语义值是type_int，由词法分析得到的数值
%token <type_id> ID RELOP TYPE STRUCT   //指定ID,RELOP 的语义值是type_id，由词法分析得到的标识符字符串mmecpy得到的
%token <type_float> FLOAT               //指定ID的语义值是type_float，由词法分析得到的float
%token <type_char> CHAR
%token LP RP LC RC LB RB SEMI COMMA     //用bison对该文件编译时，带参数-d，生成的exp.tab.h中给这些单词进行编码，可在lex.l中包含parser.tab.h使用这些单词种类码
%token DOT PLUS MINUS STAR DIV MOD ASSIGNOP AND OR NOT IF BREAK ELSE WHILE RETURN PLUSASS MINUSASS STARASS DIVASS PLUSPLUS MINUSMINUS
//由低到高的定义优先级

%%
/*High-level Definitions*/ /*Begin*/
program: ExtDefList {std::cout<<"Program"<<std::endl;} /*显示规则对应非终结符*/
        ;

ExtDefList: ExtDef ExtDefList {std::cout<<"ExtDefList"<<std::endl;} /* 全局变量定义序列 */
        | %empty {std::cout<<"ExtDefList"<<std::endl;}
        ;

ExtDef: Specifier ExtDecList SEMI {std::cout<<"ExtDef"<<std::endl;} /* 全局变量定义 */
        | Specifier SEMI {std::cout<<"ExtDef"<<std::endl;} /* 全局结构体定义 */ 
        | Specifier FunDec CompSt {std::cout<<"ExtDef"<<std::endl;} /* 函数式定义 */
        ;

ExtDecList: VarDec {std::cout<<"ExtDef"<<std::endl;} /* 单个变量定义 */
        | VarDec COMMA ExtDecList {std::cout<<"ExtDef"<<std::endl;} /* 多个变量定义 */ 
        ;

Specifier: TYPE {std::cout<<"Specifier"<<std::endl;} /* 基本类型 */
        | StructSpecifier {std::cout<<"Specifier"<<std::endl;} /* 结构类型 */
        ;
 
StructSpecifier: STRUCT OptTag LC DefList RC {std::cout<<"Specifier"<<std::endl;} /* 基本结构 */
        | STRUCT Tag {std::cout<<"Specifier"<<std::endl;} /* 简单结构 */
        ;

OptTag : ID {std::cout<<"OptTag"<<std::endl;} /* 可选名称 */
        | %empty {std::cout<<"OptTag"<<std::endl;}
        ;

Tag: ID {std::cout<<"Tag"<<std::endl;} /* 名称 */
        ;

VarDec : ID {std::cout<<"VarDec"<<std::endl;} /* 单标识符变量定义 */
        | ID LP INT RB {std::cout<<"VarDec"<<std::endl;} /* 数组变量定义 */
        ;

FunDec : ID LP VarList RP {std::cout<<"FunDec"<<std::endl;} /* 函数头定义 */
        | ID LP RP {std::cout<<"FunDec"<<std::endl;} /* 无参数函数头 */
        ;

VarList: ParamDec COMMA VarList {std::cout<<"VarList"<<std::endl;} /* 参数列表 */
        | ParamDec {std::cout<<"VarList"<<std::endl;} /* 单参数 */
        ;

ParamDec: Specifier VarDec {std::cout<<"ParamDec"<<std::endl;} /* 形参定义 */
        ;

CompSt : LC DefList StmtList RC {std::cout<<"CompSt"<<std::endl;} /* 一对花括号括起来的语句块 */
        ;

StmtList: Stmt StmtList {std::cout<<"StmtList"<<std::endl;} /* 一系列语句 */
        | %empty {std::cout<<"StmtList"<<std::endl;}
        ;

Stmt : Exp SEMI {std::cout<<"Stmt"<<std::endl;} /* 单独语句 */
        | CompSt {std::cout<<"Stmt"<<std::endl;} /* 语句块 */
        | RETURN Exp SEMI {std::cout<<"Stmt"<<std::endl;} /* 返回语句 */
        | IF LP Exp RP Stmt {std::cout<<"Stmt"<<std::endl;} /* if 语句 */
        | IF LP Exp RP Stmt ELSE Stmt {std::cout<<"Stmt"<<std::endl;} /* if else 语句 */
        | WHILE LP Exp RP Stmt {std::cout<<"Stmt"<<std::endl;} /* while 语句 */
        ;

DefList : Def DefList {std::cout<<"DefList"<<std::endl;} /* 局部变量定义 */
        | %empty {std::cout<<"DefList"<<std::endl;}
        ;

Def : Specifier DecList SEMI {std::cout<<"Def"<<std::endl;} /* 一条定义 */
        ;

DecList: Dec {std::cout<<"DecList"<<std::endl;} /* 单个变量名 */
        | Dec COMMA DecList {std::cout<<"DecList"<<std::endl;} /* 变量序列 */
        ;

Dec : VarDec {std::cout<<"Dec"<<std::endl;} /* 变量名 */
        | VarDec ASSIGNOP Exp {std::cout<<"Dec"<<std::endl;} /* 初始化赋值 */
        ;

Exp: Exp ASSIGNOP Exp {std::cout<<"Exp"<<std::endl;} /* 赋值 */
        | Exp AND Exp {std::cout<<"Exp"<<std::endl;} /* AND */
        | Exp OR Exp {std::cout<<"Exp"<<std::endl;} /* OR */
        | Exp RELOP Exp {std::cout<<"Exp"<<std::endl;} /* 关系表达式 */
        | Exp PLUS Exp {std::cout<<"Exp"<<std::endl;} /* */
        | Exp MINUS Exp {std::cout<<"Exp"<<std::endl;} /* */
        | Exp STAR Exp {std::cout<<"Exp"<<std::endl;} /* */
        | Exp DIV Exp {std::cout<<"Exp"<<std::endl;} /* */
        | LP Exp RP  {std::cout<<"Exp"<<std::endl;} /* */
        | MINUS Exp {std::cout<<"Exp"<<std::endl;} /* */
        | NOT Exp {std::cout<<"Exp"<<std::endl;} /* */
        | ID LP Args RP {std::cout<<"Exp"<<std::endl;} /* */
        | ID LP RP {std::cout<<"Exp"<<std::endl;} /* */
        | Exp LB Exp RB {std::cout<<"Exp"<<std::endl;} /* */
        | Exp DOT ID {std::cout<<"Exp"<<std::endl;} /* */
        | ID {std::cout<<"ID"<<std::endl;} /* */
        | INT {std::cout<<"INT"<<std::endl;} /* */
        | FLOAT {std::cout<<"FLOAT"<<std::endl;} /* */
        ;

Args : Exp COMMA Args {std::cout<<"Args"<<std::endl;} /* 实参列表 */
        | Exp {std::cout<<"Args"<<std::endl;} /* */
        ;



%%
/*End*/
#include<stdarg.h>
void yyerror(const char* fmt, ...)
{
    va_list ap;//取参数对应的函数
    va_start(ap, fmt);
    fprintf(stderr, "Grammar Error at Line %d Column %d: ", yylloc.first_line,yylloc.first_column);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr,"%s",myerror);
    fprintf(stderr,"%s", ".\n");
    memset(myerror,0,sizeof(myerror));
}   
void myyyerror()
{
    fprintf(stderr, "Grammar Error at Line %d Column %d: ", yylloc.first_line,yylloc.first_column);
    fprintf(stderr,"%s",myerror);
    fprintf(stderr, "%s",".\n");
    memset(myerror,0,sizeof(myerror));
}
