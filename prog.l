%{
	#include<stdio.h>
	#include<stdlib.h>
	#include<malloc.h>
	#include<string.h>
extern char yyval;
%}

alpha [a-zA-Z]
digit [0-9]
exp (E|e){digit}+
str \"[^"\n]*["\n]
%%
"-" return SUB;
"+" return ADD;
"*" return MUL;
"/" return DIV;
">=" return GE;
"<=" return LE;
"==" return EQ;
"<" return L;
">" return G;
"!=" return NEQ;
"=" {return ASSIGN;}
"&&" return AND;
"||" return OR;
"++" return INC;
"--" return DEC;
"^" return XOR;
"char" return CHAR;
"int" { printf("INT\n");return INT;}
"float" return FLOAT;
"string" return STRING; 
"void" return VOID;
"return" return RETURN;
"double" return DOUBLE; 
'.' {yylval.ourinfo.value=(char*)yytext;yylval.ourinfo.type=1;return CHARVALUE;}
{alpha}({alpha}|{digit})* {yylval.ourinfo.name=(char*)yytext; yylval.ourinfo.variable=(char)yytext[0]; printf("ID\n"); return IDENTIFIER;}
{digit}+  {yylval.ourinfo.value=(char*)yytext;yylval.ourinfo.quadvalue=atoi(yytext);yylval.ourinfo.type=2;return NUM;}
[0-9]+\.[0-9]+ {yylval.ourinfo.value=(char*)yytext;yylval.ourinfo.quadvalue=atof(yytext);yylval.ourinfo.quad=yytext;yylval.ourinfo.type=3;return REAL;}
{str} {yylval.ourinfo.value=(char *)yytext;yylval.ourinfo.value++[strlen(yylval.ourinfo.value)] = 0; yylval.ourinfo.type=4;return EXPSTR ;}
";" {printf("Semicolon\n");return SEMICOLON;}
"," return COMMA;
"(" {printf("OP\n"); return OP;}
")" {printf("CP\n"); return CP;}
"{" {printf("OB\n"); return OB;}
"}" {printf("CB\n"); return CB;}
[ \t] ;
[\n] ;
. ;
%%
int yywrap()
{
	return 1;
}


