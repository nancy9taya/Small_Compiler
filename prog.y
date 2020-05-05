%{
	#include<stdio.h>
	#include<stdlib.h>
	#include<string.h>
	int yylex();
	char* addquad(char,char* , char* ,char*);
    void addquad2(char ,char* , char , char);
	int typecheck(int,int);
	void yyerror (char const *s);
	int typeassign(int,int);
	char* declarationcheck(char a[20],int);
	char* variablecheck(char a[20]);
	void modifyvalue(char* value,char* name);
	char* getvalue(char* name);
	char* getnumber(char* num);
	int gettype(char* name);
	void my_strcpy(char *destination, char *source);
	void printsymtable();
	char* calc(int type,char* op1,char* op2,char operation,int calcflag);
	int symindex=0;
	struct table
	{
		char name[20];
		int type;
		char value[20];
	}arr[100];
	char* temp;
	char* num;
	char temp2[20];
	char quad ='T';
	int quadcount=1;
	char ob[10]="T";
	int typeno;
	FILE* yyout;
	char* calcresult;
%}
%union {
  struct info{ 
		char* name;
        char* value;
		int quadvalue;
		char * quad;
	    char  variable;
		char * string;    
        int  type;
    }ourinfo;
}  

%token  <ourinfo>IDENTIFIER 
%token  <ourinfo> NUM
%token  <ourinfo> REAL 
%token  <ourinfo> CHARVALUE 
%token  <ourinfo> EXPSTR 
%token  CHAR INT FLOAT DOUBLE STRING VOID RETURN 
%token  EQ LE GE AND OR XOR ASSIGN L G NEQ
%token  ADD SUB MUL DIV INC DEC
%token SEMICOLON COMMA
%token  OP CP OB CB

%start program

%%
program 
	: programnext {printsymtable();fprintf(yyout,"input accepted\n");exit(0);}
	;
programnext
	: function
	;

declarations 
	: type assignmentlist 
	;
assignmentlist
	:variable ASSIGN string  {if($<ourinfo>1.type == $<ourinfo>3.type){modifyvalue($<ourinfo>3.value,$<ourinfo>1.name);} else{fprintf(yyout,"ERROR: Type Mismatch ,Implicit conversion of Int type to Char or string\n");}} 
	|variable ASSIGN number {printf("type1:%d,type2:%d\n",$<ourinfo>1.type,$<ourinfo>3.type);if($<ourinfo>1.type == $<ourinfo>3.type){modifyvalue($<ourinfo>3.value,$<ourinfo>1.name);}else{fprintf(yyout,"ERROR: Type Mismatch ,Implicit conversion of Int type to Char or string\n");}} 
	| variable COMMA assignmentlist 
	| variable 
	;
number
	: NUM	{$<ourinfo>$.type = 2 ;  strcpy($<ourinfo>$.value,$<ourinfo>1.value); }
	| REAL	{$<ourinfo>$.type = 3 ;  strcpy($<ourinfo>$.value,$<ourinfo>1.value); }
	;
	
string
	:EXPSTR  {$<ourinfo>$.type = 4 ;  strcpy($<ourinfo>$.value,$<ourinfo>1.value); }
	| CHARVALUE {$<ourinfo>$.type = 1 ;  strcpy($<ourinfo>$.value,$<ourinfo>1.value); }
	;
type 
	: INT   {  $<ourinfo>1.type = 2 ; $<ourinfo>$=$<ourinfo>1; typeno=2}
	| CHAR  {  $<ourinfo>1.type = 1 ; $<ourinfo>$=$<ourinfo>1; typeno=1}
	| FLOAT {  $<ourinfo>1.type = 3 ; $<ourinfo>$=$<ourinfo>1; typeno=3}
	| VOID  {  $<ourinfo>1.type = 0 ; $<ourinfo>$=$<ourinfo>1; typeno=0}
	|STRING {  $<ourinfo>1.type = 4 ; $<ourinfo>$=$<ourinfo>1; typeno=4} 
	;
function
	: type IDENTIFIER OP CP OB statements CB  {printf("function 2boos 2dek tmamm");} 
	;

variable
	: IDENTIFIER {  $<ourinfo>$.name=declarationcheck($<ourinfo>$.name,typeno); $<ourinfo>$.type=typeno;  printf("%d\n",$<ourinfo>$.type); }
	;
argument 
	: type variable
	;
argumentlist
	: argument
	| argument COMMA argumentlist
	| {;}
	;
id
	: IDENTIFIER {$<ourinfo>$.name=variablecheck($<ourinfo>1.name); printf("var: %s\n",$<ourinfo>$.name);}
	;
statements
	: id ASSIGN expressions SEMICOLON  {printf("id name: %s, expression type:%d,id type:%d\n",$<ourinfo>1.name,$<ourinfo>3.type,gettype($<ourinfo>1.name));int check=typecheck(gettype($<ourinfo>1.name),$<ourinfo>3.type);
										if(check==1 && strcmp($<ourinfo>3.value,"-")!=0)
										{ modifyvalue($<ourinfo>3.value,$<ourinfo>1.name); 
										printf("expression value: %s \n",$<ourinfo>3.value);
										  if($<ourinfo>3.type ==2 || $<ourinfo>3.type ==3) addquad2('=',ob,'-',(char)$<ourinfo>1.variable);
										  printf("expression value: %s \n",$<ourinfo>3.value);
										}
										printf("expression value: %s \n",$<ourinfo>3.value);
									  }
    | type variable ASSIGN expressions SEMICOLON {int check=typecheck($<ourinfo>1.type,$<ourinfo>4.type);
													if(check==1 && strcmp($<ourinfo>4.value,"-")!=0){ 
														modifyvalue($<ourinfo>4.value,$<ourinfo>2.name);
														quad =(char)$<ourinfo>2.variable;
														if($<ourinfo>1.type ==2 || $<ourinfo>1.type ==3) addquad2('=',ob,'-',quad);
													}
												 }
	| declarations SEMICOLON
	| id ASSIGN number SEMICOLON{int check=typecheck(gettype($<ourinfo>1.name),$<ourinfo>3.type);
										if(check==1){ 
											modifyvalue($<ourinfo>3.value,$<ourinfo>1.name);
										}
									}
	| statements  statements 
	;

operation
	: ADD {$<ourinfo>$.value='+';}
	| SUB {$<ourinfo>$.value='-';}
	| MUL {$<ourinfo>$.value='*';}
	| DIV {$<ourinfo>$.value='/';}
	;

expressions
	: id operation id {	if(strcmp($<ourinfo>3.name,"-")!=0 && strcmp($<ourinfo>1.name,"-")!=0){
							$<ourinfo>$.type=typeassign(gettype($<ourinfo>3.name),gettype($<ourinfo>1.name));  
							strcpy($<ourinfo>$.value,calc($<ourinfo>$.type,$<ourinfo>1.name,$<ourinfo>3.name,(char)$<ourinfo>2.value,0));
						}
						else
							strcpy($<ourinfo>$.value,"-");
					  }
	| id operation number	{ if(strcmp($<ourinfo>1.name,"-")!=0){
									$<ourinfo>$.type=typeassign(gettype($<ourinfo>1.name),$<ourinfo>3.type); 
									strcpy($<ourinfo>$.value,calc($<ourinfo>$.type,$<ourinfo>1.name,$<ourinfo>3.value,(char)$<ourinfo>2.value,1));
							  }
							  else
									strcpy($<ourinfo>$.value,"-");
							}
								
	| number operation id { if(strcmp($<ourinfo>3.name,"-")!=0){
									$<ourinfo>$.type=typeassign($<ourinfo>1.type,$<ourinfo>3.type); 
									strcpy($<ourinfo>$.value,calc($<ourinfo>$.type,$<ourinfo>1.value,$<ourinfo>3.name,(char)$<ourinfo>2.value,2));
									printf("outcalc:%s",$<ourinfo>$.value);}
							else
									strcpy($<ourinfo>$.value,"-");
						  } 
	| number operation number {$<ourinfo>$.type=typeassign($<ourinfo>1.type,$<ourinfo>3.type);
								strcpy($<ourinfo>$.value,calc($<ourinfo>$.type,$<ourinfo>1.value,$<ourinfo>3.value,(char)$<ourinfo>2.value,3)); }
	;

%%
void printsymtable()
{
	fprintf(yyout,"Name	Type	Value\n");
	char* type;
	for(int j=0;j<symindex;j++)
	{
		switch(arr[j].type)
		{
			case 2:
				strcpy(type,"INT");
				break;
			case 3:
				strcpy(type,"FLOAT");
				break;
			case 1:
				strcpy(type,"CHAR");
				break;
			case 4:
				strcpy(type,"STRING");
				break;
		}
		fprintf(yyout,"%s	%s	%s\n",arr[j].name,type,arr[j].value);
	}
}

char* calc(int type,char op1[20],char op2[20],char operation,int calcflag)
{
	printf("calc,type:%d\n",type);
	switch(calcflag)
	{
				case 0:
						if(strcmp(getvalue(op2),"-")==0 && strcmp(getvalue(op1),"-")==0)
						{
							fprintf(yyout,"ERROR: Varibles %s and %s used without being inialized \n",op1,op2);
							return "-";
						}
						else if(strcmp(getvalue(op1),"-")==0)
						{
							fprintf(yyout,"ERROR: Varible %s used without being inialized \n",op1);
							return "-";
						}
						else if(strcmp(getvalue(op2),"-")==0)
						{
							fprintf(yyout,"ERROR: Varible %s used without being inialized \n",op2);
							return "-";
						}
						break;
				case 1:
						printf("switch ,op1: %s",op1);
						if(strcmp(getvalue(op1),"-")==0)
						{
							printf("in condition ");
							fprintf(yyout,"ERROR: Varible %s used without being inialized \n",op1);
							return "-";
						}
						break;
				case 2:
						if( strcmp(getvalue(op2),"-")==0)
						{
							fprintf(yyout,"ERROR: Varible %s used without being inialized \n",op2);
							return "-";
						}
						break;
				default:
				printf("default\n");
						break;
	}
		if(type==2)
		{
			printf("int");
			int num1;
			int num2;
			switch(calcflag)
			{
				case 0:
						num1=atoi(getvalue(op1));
						num2=atoi(getvalue(op2));
						break;
				case 1:
						num1=atoi(getvalue(op1));
						num2=atoi(op2);
						break;
				case 2:
						num1=atoi(op1);
						num2=atoi(getvalue(op2));
						break;
				case 3:
						num1=atoi(op1);
						num2=atoi(op2);
						break;
			}
			int value;
			switch(operation)
			{
				case '+':
					value=num1+num2;
					break;
				case '-':
					value=num1-num2;
					break;
				case '*':
					value=num1*num2;
					break;
				case '/':
					value=num1/num2;
					break;
			}
			char buffer[20];
			snprintf(buffer, sizeof(buffer), "%d", value);
			char* result=buffer;
			addquad(operation,op1,op2,ob);
			printf("total:%s",result);
			
			return result;
		}
		printf("\nafter int\n");
		printf("calc,type:%d\n",type);
		if(type==3)
		{
			printf("calc,type:%d\n",type);
			float fnum1,fnum2;
			printf("%d",calcflag);
			switch(calcflag)
			{
				case 0:
					fnum1=atof(getvalue(op1));
					printf("fnum1: %f \n: %f\n",fnum1);
					fnum2=atof(getvalue(op2));
					printf("fnum1: %f and fnum2: %f\n",fnum1,fnum2);
					break;
				case 1:
					fnum1=atof(getvalue(op1));
					printf("fnum1: %f \n: %f\n",fnum1);
					fnum2=atof(op2);
					printf("fnum1: %f and fnum2: %f\n",fnum1,fnum2);
					break;
				case 2:
					fnum1=atof(op1);
					printf("fnum1: %f \n: %f\n",fnum1);
					fnum2=atof(getvalue(op2));
					printf("%f\n",fnum2);
					break;
				case 3:
					fnum1=atof(op1);
					fnum2=atof(op2);
					printf("fnum1: %f and fnum2: %f\n",fnum1,fnum2);
					break;
			}
			printf("fnum1: %f and fnum2: %f\n",fnum1,fnum2);
			float fvalue;
			switch(operation)
			{
				case '+':
					printf("+\n");
					fvalue=fnum1+fnum2;
					break;
				case '-':
					fvalue=fnum1-fnum2;
					break;
				case '*':
					fvalue=fnum1*fnum2;
					break;
				case '/':
					fvalue=fnum1/fnum2;
					break;
			}
			char buffer[20];
			snprintf(buffer, sizeof(buffer), "%f", fvalue);
			char* result=buffer;
			printf("result:%s\n",result);
			addquad(operation,op1,op2,ob);
			return result;
		}
}

char* getvalue(char* name)
{
	int j=0;
	char* temp;
	for(j=0;j<symindex;j++)
	{
		if(strcmp(arr[j].name,name)==0)
		{
			strcpy(temp,arr[j].value);
			printf("name: %s,value: %s\n",name,arr[j].value);
			return arr[j].value;
		}
	}
}

int gettype(char* name)
{
	printf("gettype name:%s\n",name);
	int j=0;
	for(j=0;j<symindex;j++)
	{
		if(strcmp(arr[j].name,name)==0)
		{
			printf("gettype type:%d\n",arr[j].type);
			return arr[j].type;
		}
	}
}

int typecheck(int a,int b)
{
	printf("a: %d,b:%d\n",a,b);
	if(a==0 || b==0)
	{
		fprintf(yyout,"ERROR: Void Type Error\n");
		return 0;
	}
	if(a!=b)
	{
		fprintf(yyout,"ERROR: Type Mismatch\n");
		if(a==1 && b==2)
			fprintf(yyout,"Note: Implicit conversion of Int type to Char\n");
		else if(a==2 && b==3)
			fprintf(yyout,"Note: Implicit conversion of Float type expression to Int\n");
		else if(a==3)
			fprintf(yyout,"Note: Implicit conversion to Float Type\n");
		return 0;
	}
	printf("\nbefore return 1 in typecheck\n");
	return 1;
}
int typeassign(int a,int b)
{
	if(a==0 || b==0)
		return 0;
	else if(a==b)
		return a;
	else if((a==1 && b==2)||(b==1&&a==2))
		return 2;
	else if((a==2 && b==3)||(a==3 && b==2))
		return 3;
	else return 3;
}

char* declarationcheck(char a[20],int type)
{
	int j=0;
	for(j=0;j<symindex;j++)
	{
		if(strcmp(arr[j].name,a)==0)
		{
			fprintf(yyout,"ERROR: Multiple declarations of variable %s\n",a);
			return arr[j].name;
		}
	}
	if(j==symindex)
	{
		//arr[j].name=a;
		strcpy(arr[j].name,a);
		strcpy(arr[j].value,"-");
		arr[j].type=type;
		symindex++;
		return arr[j].name;
	}
}
char* variablecheck(char a[20])
{
	
	int j=0,flag=0;char*t;
	for(j=0;j<symindex;j++)
	{
		if(strcmp(arr[j].name,a)==0)
		{
			flag=1;
			t=arr[j].name;
			break;
		}
	}
	if(flag==0)
	{
		fprintf(yyout,"ERROR: Undeclared Variable %s\n",a);
		t="-";
	}
	return t;
}

void modifyvalue(char* value,char* name)
{
	char*temp="a";
	int j=0;
	for(j=0;j<symindex;j++)
	{
		if(strcmp(arr[j].name,name)==0)
		{
			temp=strtok(value,";");
			strcpy(arr[j].value,temp);
			printf("value:%s\n",arr[j].value);
			return;
		}
	}
}

char * addquad(char a,char* b, char* c,char* result)
{
	char* op;
	switch(a)
		{
			case '+':
				op="+";
				break;
			case '-':
				op="-";
				break;
			case '*':
				op="*";
				break;
			case '/':
				op="/";
				break;
		}
    fprintf(yyout,"\t result %s \t\t\t  operator %c \t\t\t   operand1 %s \t\t\t  operand2 %s \n", result, a, strtok(b,op), c);
    return ob;
}

void addquad2(char a,char * b, char c,char result)
{
   
 
    fprintf(yyout,"\t result %c \t\t\t  operator %c \t\t\t   operand1 %s \t\t\t  operand2 %c \n", result, a, strtok(b,";"), c);

   
}
#include "lex.yy.c"

extern FILE *yyin;

void yyerror (char const *s) {
	fprintf (stderr, "%s\n", s);
}



int main()
{
	yyin=fopen("input.c","r");
	
	yyout=fopen("out.txt","w");

	yyparse();
	
	fclose(yyout);
        fclose(yyin);
	return 0;
}




