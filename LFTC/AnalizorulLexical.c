
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define SAFEALLOC(var,Type) if((var=(Type*)malloc(sizeof(Type)))==NULL)err("not enough memory")

enum{ID, END, CT_INT,CT_REAL,CT_CHAR,CT_STRING,
	BREAK, CHAR, DOUBLE, ELSE, FOR, IF, INT, RETURN, STRUCT, VOID, WHILE,
	COMMA, SEMICOLON, LPAR, RPAR, LBRACKET, RBRACKET, LACC, RACC,
	ADD, SUB, MUL, DIV, DOT, AND, OR, NOT, ASSIGN, EQUAL, NOTEQ, LESS, LESSEQ, GREATER, GREATEREQ,
};

typedef struct _Token{
	int code; // codul (numele)
	union{
		char *text; // folosit pentru ID, CT_STRING (alocat dinamic)
		long int i; // folosit pentru CT_INT, CT_CHAR
		double r; // folosit pentru CT_REAL
	};
	int	line; // linia din fisierul de intrare
	struct _Token *next; // inlantuire la urmatorul AL
}Token;

void err(const char *fmt, ...)
{
	va_list va;
	va_start(va, fmt);
	fprintf(stderr, "error: ");
	vfprintf(stderr, fmt, va);
	fputc('\n', stderr);
	va_end(va);
	exit(-1);
}



void tkerr(const Token *tk, const char *fmt, ...)
{
	va_list va;
	va_start(va, fmt);
	fprintf(stderr, "error in line %d: ", tk->line);
	vfprintf(stderr, fmt, va);
	fputc('\n', stderr);
	va_end(va);
	exit(-1);
}


char input[50001];
Token *lastToken = NULL,*consumedTk,*crtTk;
Token *tokens = NULL;
int line = 0;
const char *pCrtCh;
int cnt=0,first=0;

Token *addTk(int code)
{

	Token *tk;
	SAFEALLOC(tk, Token);
		tk->code = code;
	tk->line = line;
	tk->next = NULL;
	if (lastToken){
		lastToken->next = tk;
	}
	else{
		tokens = tk;
	}
	lastToken = tk;
	return tk;
}

char* createString(const char *begin,const char *after){
	int n=after-begin;
	char * string =(char *)malloc(n+1);

	if(!string){
		printf("Eroare la alocarea memoriei.\n");
		exit(1);
	}
	memcpy(string,begin,n);
	string[n]='\0';
	return string;
}

char *unescapeChar(char *string) {//abfnrtv\'?\"\\0
	int len,i=0;
	
	len = strlen(string);
	for (i = 0; i < len; i++) {
		if (string[i] == '\\') {
			switch (string[i + 1]) {
				case 'a':
					string[i] = '\a';
					break;
				case 'b':
					string[i] = '\a';
					break;
				case 'f':
					string[i] = '\f';
					break;
				case 'n':
					string[i] = '\n';
					break;
				case 'r':
					string[i] = '\n';
					break;
				case 't':
					string[i] = '\t';
					break;
				case 'v':
					string[i] = '\v';
					break;
				case '\\':
					string[i] = '\\';
					break;
				case '\'':
					string[i] = '\'';
					break;
				case '?':
					string[i] = '\?';
					break;
				case '"':
					string[i] = '\"';
					break;
				case '0':
					string[i] = '\0';
					break;
				}
				for (int j = i + 1; j < len; j++) {
						string[j] = string[j + 1];
					}
			}
		}
		return string;
	}

int getNextToken()
{
	int state = 0, nCh;
	char ch;
	char *s;
	const char *pStartCh;
	Token *tk;

	while (1){ // bucla infinita
		ch = *pCrtCh;
		switch (state){
	
		case 0: // testare tranzitii posibile din starea 0
			
			if (isalpha(ch) || ch == '_'){
				pStartCh = pCrtCh; // memoreaza inceputul ID-ului
				pCrtCh++; // consuma caracterul
				state = 1; // trece la noua stare
			}
			else if (ch == '='){
				pCrtCh++;
				state = 3;
			}
			else if (ch == ' ' || ch == '\r' || ch == '\t'){
				pCrtCh++; // consuma caracterul si ramane in starea 0
			
			}
			else if (ch == '\n'){ // tratat separat pentru a actualiza linia curenta
				line++;
				pCrtCh++;
			}
			else if (ch =='/'){
				pCrtCh++;
				
				state=59;
			}
			else if (ch == 0){ // sfarsit de sir
				addTk(END);
				return END;
			}
			else if (ch == '+'){
				pCrtCh++;
				state=6;
			}
			else if (ch == '-'){
				pCrtCh++;
				state=7;
			}
			else if (ch == '*'){
				pCrtCh++;
				state=8;
			}
			else if (ch == '.'){
				pCrtCh++;
				state=9;
			}
			else if (ch == '&'){
				pCrtCh++;
				state=10;
			}
			else if (ch == '|'){
				pCrtCh++;
				state=12;
			}
			else if (ch == '!'){
				pCrtCh++;
				state=14;
			}
			else if (ch == '<'){
				pCrtCh++;
				state=17;
			}
			else if (ch == '>'){
				pCrtCh++;
				state=20;
			}
			else if (ch == ','){
				pCrtCh++;
				state=29;
			}
			else if (ch == ';'){
				pCrtCh++;
				state=30;
			}
			else if (ch == '('){
				pCrtCh++;
				state=31;
			}
			else if (ch == ')'){
				pCrtCh++;
				state=32;
			}
			else if (ch == '['){
				pCrtCh++;
				state=33;
			}
			else if (ch == ']'){
				pCrtCh++;
				state=34;
			}
			else if (ch == '{'){
				pCrtCh++;
				state=35;
			}
			else if (ch == '}'){
				pCrtCh++;
				state=36;
			}
			else if (ch== '0'){
				pStartCh=pCrtCh;
				pCrtCh++;
				state=45;
			}
			else if (ch >= '1'&& ch<= '9'){// pt CT_INT
				pStartCh=pCrtCh;
				state=37;
			}
		
			else if(ch == '\''){// pt CT_CHAR
				pCrtCh++;
				state=50;
			}
			else if(ch =='"'){// pt CT_STRING
				first=1;
				pCrtCh++;
				state=54;
			}
			else {
				tkerr(addTk(END), "caracter invalid");
			}

			break;

		case 1:
			if (isalnum(ch) || ch == '_')
				pCrtCh++;
			else state = 2;
				break;
		case 2:
			nCh = pCrtCh - pStartCh; // lungimea cuvantului gasit
			// teste cuvinte cheie
			if (nCh == 2 && !memcmp(pStartCh, "if", 2))tk = addTk(IF);
			else if (nCh == 3 && !memcmp(pStartCh, "for", 3))tk = addTk(FOR);
			else if (nCh == 3 && !memcmp(pStartCh, "int", 3))tk = addTk(INT);
			else if (nCh == 4 && !memcmp(pStartCh, "void", 4))tk = addTk(VOID);
			else if (nCh == 4 && !memcmp(pStartCh, "char", 4))tk = addTk(CHAR);
			else if (nCh == 4 && !memcmp(pStartCh, "else", 4))tk = addTk(ELSE);
			else if (nCh == 5 && !memcmp(pStartCh, "break", 5))tk = addTk(BREAK);
			else if (nCh == 5 && !memcmp(pStartCh, "while", 5))tk = addTk(WHILE);
			else if (nCh == 6 && !memcmp(pStartCh, "double", 6))tk = addTk(DOUBLE);
			else if (nCh == 6 && !memcmp(pStartCh, "return", 6))tk = addTk(RETURN);
			else if (nCh == 6 && !memcmp(pStartCh, "struct", 6))tk = addTk(STRUCT);

			else{ // daca nu este un cuvant cheie, atunci e un ID
				
				tk = addTk(ID);
				tk->text = createString(pStartCh, pCrtCh);

			}
			return tk->code;
		case 3:
			if (ch == '='){
				pCrtCh++;
				state = 4;
			}
			else state = 5;
			break;
		case 4:
			addTk(EQUAL);
			return EQUAL;
		case 5:
			addTk(ASSIGN);
			return ASSIGN;
		case 6:
			addTk(ADD);
			return ADD;
		case 7:
			addTk(SUB);
			return SUB;
		case 8:
			addTk(MUL);
			return MUL;
		case 9:
			addTk(DOT);
			return DOT;

		case 10:
			if(ch == '&'){
				pCrtCh++;
				state=11;
			}
			break;
		case 11:
			addTk(AND);
			return AND;
		case 12:
			if(ch == '|'){
				pCrtCh++;
				state=13;
			}
			break;
		case 13:
			addTk(OR);
			return OR;
		case 14:
			if(ch == '='){
				pCrtCh++;
				state=15;
			}
			else 
				state=16;
			break;
		case 15:
			addTk(NOTEQ);
			return NOTEQ;

		case 16:
			addTk(NOT);
			return NOT;

		case 17:

			if(ch == '='){
				pCrtCh++;
				state=18;
			}
			else 
				state=19;
			break;
		case 18:
			addTk(LESSEQ);
			return LESSEQ;
		case 19:
			addTk(LESS);
			return LESS;
		case 20:
			if(ch == '='){
				pCrtCh++;
				state=21;
			}
			else 
				state=22;
			break;
		case 21:
			addTk(GREATEREQ);
			return GREATEREQ;
		case 22:
			addTk(GREATER);
			return GREATER;
		case 23:
			if(ch == '/'){
				pCrtCh++;
				state=24;
			}
			else if(ch =='*'){
				pCrtCh++;
				state=25;
			}
			else
				state=26;
			break;
		case 24:
			if (ch != '\n' && ch != '\r' && ch != '\0'){
				pCrtCh++;
			}
			else
				state=0;
			break;
		case 25:
			if(ch == '*'){
				pCrtCh++;
				state=27;
			}
			break;
		case 26:
			addTk(DIV);
			return DIV;
		case 27:
			if(ch != '*'){
				pCrtCh++;
			}
			else
				state=28;
			break;
		case 28:
			if(ch == '*'){
				pCrtCh++;
			}
			else if(ch =='/'){
				pCrtCh++;
				state=0;
			}
			else
				state=27;
			break;
		case 29:
			addTk(COMMA);
			return COMMA;
		case 30:
			addTk(SEMICOLON);
			return SEMICOLON;
		case 31:
			addTk(LPAR);
			return LPAR;
		case 32:
			addTk(RPAR);
			return RPAR;
		case 33:
			addTk(LBRACKET);
			return LBRACKET;
		case 34:
			addTk(RBRACKET);
			return RBRACKET;
		case 35:
			addTk(LACC);
			return LACC;
		case 36:
			addTk(RACC);
			return RACC;
		case 37:
			if(ch>='0'&&ch<='9'){
				pCrtCh++;
			}
			else if(ch == '.'){
				pCrtCh++;
				state = 39;
			}
			else if(ch=='e'||ch=='E'){
				pCrtCh++;
				state=41;
			}
			else 
				state=38;
			break;
		case 38:
		
			tk=addTk(CT_INT);
			s=createString(pStartCh,pCrtCh);
			
			if(strchr(s,'x')!=0){
				tk->i=strtol(s,NULL,16);//baza 16
			}
			else if(*s=='0'){
				tk->i=strtol(s,NULL,8);//baza 8
			}
			else{
				tk->i=strtol(s,&s,10);//baza 10
			}

			return CT_INT;
		case 39:
			if(ch>='0'&&ch<='9'){
				pCrtCh++;
				state=40;
			}
			break;
		case 40:
			if(ch>='0'&& ch<='9'){
				pCrtCh++;
			}
			else if(ch =='e'|| ch =='E'){
				pCrtCh++;
				state=41;
			}
			else
				state=44;
			break;
		case 41:
			if(ch== '+'||ch=='-'){
				pCrtCh++;
				state=42;
			}
			else if(ch>='0'&&ch<='9'){
				pCrtCh++;
				state=43;
			}
			break;
		case 42:
			if(ch>='0'&&ch<='9'){
				pCrtCh++;
				state=43;
			}
			break;
		case 43:
			if(ch>='0'&&ch<='9'){
				pCrtCh++;
			}
			else
				state=44;
			break;
		case 44:

			tk=addTk(CT_REAL);
			s=createString(pStartCh,pCrtCh);
			tk->r=strtof(s,&s);
	
			return CT_REAL;
		case 45:
			if(ch=='x'){
				pCrtCh++;
				state=46;
			}
			else if(ch=='.'){
				pCrtCh++;
				state=39;
			}
			else if((ch<'0'||ch>'9')&&(ch!='e')&&(ch!='E')){
				state=38;
			}
			else{
				pCrtCh++;
				state=48;
			}
			break;
		case 46:
			if((ch>='a'&&ch<='f')||(ch>='A'&&ch<='F')||(ch>='0'&&ch<='9')){
				pCrtCh++;
				state=47;
			}
			break;
		case 47:
			if((ch>='a'&&ch<='f')||(ch>='A'&&ch<='F')||(ch>='0'&&ch<='9')){
				pCrtCh++;
			}
			else
				state=38;
			break;
		case 48:
			if(ch>='0'&&ch<='7'){
				pCrtCh++;
			}
			else if (ch=='8'||ch=='9'){
				pCrtCh++;
				state=49;
			}
			else
				state=38;
			break;
		case 49:
			if(ch>='0'&&ch<='9'){
				pCrtCh++;
			}
			else if(ch=='e'||ch=='E'){
				pCrtCh++;
				state=41;
			}
			else if(ch== '.'){
				pCrtCh++;
				state=39;
			}
			break;
		case 50:
			if((ch!='\'') && (ch!='\\')){
				pStartCh=pCrtCh;
				pCrtCh++;
			}
				
			else if(ch=='\\'){
				pStartCh=pCrtCh;
				pCrtCh++;
				state=51;
			}
			else{
				pCrtCh++;
				state=53;

			}
			break;
		case 51:
			if(strchr("abfnrtv\'?\"\\0",ch)){
				pCrtCh++;
				state=52;
			}
			break;
		case 52:
			if(ch== '\''){
				pCrtCh++;
				state=53;
			}
			break;
		case 53:
			tk=addTk(CT_CHAR);
			s=createString(pStartCh,pCrtCh);
			tk->i=*s;
			return CT_CHAR;
		case 54:
			if(first){//pt a marca inceputul stringului
				pStartCh=pCrtCh;

			}
			if((ch!='\"')&&(ch!='\\')){
				pCrtCh++;
			}
			else if(ch== '\\'){
				pCrtCh++;
				state=56;
			}
			else{
				pCrtCh++;
				state=58;
			}
			first=0;
			 break;
			

		case 56:
			if(strchr("abfnrtv\'?\"\\0",ch)){
				pCrtCh++;
				state=57;
			}
			break;
		case 57:
			if(ch=='\"'){
				state=58;
			}
			else{
				pCrtCh++;
				state=54;
			}
			break;
		case 58:
			tk=addTk(CT_STRING);
			s=createString(pStartCh,pCrtCh-1);
			s=unescapeChar(s);
			tk->text=s;
			return CT_STRING;
		
		case 59:
			if(ch=='/'){
				pCrtCh++;
				state=60;
			}
			else if(ch=='*'){
				pCrtCh++;
				state=61;
			}
			else
				state=23;
			break;
		case 60:
			if(ch!='\n'&& ch != '\r' && ch != '\0')
				pCrtCh++;
			else
				state=0;
			break;

		case 61:
			if(ch!='*')
				pCrtCh++;
			else{
				pCrtCh++;
				state=62;
			}
			break;
		case 62:
			if(ch=='*')
				pCrtCh++;
			else if(ch=='/'){
				pCrtCh++;
				state=0;
			}
			else {
				pCrtCh++;
				state=61;
			}
			break;
		}
	}
}


void showAtoms(Token *tk){
	static char* names[]={"ID", "END", "CT_INT","CT_REAL","CT_CHAR","CT_STRING",		//replace la , cu ","
	"BREAK", "CHAR", "DOUBLE", "ELSE","FOR", "IF", "INT","RETURN"," STRUCT", "VOID", "WHILE",
	"COMMA", "SEMICOLON", "LPAR", "RPAR", "LBRACKET", "RBRACKET", "LACC", "RACC",""
	"ADD", "SUB", "MUL"," DIV", "DOT", "AND", "OR", "NOT", "ASSIGN", "EQUAL", "NOTEQ", "LESS", "LESSEQ", "GREATER", "GREATEREQ",
	};
	const char *str;

		str=names[tk->code];

		printf("%s ",str);

		if(strcmp(str,"CT_INT")==0){
			printf(": %ld ",tk->i);
		}
		else if(strcmp(str,"CT_CHAR")==0){
			printf(": %c ",tk->i);

		}
		else if(strcmp(str,"CT_REAL")==0){
			printf(": %.02f ",tk->r);
		}
		else if((strcmp(str,"CT_STRING")==0))
			printf(": %s ",tk->text);
		else if(strcmp(str,"ID")==0){
			printf(": %s ",tk->text);
		}

}
int consume(int code){
	if(crtTk->code==code){
		consumedTk=crtTk;
		crtTk=crtTk->next;
		return 1;

	}
	return 0;
}
int arrayDecl(){
	if(!consume(LBRACKET)){
		return 0;
	}
	expr();
	if(consume(RBRACKET)){
	}else tkerr(crtTk,"missing ]");
	
	return 1;
}

int typebase(){
	Token *startTk=crtTk;

	if(consume(INT)){
		return 1;
	}
	else if(consume(DOUBLE)){
		return 1;
	}
	else if(consume(CHAR)){
		return 1;
	}
	else if (consume(STRUCT)){
		if(consume(ID)){
			return 1;
		}
		else tkerr(crtTk,"missing ID");

	}else tkerr(crtTk,"missing STRUCT");
	
	else tkerr(crtTk,"invalid declaration ");
	crtTk=startTk;
	return 0;
}
int typeName(){
	if(!typebase())
		return 0;
	arrayDecl();
	return 1;
} 
int declVar(){
	if(typebase()){
		if(typeName()){//pt ID arrayDecl?
				while(1){
					if(consume(COMMA)){
						if(typeName()){
							return 1;
						}else tkerr(crtTk,"invalid declaration");
					}else tkerr(crtTk,"missing ,");
					
					else
						break;
				}
				if(consume(SEMICOLON)){
					return 1;
				}else tkerr(crtTk,"missing ;");
			
		}else tkerr("invalid declaration");
	}
	return 0;
}
int exprOr(){// exprOr OR exprAnd | exprAnd - wtf?
	if(exprOr()){
		if(consume(OR)){
			if(exprAnd()){
				return 1;
			}
			else if()
		}else tkerr(crtTk,"missing || ");
	}
	return 0;
}

int exprAssign(){
	if(exprUnary()){
		if(consume(ASSIGN)){
			if(exprAssign){
				return 1;
			}
			else if(exprOr()){
				return 1;
			}
		}else tkerr("missing = ");
	}
	return 0;
}
int expr(){
	if(exprAssign()){
		return 1;
	}
	return 0;
}
int ruleExpr(){
	expr();
	if(consume(SEMICOLON)){
		return 1;
	}else tkerr(crtTk,"missing ; in expr rule");
	return 0;
}

int ruleReturn(){
	if(!consume(RETURN)){
		return 0;
	}
	expr();
	if(consume(SEMICOLON)){
		return 1;
	}else tkerr(crtTk,"missing ;");
	return 0;
}
int ruleFor(){
	if(consume(FOR)){
		if(!consume(LPAR)){
			tkerr(crtTk,"missing ( after FOR");
		}
		expr();
		if(!consume(SEMICOLON)){
			tkerr(crtTk,"missing ; in FOR rule");
		}
		expr();
		if(!consume(SEMICOLON)){
			tkerr(crtTk,"missing ; ");
		}
		expr();
		if(consume(RPAR)){
			if(stm()){
				return 1;
			}else tkerr(crtTk,"missing FOR statement");
		}else tkerr(crtTk,"missing )");
	}
	return 0;
}
int ruleWhile{
	if(!consume(WHILE))return 0;
	if(!consume(LPAR))tkerr(crtTk,"missing ( after while");
	if(!expr())tkerr(crtTk,"invalid expression after (");
	if(!consume(RPAR))tkerr(crtTk,"missing )");
	if(!stm())tkerr(crtTk,"missing while statement");
	return 1;
}
int ruleInt(){
	if(consume(IF)){
		if(consume(LPAR)){
			if(expr()){
				if(consume(RPAR)){
					if(!stm()){
						tkerr(crtTk,"missing IF statement");
					}
					if(consume(ELSE)){
						if(stm()){
							return 1;
						}
					}
					return 1;
				}else tkerr(crtTk,"missing )");
			}else tkerr(crtTk,"invalid expression after (");
		}else tkerr(crtTk,"missing ( after IF");
	}
	return 0;
}
int stm(){
	Token *startTk=crtTk;
	if(consume(IF)){
		crtTk=startTk;
		ruleInt();
	}
	else if(consume(WHILE)){
		crtTk=startTk;
		ruleWhile();
	}
	else if(consume(FOR)){
		crtTk=startTk;
		ruleFor();
	}
	else if(consume(BREAK)){
		if(consume(SEMICOLON)){
			return 1;
		}
		else tkerr(crtTk,"missing ; after BREAK");
	}
	else if(consume(RETURN)){
		crtTk=startTk;
		ruleReturn();
	}
	else if(expr()){
		crtTk=startTk;
		ruleExpr();
	}
	return 0;
}
int stmCompound(){
	if(!consume(LACC))
		return 0;
	while(1){
		if(declVar()){
		}
		else if(stm()){
		}
		else break;
	}
	if(!consume(RACC))tkerr(crtTk,"missing } or syntax error");
		return 1;
}


int funcArg(){
	if(typebase()){
		if(!consume(ID))
			return 0;
		arrayDecl();
		return 1;
		}
	}
	return 0;
}
int declFunc(){
	if(typebase()){
		if(consume(MUL)){
			
		}
		if(consume(VOID)){
			return 1;
		}
		if(consume(ID)){
			if(!consume(LPAR)){
				return 0;
			}
			funcArg();
			while(1){
				if(consume(COMMA)){
					if(funcArg()){
						return1;
					}else tkerr(crtTk,"invalid declaration");
				}
				else{
					tkerr(crtTk,"missing ,");
					break;
				}
			}
			if(consume(RPAR)){
				if(stmCompound()){
					return 1;
				}
			}else tkerr(crtTk,"missing )");
		}else tkerr(crtTk,"missing ID");
	}
	return 0;
}
int declStruct(){
	if(consume(STRUCT)){
		if(consume(ID)){
			if(consume(LACC)){
				while(1){
					if(declVar()){
						
					}
					else
						break;
				}
				if(consume(RACC)){
					if(consume(SEMICOLON)){
						return 1;
					}else tkerr(crtTk,"missing ;");
				}else tkerr(crtTk,"missing }");
			}else tkerr(crtTk,"missing {");
		}else tkerr(crtTk,"missing ID");
	}else tkerr(crtTk,"missing STRUCT");
	return 0;
}
int unit(){
	while(1){
		if(declStruct()){
		}
		else if (declFunc()){
		}
		else if(declVar){
		}
		else break;
	}
	if(consume(END)){
		return 1;
	}
	return 0;
}
int main(){
	int fdr,fdw,nr,code;
	char c;

	fdr = open("cod.txt",O_RDONLY);
	fdw = open("response.txt",O_WRONLY);

	nr=read(fdr,&input,sizeof(input));
	if(nr==-1){
		printf("Eroare la citirea din fisier.\n");
	}
	
	pCrtCh = input;

 	while((code=getNextToken())!=END){
 		showAtoms(lastToken);
    };

    showAtoms(lastToken);  

	close(fdr);
	close(fdw);

	return 0;

}
//TODO
//vezi ?
//restul functiilor
//cand trebuie sa punem starttk bla bla?--> TREBUIE PUS LA TOATE?
//eliminarea rec stangi
