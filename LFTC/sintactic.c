
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include  "lexical.h"


#define SAFEALLOC(var,Type) if((var=(Type*)malloc(sizeof(Type)))==NULL)err("not enough memory");

Token *crtTk,*consumedTk;

int expr();
int exprPostfix();
int exprPostfix_aux();
int exprPrimary();
int exprMul();
int exprMul_aux();
int exprAdd();
int exprAdd_aux();
int exprRel();
int exprRel_aux();
int exprAnd();
int exprAnd_aux();
int exprEq();
int exprEq_aux();
int exprOr_aux();
int stm();
int stmCompound();

typedef union{ 
	long int i; // int, char
	double d; // double 
	const char *str; // char[]
}CtVal;

 struct _Symbol;
 typedef struct _Symbol Symbol;
enum{TB_INT,TB_DOUBLE,TB_CHAR,TB_STRUCT,TB_VOID}; 
static char *typesName[]={"TB_INT","TB_DOUBLE","TB_CHAR","TB_STRUCT","TB_VOID"};

typedef struct{
	int typeBase; // TB_* 
	Symbol *s; // struct definition for TB_STRUCT 
	int nElements; // >0 array of given size, 0=array without size, <0 non array 
}Type;

typedef struct{
	Type type; // type of the result 
	int isLVal; // if it is a LVal 
	int isCtVal; // if it is a constant value (int, real, char, char[])
	CtVal ctVal; // the constat value 
}RetVal;


 typedef struct{
	 Symbol **begin; // the beginning of the symbols, or NULL
	 Symbol **end; // the position after the last symbol
	 Symbol **after; // the position after the allocated space 
}Symbols; 

enum{CLS_VAR,CLS_FUNC,CLS_EXTFUNC,CLS_STRUCT}; 
static char *clsNames[]={"CLS_VAR","CLS_FUNC","CLS_EXTFUNC","CLS_STRUCT"};

enum{MEM_GLOBAL,MEM_ARG,MEM_LOCAL};
static char *memNames[]={"MEM_GLOBAL","MEM_ARG","MEM_LOCAL"};

typedef struct _Symbol{
	const char *name; // a reference to the name stored in a token 
	int cls; // CLS_*
	int mem; // MEM_*
	Type type;
	int depth; // 0-global, 1-in function, 2... - nested blocks in function
	union{ 
		Symbols args; // used only of functions
		Symbols members; // used only for structs
		}; 

	union{
		void *addr;// vm: the memory address for global symbols
		int offset;// vm: the stack offset for local symbols
		};
}Symbol; 

 Symbols symbols,*symbolsTab;
int crtDepth=0;
Symbol *crtFunc=NULL,*crtStruct=NULL;


void initSymbols(Symbols *symbols) { 
	symbols->begin=NULL;
	symbols->end=NULL;
	symbols->after=NULL; 
}

Symbol *addSymbol(Symbols *symbols,const char *name,int cls) {
	Symbol *s;
	if(symbols->end==symbols->after){ // create more room
		int count=symbols->after-symbols->begin;
		int n=count*2; // double the room 
		if(n==0)
			n=1; // needed for the initial case
		symbols->begin=(Symbol**)realloc(symbols->begin, n*sizeof(Symbol*)); 
		if(symbols->begin==NULL)
			err("not enough memory");
		symbols->end=symbols->begin+count; 
		symbols->after=symbols->begin+n;
	}
	SAFEALLOC(s,Symbol) *symbols->end++=s;
	s->name=name; 
	s->cls=cls; 
	s->depth=crtDepth; 
	return s;
}

int deleteSymbolsAfter(Symbols *st, Symbol *symbol) {
    int i, j, n;
    n = st->end - st->begin;

    for (i = 0; i < n; i++) {
        if (st->begin[i] == symbol) {
            for (j = n - 1; j > i; j--) {
                free(st->begin[j]);
            }
            st->end = st->begin + i + 1;
            return 1;
        }
    }
    return 0;
}
Symbol *findSymbol(Symbols *symbols,const char *name){
	int n,i;
	Symbol *tmp=NULL;
	n=symbols->end-symbols->begin;
	for(i=n-1;i>=0;i--){
		if(strcmp(symbols->begin[i]->name,name)==0){
			tmp= symbols->begin[i];
		}

	}
	return tmp;
}	

int showSymbols() {
    int i, n;
    if (symbolsTab == NULL){

        return 0;
    }
    n = symbolsTab->end - symbolsTab->begin;

    for (i = 0; i < n; i++) {
    	if(strcmp(clsNames[symbolsTab->begin[i]->cls],"CLS_STRUCT")==0){
    		printf("%s\t%s\t\t%s\n", symbolsTab->begin[i]->name,clsNames[symbolsTab->begin[i]->cls],memNames[symbolsTab->begin[i]->mem]);

    	}
        else
        	printf("%s\t%s\t\t%s\t\t%s\n", symbolsTab->begin[i]->name,clsNames[symbolsTab->begin[i]->cls],memNames[symbolsTab->begin[i]->mem],typesName[symbolsTab->begin[i]->type.typeBase]);
    }

    free(symbolsTab);
    return 1;
}
Type createType(int typeBase,int nElements) {
	Type t;
	t.typeBase=typeBase;
	t.nElements=nElements;
	return t;
}
void put_i(){
	printf("#%ld\n",popi());
}
void cast(Type *dst,Type *src) { 
	if(src->nElements>-1){
		if(dst->nElements>-1){ 
			if(src->typeBase!=dst->typeBase) tkerr(crtTk,"an array cannot be converted to an array of another type");
		}else{ 
			tkerr(crtTk,"an array cannot be converted to a non-array");
		} 
		}else{ 
			if(dst->nElements>-1){
				tkerr(crtTk,"a non-array cannot be converted to an array"); 
			}	 
		} 
	switch(src->typeBase){ 
		case TB_CHAR:
		case TB_INT:
		case TB_DOUBLE:
			switch(dst->typeBase){
				case TB_CHAR: 
				case TB_INT:
				case TB_DOUBLE:
					return; 
				}
		case TB_STRUCT: 
			if(dst->typeBase==TB_STRUCT){	
				if(src->s!=dst->s) 
					tkerr(crtTk,"a structure cannot be converted to another one"); 
				return;
				}
	}
	tkerr(crtTk,"incompatible types"); 
}
Symbol *addExtFunc(const char *name,Type type,void *addr) {
	Symbol *s=addSymbol(symbolsTab,name,CLS_EXTFUNC);
	s->type=type; 
	s->addr=addr;
	initSymbols(&s->args); 
	return s; 
}

Symbol *addFuncArg(Symbol *func,const char *name,Type type) {
	Symbol *a=addSymbol(&func->args,name,CLS_VAR); 
	a->type=type;
	return a;
}
Type getArithType(Type *s1, Type *s2) {
    Type t;
	if(s1->typeBase < s2->typeBase){
		t.typeBase=s1->typeBase;
	}
	else{
		t.typeBase=s2->typeBase;
	}
    t.s = s1->s;
    t.nElements = s1->nElements;
    return t;
}
void init(){
	Symbol *s,*a;
	 SAFEALLOC(symbolsTab,Symbols);
	//SAFEALLOC(crtTk,Token);
	//SAFEALLOC(consumedTk,Token);

	initSymbols(symbolsTab);

	//symbolsTab=(Symbols*)realloc(symbolsTab,10);
	s=addExtFunc("put_s",createType(TB_VOID,-1),put_s);
	//addFuncArg(s,"s",createType(TB_CHAR,0));
	a=addSymbol(&s->args,"s",CLS_VAR);
	a->type=createType(TB_CHAR,-1);
	
	s=addExtFunc("get_s",createType(TB_VOID,-1),get_s);
	//addFuncArg(s,"s",createType(TB_CHAR,0));
	a=addSymbol(&s->args,"s",CLS_VAR);
	a->type=createType(TB_CHAR,-1);
	
	s=addExtFunc("get_i",createType(TB_INT,-1),get_i);
	//a=addSymbol(&s->args,"s",CLS_VAR);
	a->type=createType(TB_INT,-1);
	
	s=addExtFunc("put_d",createType(TB_VOID,-1),put_d);
	//addFuncArg(s,"d",createType(TB_DOUBLE,-1));
	a=addSymbol(&s->args,"d",CLS_VAR);
	a->type=createType(TB_VOID,-1);
	
	s=addExtFunc("get_d",createType(TB_DOUBLE,-1),get_d);
	a->type=createType(TB_DOUBLE,-1);


	s=addExtFunc("put_c",createType(TB_VOID,-1),put_c);
	//addFuncArg(s,"c",createType(TB_CHAR,-1));
	a=addSymbol(&s->args,"c",CLS_VAR);
	a->type=createType(TB_VOID,-1);
	
	s=addExtFunc("get_c",createType(TB_CHAR,-1),get_c);
	a->type=createType(TB_CHAR,-1);
	
	s=addExtFunc("seconds",createType(TB_DOUBLE,-1),seconds);
	a->type=createType(TB_DOUBLE,-1);


	s=addExtFunc("put_i",createType(TB_VOID,-1),put_i);
	//addFuncArg(s,"s",createType(TB_INT,-1));
	a=addSymbol(&s->args,"s",CLS_VAR);
	a->type=createType(TB_INT,-1);
	
	//showSymbols();
}
void addVar(Token *tkName,Type t){
    Symbol      *s;
   // printf("addVar:%s\n",tkName->info.text);
    if(crtStruct){
    	//printf("addVar crtStruct\n");
        if(findSymbol(&crtStruct->members,tkName->info.text))
            tkerr(crtTk,"symbol redefinition: %s",tkName->info.text);
        s=addSymbol(&crtStruct->members,tkName->info.text,CLS_VAR);
        }
    else if(crtFunc){
    	//printf("addVar crtFunc\n");

        s=findSymbol(symbolsTab,tkName->info.text);

        if(s&&s->depth==crtDepth)
            tkerr(crtTk,"symbol redefinition: %s",tkName->info.text);
        s=addSymbol(symbolsTab,tkName->info.text,CLS_VAR);
   
        s->mem=MEM_LOCAL;
        }
    else{
    	 // printf("addVar else.%p\n",symbolsTab);

        if(findSymbol(symbolsTab,tkName->info.text))
            tkerr(crtTk,"symbol redefinition: %s",tkName->info.text);

        	s=addSymbol(symbolsTab,tkName->info.text,CLS_VAR);   
        	s->mem=MEM_GLOBAL;
        }
    s->type=t;
   // printf("addVar end\n");
}

int consume(int code){

	if(crtTk->code==code){
		consumedTk=crtTk;
		crtTk=crtTk->next;

	//printf("s-a consumat %s\n",names[code]);
		return 1;
	}
	//printf("s-a incercat consumarea %s, dar s-a gasit %s\n",names[code],names[crtTk->code]);
	return 0;
}
int arrayDecl(Type *ret){
	//printf("\n \tfunctia arrayDecl ");
	Token *startTk=crtTk;
	 RetVal rv;
	if(!consume(LBRACKET)){
		return 0;
	}
	if(expr(&rv)){
		if(!rv.isCtVal)
			tkerr(crtTk,"the array size is not a constant");
        if(rv.type.typeBase!=TB_INT)
			tkerr(crtTk,"the array size is not an integer");
        ret->nElements=rv.ctVal.i;
    }else
		ret->nElements=0; 
		
	if(consume(RBRACKET)){
		return 1;
	}else tkerr(crtTk,"missing ]");
	
	crtTk=startTk;
	return 0;
}

int typebase(Type *ret){
	//printf("\n \tfunctia typebase ");

	Token *startTk=crtTk, *tkName;

	if(consume(INT)){
		ret->typeBase=TB_INT;
		return 1;
	}
	else if(consume(DOUBLE)){
		ret->typeBase=TB_DOUBLE;
		return 1;
	}
	else if(consume(CHAR)){
		ret->typeBase=TB_CHAR;

		return 1;
	}
	else if (consume(STRUCT)){
		if(consume(ID)){
			tkName=consumedTk;
			Symbol *s=findSymbol(symbolsTab,tkName->info.text);
			if(s==NULL)
				tkerr(crtTk,"undefined symbol: %s",tkName->info.text);
			if(s->cls!=CLS_STRUCT)
				tkerr(crtTk,"%s is not a struct",tkName->info.text);
			ret->typeBase=TB_STRUCT;
			ret->s=s;
			
			return 1;
		}
		else tkerr(crtTk,"missing ID");

	}
	
	crtTk=startTk;
	return 0;
}
int typeName(Type *ret){
	//printf("\n \tfunctia typeName ");
	 SAFEALLOC(ret,Type);

	if(typebase(ret)){
		if(!arrayDecl(ret)){
			ret->nElements=-1;
		}
		return 1;
	}
	return 0;
} 

int declVar(){
	//printf("\n \tfunctia declVar ");
	Type t;
	Token *tkName;
	Token *startTk=crtTk;

	if(typebase(&t)){
		 if(consume(ID)){
			tkName=consumedTk;
			if(!arrayDecl(&t)){
                t.nElements=-1;
            }
                addVar(tkName,t);

			while(1){
				if(consume(COMMA)){
					if(consume(ID)){
						tkName=consumedTk;
						if(!arrayDecl(&t)){
							t.nElements=-1;
						}
							addVar(tkName,t);
						
					}else tkerr(crtTk,"missing ID after ,");
				}else 
					break;

			}
			if(consume(SEMICOLON)){
				return 1;
			}else tkerr(crtTk,"missing ; in declVar");
			
		}
	}

	crtTk=startTk;
	return 0;
}

/*exprPostfix: exprPrimary exprPostfix_aux
	exprPostfix_aux:LBRACKET expr RBRACKET exprPostfix_aux
					|DOT ID exprPostfix_aux
					|epsilon*/

int exprPostfix(RetVal *rv){
	//printf("\n \tfunctia exprPostfix ");
	Token *startTk=crtTk;
	
	if(exprPrimary(rv)){
		if(exprPostfix_aux(rv)){
			return 1;
		}
	}
	crtTk=startTk;
	return 0;
}


int exprPostfix_aux(RetVal *rv){
	//printf("\n \tfunctia exprPostfix_aux ");
	RetVal rve;
	Token *startTk=crtTk;
		
	 if(consume(LBRACKET)){
	 	if(expr(&rve)){
			if (rv->type.nElements < 0)
                    tkerr(crtTk, "only an array can be indexed");
            Type typeInt = createType(TB_INT, -1);
            cast(&typeInt, &rve.type);
            //rv->type = rv.type;
            rv->type.nElements = -1;
            rv->isLVal = 1;
			rv->isCtVal = 0;
			
	 		if(consume(RBRACKET)){
	 			if(exprPostfix_aux(rv))
	 				return 1;
	 		}else err("missing ] in exprPostfix");
	 	}else err("missing expr after [ in exprPostfix");
	 }
	 crtTk=startTk;
	 if(consume(DOT)){
	 	if(consume(ID)){
			 Token *tkName = consumedTk;
			 Symbol *sStruct = rv->type.s;
			Symbol *sMember = findSymbol(&sStruct->members, tkName->info.text);
			if (!sMember)
				tkerr(crtTk, "struct %s does not have a member %s ",sStruct->name, tkName->info.text);
            rv->type = sMember->type;
            rv->isLVal = 1;
			rv->isCtVal = 0;
			
	 		if(exprPostfix_aux(rv))
	 			return 1;
	 	}else err("missing ID after . in exprPostfix_aux");
	 }
	 crtTk=startTk;
	 return 1;
}

 
int exprUnary(RetVal *rv){
	//printf("\n \tfunctia exprUnary ");

	Token *startTk=crtTk, *tkOp;
	
if(consume(SUB)||consume(NOT)){
		tkOp = consumedTk;
		
		if(exprUnary(rv)){
			if(tkOp->code==SUB){
				if(rv->type.nElements>=0)tkerr(crtTk,"unary '-' cannot be applied to an array");
				if(rv->type.typeBase==TB_STRUCT)
					tkerr(crtTk,"unary '-' cannot be applied to a struct");

            }else{  // NOT
				if(rv->type.typeBase==TB_STRUCT)tkerr(crtTk,"'!' cannot be applied to a struct");
				rv->type=createType(TB_INT,-1);
            }
			rv->isCtVal=rv->isLVal=0;
			
			return 1;
		}else err("missing expr after SUB in exprUnary");

	}
	/*else if(consume(NOT)){
		if(exprUnary()){
			return 1;
		}
		else err("missing expr after NOT in exprUnary");
	}
	*/
	else if(exprPostfix(rv)){
		return 1;
	}
	
	crtTk=startTk;
	return 0;
}
/*exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )?
           | CT_INT | CT_REAL | CT_CHAR
           | CT_STRING | LPAR expr RPAR ;*/
int exprPrimary(RetVal *rv){
	//printf("\n \tfunctia exprPrimary \n");
	int flag=0;//e ok flag-ul asta?
	Token *startTk=crtTk;
	Token *tkName;
    Symbol *s;
    Symbol **crtDefArg;
	RetVal arg;
	
	if(consume(ID)){
		 tkName = consumedTk;
		flag=1;
		s = findSymbol(symbolsTab, tkName->info.text);
		if (!s)
			tkerr(crtTk, "undefined symbol %s", tkName->info.text);
		rv->type = s->type;
		rv->isCtVal = 0;
		rv->isLVal = 1;
		
		if(consume(LPAR)){
			Symbol **crtDefArg=s->args.begin;
			if(s->cls!=CLS_FUNC&&s->cls!=CLS_EXTFUNC)
				tkerr(crtTk,"call of the non-function %s",tkName->info.text);
			
			if(expr(&arg)){
				if (crtDefArg == s->args.end)
                        tkerr(crtTk, "too many arguments in call");
                cast(&(*crtDefArg)->type, &(arg.type));
				crtDefArg++;
				
				while(1){
					if(consume(COMMA)){
						if(expr(&arg)){
							 if (crtDefArg == s->args.end)
								tkerr(crtTk, "too many arguments in call");
							cast(&(*crtDefArg)->type, &(arg.type));
							crtDefArg++;

						}
					}
					else break;
				}
			}
			if(consume(RPAR)){
				 if (crtDefArg != s->args.end)
                        tkerr(crtTk, "too few arguments in call");
                rv->type = s->type;
				rv->isCtVal = rv->isLVal = 0;
				
				return 1;
			}else tkerr(crtTk,"missing ) in expression");
		}//else tkerr(crtTk,"missing ( in expression");
	}else if(consume(CT_INT)){
		Token *tki=consumedTk;
		rv->type = createType(TB_INT, -1);
		rv->ctVal.i = tki->info.i;
		rv->isCtVal = 1;
		rv->isLVal = 0;
		
		return 1;
	}else if(consume(CT_REAL)){
		 Token *tkr=consumedTk;
		rv->type = createType(TB_DOUBLE, -1);
		rv->ctVal.d = tkr->info.r;
		rv->isCtVal = 1;
		rv->isLVal = 0;
		
		return 1;
	}else if(consume(CT_CHAR)){
		Token *tkc=consumedTk;
		rv->type = createType(TB_CHAR, -1);
		rv->ctVal.str = tkc->info.text;
		rv->isCtVal = 1;
		rv->isLVal = 0;
		
		return 1;
	}else if(consume(CT_STRING)){
		Token *tks=consumedTk;
		rv->type = createType(TB_CHAR, 0);
		rv->ctVal.str = tks->info.text;
		rv->isCtVal = 1;
		rv->isLVal = 0;
		
		return 1;
	}else if(consume(LPAR)){
		if(expr(rv)){
			if (consume(RPAR))
			{
				return 1;
			}
		}
	}
	
	if(flag==1){
		return 1;
	}

	 crtTk = startTk;
	return 0;
}
int exprCast(RetVal *rv){
	//printf("\n \tfunctia exprCast ");
	Token *startTk=crtTk;
	Type t;
	RetVal rve;

	if(consume(LPAR)){
		if(typeName(&t)){
			if(consume(RPAR)){
				if(exprCast(&rve)){
					cast(&t,&rve.type);
					rv->type=t;
					rv->isCtVal=rv->isLVal=0;
					
					return 1;
				}
			}else err("missing ) in cast ");
		}
		else err("missing typeName after ( in cast ");
	}else if(exprUnary(rv)){
		return 1;
	}

	crtTk=startTk;
	return 0;
}

//exprMul: exprMul ( MUL | DIV ) exprCast | exprCast ;
//exprMul= exprCast exprMul_aux
//exprMul_aux= ( MUL | DIV ) exprCast exprMul_aux |epsilon
int exprMul(RetVal *rv){
	//printf("\n \tfunctia exprMul ");
	Token *startTk=crtTk;

	if(exprCast(rv)){
		if(exprMul_aux(rv)){
			return 1;
		}
	}

	crtTk=startTk;
	return 0;
}
int exprMul_aux(RetVal *rv){
	//printf("\n \tfunctia exprMul_aux");
	 Token *startTk = crtTk;
    RetVal rve;
	Token *tkOp;
	
	if(consume(MUL)|| consume(DIV)){
		 tkOp = consumedTk;
		if(exprCast(&rve)){
			 if (rv->type.nElements > -1 || rve.type.nElements > -1)
                    tkerr(crtTk, "an array cannot be multiplied or divided");
            if (rv->type.typeBase == TB_STRUCT|| rve.type.typeBase == TB_STRUCT)
                tkerr(crtTk, "a structure cannot be multiplied or divided");
            rv->type = getArithType(&rv->type, &rve.type);
			rv->isCtVal = rv->isLVal = 0;
			
			if(exprMul_aux(rv))
				return 1;
		}else err("missing expr after %s",names[startTk->code]);

	}
	
	crtTk=startTk;
	return 1;
}

//exprAdd= exprMul exprAdd_aux
//exprAdd_aux=( ADD | SUB ) exprMul exprAdd_aux |epsilon
int exprAdd(RetVal *rv){
	//printf("\n \tfunctia exprAdd");
	Token *startTk=crtTk;

	if(exprMul(rv)){
		if(exprAdd_aux(rv)){
			return 1;
		}
	}

	crtTk=startTk;
	return 0;
}

int exprAdd_aux(RetVal *rv){
	//printf("\n \tfunctia exprAnd_aux ");
	Token *startTk = crtTk;
    RetVal rve;
	Token *tkOp;

	if(consume(ADD)){
		 tkOp = consumedTk;
		if(exprMul(&rve)){
			 if (rv->type.nElements > -1 || rve.type.nElements > -1)
                    tkerr(crtTk, "an array cannot be added ");
             if (rv->type.typeBase == TB_STRUCT|| rve.type.typeBase == TB_STRUCT)
                tkerr(crtTk, "a structure cannot be added ");
            rv->type = getArithType(&rv->type, &rve.type);
			rv->isCtVal = rv->isLVal = 0;
			
			if(exprAdd_aux(rv))
				return 1;
		}else err("missing expr after +");

	}else if(consume(SUB)){
		 tkOp = consumedTk;
		if(exprMul(&rve)){
			 if (rv->type.nElements > -1 || rve.type.nElements > -1)
                    tkerr(crtTk, "an array cannot be subtracted");
            if (rv->type.typeBase == TB_STRUCT|| rve.type.typeBase == TB_STRUCT)
                tkerr(crtTk, "a structure cannot be subtracted");
            rv->type = getArithType(&rv->type, &rve.type);
			rv->isCtVal = rv->isLVal = 0;
			
			if(exprAdd_aux(rv))
				return 1;
		}else err("missing expr after -");
	}
	crtTk=startTk;
	return 1;
}

//expRel= exprAdd exprRel_aux
//expRel_aux=( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd exprRel_aux | epsilon 
int exprRel(RetVal *rv){
	//printf("\n \tfunctia exprRel");
	Token *startTk=crtTk;
	RetVal rve;
	
	if(exprAdd(rv)){
		if(exprRel_aux(rv)){
			return 1;
		}
	}

	crtTk=startTk;
	return 0;
}

int exprRel_aux(RetVal *rv){
	//printf("\n \tfunctia exprRel_aux");
	Token *startTk = crtTk;
    RetVal rve;
	Token *tkOp;

	if(consume(LESS)){
		 tkOp = consumedTk;
		if(exprAdd(&rve)){
			if (rv->type.nElements > -1 || rve.type.nElements > -1)
                    tkerr(crtTk, "an array cannot be compared");
            if (rv->type.typeBase == TB_STRUCT|| rve.type.typeBase == TB_STRUCT)
                tkerr(crtTk, "a structure cannot be compared");
            rv->type = createType(TB_INT, -1);
			rv->isCtVal = rv->isLVal = 0;
			
			if(exprRel_aux(rv))
				return 1;
		}else err("missing expression after <");

	}else if(consume(LESSEQ)){
		 tkOp = consumedTk;
			if(exprAdd(&rve)){
				 if (rv->type.nElements > -1 || rve.type.nElements > -1)
                    tkerr(crtTk, "an array cannot be compared");
				if (rv->type.typeBase == TB_STRUCT|| rve.type.typeBase == TB_STRUCT)
					tkerr(crtTk, "a structure cannot be compared");
				rv->type = createType(TB_INT, -1);
				rv->isCtVal = rv->isLVal = 0;
			
			if(exprRel_aux(rv))
				return 1;
		}else err("missing expression after <=");

	}else if(consume(GREATER)){
		 tkOp = consumedTk;
			if(exprAdd(&rve)){
				 if (rv->type.nElements > -1 || rve.type.nElements > -1)
                    tkerr(crtTk, "an array cannot be compared");
				if (rv->type.typeBase == TB_STRUCT|| rve.type.typeBase == TB_STRUCT)
					tkerr(crtTk, "a structure cannot be compared");
				rv->type = createType(TB_INT, -1);
				rv->isCtVal = rv->isLVal = 0;
				
			if(exprRel_aux(rv))
				return 1;
		}else err("missing expression after >");

	}else if(consume(GREATEREQ)){
		 tkOp = consumedTk;
			if(exprAdd(&rve)){
				 if (rv->type.nElements > -1 || rve.type.nElements > -1)
                    tkerr(crtTk, "an array cannot be compared");
				if (rv->type.typeBase == TB_STRUCT|| rve.type.typeBase == TB_STRUCT)
					tkerr(crtTk, "a structure cannot be compared");
				rv->type = createType(TB_INT, -1);
				rv->isCtVal = rv->isLVal = 0;
				
			if(exprRel_aux(rv))
				return 1;
		}else err("missing expression after >=");

	}
	crtTk = startTk;
	return 1;
}

//exprAnd:exprEq exprAnd_aux ;
//exprAnd_aux= AND exprEq exprAnd_aux | epsilon
int exprAnd(RetVal *rv){
	//printf("\n \tfunctia exprAnd ");
	
	Token *startTk=crtTk;

	if(exprEq(rv)){
		if(exprAnd_aux(rv)){
			return 1;
		}
	}

	crtTk=startTk;
	return 0;
}

int exprAnd_aux(RetVal *rv){
	//printf("\n \tfunctia exprAnd_aux");
	Token *startTk = crtTk;
	RetVal rve;
	
	if(consume(AND)){
		if(exprEq(&rve)){
			if (rv->type.typeBase == TB_STRUCT || rve.type.typeBase == TB_STRUCT)
                    tkerr(crtTk, "a structure cannot be logically tested");
            rv->type = createType(TB_INT, -1);
			rv->isCtVal = rv->isLVal = 0;
			
			if(exprAnd_aux(rv))
				return 1;

		}else err("missing expression after &&");
	}
	crtTk = startTk;
	return 1;
}

//exprOr=exprAnd exprOr_aux
//exprOr=OR exprAnd exprOr_aux | epsilon
int exprOr(RetVal *rv){
	//printf("\n \tfunctia exprOr");
	Token *startTk=crtTk;

	if(exprAnd(rv)){
		if(exprOr_aux(rv)){
			return 1;
		
		}
	}

	crtTk=startTk;
	return 0;
}

int exprOr_aux(RetVal *rv){
	//printf("\n \tfunctia exprOr_aux ");
	Token *startTk = crtTk;
	RetVal rve;
	
	if(consume(OR)){
		if(exprAnd(&rve)){
			 if (rv->type.typeBase == TB_STRUCT|| rve.type.typeBase == TB_STRUCT)
                 err("a structure cannot be logically tested");
            rv->type = createType(TB_INT, -1);
			rv->isCtVal = rv->isLVal = 0;
			
			if(exprOr_aux(rv))
			 return 1;
		}else err("missing expression after ||");
	}
	crtTk = startTk;
	return 1;		
}

//exprEq= exprRel exprEq_aux
//expreq_aux= (Equal|noteq) exprRel expreq_aux|epsilon
int exprEq(RetVal *rv){
	//printf("\n \tfunctia exprEq ");
	Token *startTk=crtTk;

	if(exprRel(rv)){
		if(exprEq_aux(rv)){
			return 1;
		}
	}

	crtTk=startTk;
	return 0;
}

int exprEq_aux(RetVal *rv){
	//printf("\n \tfunctia exprEq_aux ");
	 Token *tkStart = crtTk;
	RetVal rve;
	Token *tkOp;
	
	if(consume(EQUAL)){
		tkOp = consumedTk;
		if(exprRel(&rve)){
			 if (rv->type.typeBase == TB_STRUCT|| rve.type.typeBase == TB_STRUCT)
                    tkerr(crtTk, "a structure cannot be compared");
            rv->type = createType(TB_INT, -1);
			rv->isCtVal = rv->isLVal = 0;
			
			if(exprEq_aux(rv))
				return 1;
		}else err("missing expr after ==");
	}
	else if(consume(NOTEQ)){
		tkOp = consumedTk;
		if(exprRel(&rve)){
			 if (rv->type.typeBase == TB_STRUCT|| rve.type.typeBase == TB_STRUCT)
                    tkerr(crtTk, "a structure cannot be compared");
            rv->type = createType(TB_INT, -1);
			rv->isCtVal = rv->isLVal = 0;
			
			if(exprEq_aux(rv))
				return 1;
		
		}else err("missing expr after !=");
	}
	
	return 1;
}
int exprAssign(RetVal *rv){
	//printf("\n \tfunctia exprAssign ");
	Token *startTk=crtTk;
	 RetVal rve;
	 
	if(exprUnary(rv)){
		if(consume(ASSIGN)){
			if(exprAssign(&rve)){
				 if(!rv->isLVal)tkerr(crtTk,"cannot assign to a non-lval");
				 if(rv->type.nElements>-1||rve.type.nElements>-1)
					tkerr(crtTk,"the arrays cannot be assigned");
				cast(&rv->type,&rve.type);
				rv->isCtVal=rv->isLVal=0;
				return 1;
			}
		}
	}
	crtTk=startTk;
	if(exprOr(rv))
		return 1;
		
	crtTk=startTk;
	return 0;
}
int expr(RetVal *rv){
	//printf("\n \tfunctia expr ");
	 Token *startTk = crtTk;
	 
	if(exprAssign(rv)){
		return 1;
	}
	crtTk=startTk;
	return 0;
}
int ruleExpr(){
	//printf("\n \tfunctia ruleExpr");
	RetVal rv;
	if(expr(&rv)){
		if(!consume(SEMICOLON))
			tkerr(crtTk,"missing ; after expr");
		return 1;
	}
	if(consume(SEMICOLON)){
		return 1;
	}//else tkerr(crtTk,"missing ; in expr rule");
	return 0;
}

int ruleReturn(){
	//printf("\n \tfunctia ruleReturn ");
	RetVal rv;

	if(!consume(RETURN)){
		return 0;
	}
	expr(&rv);
    if(crtFunc->type.typeBase==TB_VOID)
		tkerr(crtTk,"a void function cannot return a value");
	cast(&crtFunc->type,&rv.type);
	if(consume(SEMICOLON)){
		return 1;
	}else tkerr(crtTk,"missing ; in ruleReturn");

	return 0;
}
int ruleFor(){
	//printf("\n \tfunctia ruleFor");
	RetVal rv1,rv2,rv3;
	
	if(consume(FOR)){
		if(!consume(LPAR)){
			tkerr(crtTk,"missing ( after FOR");
		}
		expr(&rv1);
		if(!consume(SEMICOLON)){
			tkerr(crtTk,"missing ; in FOR rule");
		}
		expr(&rv2);
		if(rv2.type.typeBase==TB_STRUCT)
            tkerr(crtTk,"a structure cannot be logically tested");
		if(!consume(SEMICOLON)){
			tkerr(crtTk,"missing ; in ruleFor");
		}
		expr(&rv3);
		if(consume(RPAR)){
			if(stm()){
				return 1;
			}else tkerr(crtTk,"missing FOR statement");
		}else tkerr(crtTk,"missing )");
	}

	return 0;
}
int ruleWhile(){
	//printf("\n \tfunctia ruleWhile");
	RetVal rv;

	if(!consume(WHILE))return 0;
	if(!consume(LPAR))tkerr(crtTk,"missing ( after while");
	if(expr(&rv)){
		if(rv.type.typeBase==TB_STRUCT)
                        tkerr(crtTk,"a structure cannot be logically tested");
	
		if(!consume(RPAR))tkerr(crtTk,"missing )");
		if(!stm())tkerr(crtTk,"missing while statement");
	}
	else tkerr(crtTk,"invalid expression after (");
	return 1;
}

int ruleIf(){
	//printf("\n \tfunctia ruleIf ");
	RetVal rv;
	if(consume(LPAR)){
		if(expr(&rv)){
			if(rv.type.typeBase==TB_STRUCT)
                        tkerr(crtTk,"a structure cannot be logically tested");
			if(consume(RPAR)){
				if(!stm()){
					//tkerr(crtTk,"missing IF statement");
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

	return 0;
}
int stm(){
	//printf("\n \tfunctia stm ");
	Token *startTk=crtTk;
	RetVal *rv;
	if(stmCompound()){
		return 1;
	}
	if(consume(IF)){
		//crtTk=startTk;
		if(ruleIf())
			return 1;
	}
	else if(consume(WHILE)){
		crtTk=startTk;
		if(ruleWhile())
			return 1;
	}
	else if(consume(FOR)){
		crtTk=startTk;
		if(ruleFor())
			return 1;
	}
	else if(consume(BREAK)){
		if(consume(SEMICOLON)){
			return 1;
		}
		else tkerr(crtTk,"missing ; after BREAK");
	}
	else if(consume(RETURN)){
		crtTk=startTk;
		if(ruleReturn())
			return 1;

	}
	else {
		crtTk=startTk;
		if(ruleExpr())
			return 1;
	
	}

	crtTk=startTk;
	return 0;
}

int stmCompound(){
	//printf("\n \tfunctia stmCompound ");
	Token *startTk=crtTk;
	Symbol *start=symbolsTab->end[-1];
	
	if(consume(LACC)){
		crtDepth++;
	
		while(1){
			if(declVar()){
			}
			else if(stm()){
			}
			else break;
		}
	
		if(consume(RACC)){
			crtDepth--;
			deleteSymbolsAfter(symbolsTab,start);
			return 1;
			
		}else tkerr(crtTk,"missing } or syntax error");
	
	}
	crtTk=startTk;
	return 0;
}


int funcArg(){
	//printf("\n \tfunctia funcArg ");
	Token *startTk=crtTk,*tkName;
	Type varType;

	if(typebase(&varType)){

		 if(consume(ID)){

			 tkName = consumedTk;

			if(!arrayDecl(&varType)){
				varType.nElements=-1;
			}

			Symbol *s = addSymbol(symbolsTab, tkName->info.text, CLS_VAR);
            s->mem = MEM_ARG;
            s->type = varType;

            s = addSymbol(&crtFunc->args, tkName->info.text, CLS_VAR);
             s->mem = MEM_ARG;
			 s->type = varType;
		
			return 1;
		}
		else{
			tkerr(crtTk,"missing param name ");
			crtTk=startTk;
			return 0;
		}
	}

	crtTk=startTk;
	return 0;
}

int declFunc(){
	//printf("\n \tfunctia declFunc");
	Type varType;
	Token *startTk=crtTk,*tkName;
	int ok=0;
	
	if(typebase(&varType)){//|consume(MUL))||consume(VOID)){
		if(consume(MUL)){
			varType.nElements=0;
		}
		else{
			varType.nElements=-1;
		}
		ok=1;
	}else if(consume(VOID)){
			varType.typeBase=TB_VOID;
			ok=1;
		}
		
		if((ok==1)&&consume(ID)){
			 tkName = consumedTk;
			if(consume(LPAR)){
				 if(findSymbol(symbolsTab,tkName->info.text))
					tkerr(crtTk,"symbol redefinition: %s",tkName->info.text);
				crtFunc=addSymbol(symbolsTab,tkName->info.text,CLS_FUNC);
				initSymbols(&crtFunc->args);
				crtFunc->type=varType;
				crtDepth++;
			
			
			if(funcArg()){
				while(1){
					if(consume(COMMA)){
						if(funcArg()){
							//return 1;
						}else tkerr(crtTk,"invalid declaration");
					}
					else
						break;
				}
			}
			if(consume(RPAR)){
				crtDepth--;
				if(stmCompound()){
					deleteSymbolsAfter(symbolsTab,crtFunc);
					crtFunc=NULL;
					return 1;
				}

			}//else tkerr(crtTk,"missing )");
		}
	}

	crtTk=startTk;
	return 0;
}

int declStruct(){
	//printf("\n \tfunctia declStruct ");
	Token *startTk=crtTk;
	 Token *tkName;

	if(consume(STRUCT)){
		if(consume(ID)){
			 tkName=consumedTk;
			if(consume(LACC)){
				if(findSymbol(symbolsTab,tkName->info.text))
            		tkerr(crtTk,"symbol redefinition: %s",tkName->info.text);
      	 		crtStruct=addSymbol(symbolsTab,tkName->info.text,CLS_STRUCT);
       			 initSymbols(&crtStruct->members);
				while(1){
					if(declVar()){
						
					}
					else
						break;
				}
				if(consume(RACC)){
					if(consume(SEMICOLON)){
						crtStruct=NULL;
						return 1;
					}else tkerr(crtTk,"missing ; in declStruct");
				}else tkerr(crtTk,"missing }");
			}
		}else tkerr(crtTk,"missing ID");
	}

	crtTk=startTk;
	return 0;
}

int unit(){
	//printf("\n \tfunctia unit ");
	Token *startTk;

	SAFEALLOC(startTk,Token);
	startTk=crtTk;

	while(1){
		if(declStruct()){
		}
		else if (declFunc()){
		}
		else if(declVar()){
		}
		else break;
	}
	if(consume(END)){
		return 1;
	}

	crtTk=startTk;
	return 0;
}

//Type getArithType(Type *s1,Type *s2);
