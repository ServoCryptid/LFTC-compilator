
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifndef SINTACTIC_H_
#define SINTACTIC_H_

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
int consume(int code);
int unit();
void init();
void showSymbols();

#endif