
/* Please feel free to modify any content */

/* Definition section */
%{
    #include "compiler_common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;

    typedef struct symbol_t {
        int index;
        char *name;
        int mut;
        char* type;
        int addr;
        int lineno;
        char *func_sig;
    } Symbol;

    Symbol *symbol_table[1000][1000];    // max 1000 scopes, max 1000 symbols per scope.
    int symbol_count[1000] = {0};        // number of symbols in each scope
    int cur_scope = -1;               // current scope level
    int cur_addr = -1;                // current address for symbol allocation


    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    int yylex_destroy ();

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    /* Used to generate code */
    /* As printf; the usage: CODEGEN("%d - %s\n", 100, "Hello world"); */
    /* We do not enforce the use of this macro */
    #define CODEGEN(...) \
        do { \
            for (int i = 0; i < g_indent_cnt; i++) { \
                fprintf(fout, "\t"); \
            } \
            fprintf(fout, __VA_ARGS__); \
        } while (0)

    /* Symbol table function - you can add new functions if needed. */
    /* parameters and return type can be changed */
    static void create_symbol();
    static void insert_symbol(const char *name, int mul, const char *type, const char *func_sig);
    static Symbol* lookup_symbol(const char *name);
    static void dump_symbol(int stop_scope);
    static void check_mismatch(const char* lhs, const char* opt, const char* rhs);

    /* Global variables */
    bool g_has_error = false;
    FILE *fout = NULL;
    int g_indent_cnt = 0;
    int g_lbl_counter = 0;    // avoid if-else label conflict

    bool HAS_ERROR = false;
    void yyerror (char const *s)
    {
        HAS_ERROR = true;
        printf("error:%d: %s\n", yylineno, s);
    }
%}

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 *  - you can add new fields if needed.
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    struct symbol_t *sym;
    struct {
        struct symbol_t *sym;
        char *name;
    } lval;
    struct {
        char *opname;
        char *optype;
    } opval;
    /* ... */
}

/* Token without return */
%token LET MUT NEWLINE
%token INT FLOAT BOOL STR
%token TRUE FALSE
%token GEQ LEQ EQL NEQ LOR LAND
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN REM_ASSIGN
%token IF ELSE FOR WHILE LOOP
%token PRINT PRINTLN
%token FUNC RETURN BREAK
%token ID ARROW AS IN DOTDOT RSHIFT LSHIFT

/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT
%token <s_val> IDENT

/* Operator Precedence (from lowest to highest) */
%right '=' ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN REM_ASSIGN
%left LOR
%left LAND
%left '>' '<' GEQ LEQ EQL NEQ
%left LSHIFT RSHIFT
%left '+' '-'
%left '*' '/' '%'
%right UMINUS NOT

/* Nonterminal with return, which need to sepcify type */
%type <s_val> OptionalTypeDecl
%type <s_val> OptionalInit
%type <s_val> Type
%type <s_val> TypeDecl
%type <s_val> Expr
%type <s_val> ExprList
%type <lval> AssExpr

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : { create_symbol(); } GlobalStatementList
;

GlobalStatementList 
    : GlobalStatementList GlobalStatement
    | GlobalStatement
;

GlobalStatement
    : FunctionDeclStmt
    | NEWLINE
;

FunctionDeclStmt
    : FUNC ID { g_indent_cnt++; printf("func: %s\n", $<s_val>2); insert_symbol($<s_val>2, -1, "func", "(V)V"); } '(' ')' '{' { create_symbol(); } StatementList '}' { g_indent_cnt--; dump_symbol(0);}
;

StatementList
    : StatementList Statement
    | Statement
;

Statement
    : LET TypeDecl
    | FuncCall
    | '{' {create_symbol();} StatementList '}' { dump_symbol(cur_scope); }
    | ExprList ';'
    | IF Expr '{' { create_symbol(); } StatementList '}' { dump_symbol(cur_scope); } ElseStatement
    | WHILE Expr '{' { create_symbol(); } StatementList '}' { dump_symbol(cur_scope); }
;

ElseStatement
    : ELSE '{' { create_symbol(); } StatementList '}' { dump_symbol(cur_scope); }
    | {;}
;

TypeDecl
    : ID OptionalTypeDecl OptionalInit ';' { insert_symbol($<s_val>1, 0, $<s_val>2 ? $<s_val>2 : $<s_val>3, "-"); }
    | MUT ID OptionalTypeDecl OptionalInit ';' { insert_symbol($<s_val>2, 1, $<s_val>3 ? $<s_val>3 : $<s_val>4, "-"); }
;

OptionalTypeDecl
    : ':' Type  { $$ = $<s_val>2; }
    | ':' '&' Type  { $$ = $<s_val>3; }
    | ':' '[' Type ';' Expr ']' { $$ = "array"; } 
    | {$$ = NULL;}
;

OptionalInit
    : '=' Expr { 
        if (strcmp($<s_val>2, "i32") == 0) {
            CODEGEN("istore %d\n", cur_addr);
        } else if (strcmp($<s_val>2, "f32") == 0) {
            CODEGEN("fstore %d\n", cur_addr);
        } else if (strcmp($<s_val>2, "str") == 0) {
            CODEGEN("astore %d\n", cur_addr);
        } else if (strcmp($<s_val>2, "bool") == 0) {
            CODEGEN("istore %d\n", cur_addr);
        }
        $$ = $<s_val>2; 
    }
    | {$$ = NULL;}
;

Type
    : INT { $$ = "i32"; }
    | FLOAT { $$ = "f32"; }
    | BOOL { $$ = "bool"; }
    | STR { $$ = "str"; }
;

FuncCall
    : PRINT { CODEGEN("getstatic java/lang/System/out Ljava/io/PrintStream;\n"); } '(' ExprList ')' ';' { 
        printf("PRINT %s\n", $<s_val>4); 
        if (strcmp($<s_val>4, "str") == 0) {
            CODEGEN("invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
        } else if (strcmp($<s_val>4, "i32") == 0) {
            CODEGEN("invokevirtual java/io/PrintStream/print(I)V\n");
        } else if (strcmp($<s_val>4, "f32") == 0) {
            CODEGEN("invokevirtual java/io/PrintStream/print(F)V\n");
        } else if (strcmp($<s_val>4, "bool") == 0) {
            CODEGEN("invokevirtual java/io/PrintStream/print(Z)V\n");
        } 
    } 
    | PRINTLN { CODEGEN("getstatic java/lang/System/out Ljava/io/PrintStream;\n"); } '(' ExprList ')' ';' {
        printf("PRINTLN %s\n", $<s_val>4); 
        if (strcmp($<s_val>4, "str") == 0) {
            CODEGEN("invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
        } else if (strcmp($<s_val>4, "i32") == 0) {
            CODEGEN("invokevirtual java/io/PrintStream/println(I)V\n");
        } else if (strcmp($<s_val>4, "f32") == 0) {
            CODEGEN("invokevirtual java/io/PrintStream/println(F)V\n");
        } else if (strcmp($<s_val>4, "bool") == 0) {
            CODEGEN("invokevirtual java/io/PrintStream/println(Z)V\n");
        } 
    }
;

ExprList
    : Expr ',' ExprList
    | Expr
;

Expr
    : Expr LOR Expr { CODEGEN("ior\n"); printf("%s\n", "LOR"); $$ = "bool";}
    | Expr LAND Expr {CODEGEN("iand\n"); printf("%s\n", "LAND"); $$ = "bool";}
    | Expr '>' Expr {
        check_mismatch($<s_val>1, "GTR", $<s_val>3); 
        if (strcmp($<s_val>1, "i32") == 0 && strcmp($<s_val>3, "i32") == 0) {
            CODEGEN("if_icmpgt Comp_true_%d\n", g_lbl_counter);
            CODEGEN("iconst_0\n");     // if false push 0 (iconst_0 == ldc 0)
            CODEGEN("goto Comp_end_%d\n", g_lbl_counter);
        } else if (strcmp($<s_val>1, "f32") == 0 && strcmp($<s_val>3, "f32") == 0) {
            CODEGEN("fcmpg\n");
            CODEGEN("ifgt Comp_true_%d\n", g_lbl_counter);
            CODEGEN("iconst_0\n");     // if false push 0 (iconst_0 == ldc 0)
            CODEGEN("goto Comp_end_%d\n", g_lbl_counter);
        }
        CODEGEN("Comp_true_%d:\n", g_lbl_counter);
        CODEGEN("iconst_1\n");     // if true push 1 (iconst_1 == ldc 1)
        CODEGEN("Comp_end_%d:\n", g_lbl_counter);
        g_lbl_counter++;
        // The above code is for comparison, if true then push 1, else push 0
        printf("%s\n", "GTR"); 
        $$ = "bool"; 
    }
    | Expr '<' Expr {check_mismatch($<s_val>1, "LSS", $<s_val>3); printf("%s\n", "LSS"); $$ = "bool"; }
    | Expr GEQ Expr {check_mismatch($<s_val>1, "GEQ", $<s_val>3); printf("%s\n", "GEQ"); $$ = "bool"; }
    | Expr LEQ Expr {check_mismatch($<s_val>1, "LEQ", $<s_val>3); printf("%s\n", "LEQ"); $$ = "bool"; }
    | Expr EQL Expr {check_mismatch($<s_val>1, "EQL", $<s_val>3); printf("%s\n", "EQL"); $$ = "bool"; }
    | Expr NEQ Expr {check_mismatch($<s_val>1, "NEQ", $<s_val>3); printf("%s\n", "NEQ"); $$ = "bool"; }
    | Expr LSHIFT Expr {check_mismatch($<s_val>1, "LSHIFT", $<s_val>3); printf("%s\n", "LSHIFT"); $$ = "bool"; }
    | Expr RSHIFT Expr {check_mismatch($<s_val>1, "RSHIFT", $<s_val>3); printf("%s\n", "RSHIFT"); $$ = "bool"; }
    | Expr '+' Expr { 
        if (strcmp($<s_val>1, "i32") == 0 && strcmp($<s_val>3, "i32") == 0) {
            CODEGEN("iadd\n"); 
        }
        else if (strcmp($<s_val>1, "f32") == 0 && strcmp($<s_val>3, "f32") == 0) {
            CODEGEN("fadd\n");
        } 
        printf("%s\n", "ADD"); 
        $$ = $<s_val>1; 
      }
    | Expr '-' Expr { 
        if (strcmp($<s_val>1, "i32") == 0 && strcmp($<s_val>3, "i32") == 0) {
            CODEGEN("isub\n");
        }
        else if (strcmp($<s_val>1, "f32") == 0 && strcmp($<s_val>3, "f32") == 0) {
            CODEGEN("fsub\n");
        }
        printf("%s\n", "SUB"); 
        $$ = $<s_val>1; 
      }
    | Expr '*' Expr { 
        if (strcmp($<s_val>1, "i32") == 0 && strcmp($<s_val>3, "i32") == 0) {
            CODEGEN("imul\n");
        }
        else if (strcmp($<s_val>1, "f32") == 0 && strcmp($<s_val>3, "f32") == 0) {
            CODEGEN("fmul\n");
        }
        printf("%s\n", "MUL"); 
        $$ = $<s_val>1; 
      }
    | Expr '/' Expr { 
        if (strcmp($<s_val>1, "i32") == 0 && strcmp($<s_val>3, "i32") == 0) {
            CODEGEN("idiv\n");
        }
        else if (strcmp($<s_val>1, "f32") == 0 && strcmp($<s_val>3, "f32") == 0) {
            CODEGEN("fdiv\n");
        }
        printf("%s\n", "DIV"); 
        $$ = $<s_val>1; 
      }
    | Expr '%' Expr { CODEGEN("irem\n"); printf("%s\n", "REM"); $$ = $<s_val>1; }
    | '(' Expr ')' { $$ = $<s_val>2; }
    | '-' Expr %prec UMINUS { 
        if (strcmp($<s_val>2, "i32") == 0) {
            CODEGEN("ineg\n");
        } else if (strcmp($<s_val>2, "f32") == 0) {
            CODEGEN("fneg\n");
        } 
        printf("NEG\n"); 
        $$ = $<s_val>2; 
      }
    | '!' Expr %prec NOT   { 
        // Not : use 1 - x ==> !x = 1 - x
        CODEGEN("ldc 1\n");
        CODEGEN("swap\n");
        CODEGEN("isub\n");
        printf("NOT\n"); 
        $$ = "bool"; 
      }
    | AssExpr '=' Expr { 
        if ($1.sym == NULL) {
            printf("error:%d: undefined: %s\n", yylineno + 1, $1.name);
        } else if ($1.sym->mut == 0) {
            printf("ASSIGN\n");
            printf("error:%d: cannot borrow immutable borrowed content `%s` as mutable\n", yylineno + 1, $1.name);
            HAS_ERROR = true;
        }
        else {
            printf("ASSIGN\n");
        }
        $$ = $<s_val>3;
      }
    | AssExpr ADD_ASSIGN Expr { printf("ADD_ASSIGN\n"); $$ = $<s_val>1; }
    | AssExpr SUB_ASSIGN Expr { printf("SUB_ASSIGN\n"); $$ = $<s_val>1; }
    | AssExpr MUL_ASSIGN Expr { printf("MUL_ASSIGN\n"); $$ = $<s_val>1; }
    | AssExpr DIV_ASSIGN Expr { printf("DIV_ASSIGN\n"); $$ = $<s_val>1; }
    | AssExpr REM_ASSIGN Expr { printf("REM_ASSIGN\n"); $$ = $<s_val>1; }
    | Expr AS Type { printf("%c2%c\n", ($<s_val>1)[0], ($<s_val>3)[0]);  }
    | INT_LIT { CODEGEN("ldc %d\n", $<i_val>1); printf("INT_LIT %d\n", $<i_val>1); $$ = "i32"; }
    | FLOAT_LIT { CODEGEN("ldc %f\n", $<f_val>1); printf("FLOAT_LIT %f\n", $<f_val>1); $$ = "f32"; }
    | '"' STRING_LIT '"' { CODEGEN("ldc \"%s\"\n", $<s_val>2); printf("STRING_LIT \"%s\"\n", $<s_val>2); $$ = "str"; }
    | '"' '"' { CODEGEN("ldc \"\"\n"); printf("STRING_LIT \"\"\n"); $$ = "str"; }
    | TRUE { CODEGEN("ldc 1\n"); printf("bool TRUE\n"); $$ = "bool"; }
    | FALSE { CODEGEN("ldc 0\n"); printf("bool FALSE\n"); $$ = "bool"; }
    | '[' ExprList ']' { $$ = "array"; }
    | Expr '[' ExprList ']' { $$ = "array"; }
    | ID { 
        Symbol* sym = lookup_symbol($<s_val>1);
        if (!sym) {
            printf("error:%d: undefined: %s\n", yylineno + 1, $<s_val>1);
            HAS_ERROR = true;
            $$ = "undefined";
        } else {
            printf("IDENT (name=%s, address=%d)\n", $<s_val>1, sym->addr);
            if (strcmp(sym->type, "i32") == 0) {
                CODEGEN("iload %d\n", sym->addr);
            } else if (strcmp(sym->type, "f32") == 0) {
                CODEGEN("fload %d\n", sym->addr);
            } else if (strcmp(sym->type, "str") == 0) {
                CODEGEN("aload %d\n", sym->addr);
            } else if (strcmp(sym->type, "bool") == 0) {
                CODEGEN("iload %d\n", sym->addr);
            }
            $$ = sym->type;
        }
     }
;

AssExpr
    : ID {
        Symbol *sym = lookup_symbol($<s_val>1);
        $$.sym = sym;
        $$.name = $<s_val>1;
        if (!sym) {
            HAS_ERROR = true;
        }
    }
%%

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }
    if (!yyin) {
        printf("file `%s` doesn't exists or cannot be opened\n", argv[1]);
        exit(1);
    }

    /* Codegen output init */
    char *bytecode_filename = "hw3.j";
    fout = fopen(bytecode_filename, "w");
    CODEGEN(".source hw3.j\n");
    CODEGEN(".class public Main\n");
    CODEGEN(".super java/lang/Object\n");
    CODEGEN(".method public static main([Ljava/lang/String;)V\n");
    CODEGEN(".limit stack 100\n");
    CODEGEN(".limit locals 100\n");

    /* Symbol table init */
    // Add your code

    yylineno = 0;
    yyparse();

    /* Symbol table dump */
    // Add your code
    CODEGEN("return\n");
    CODEGEN(".end method\n");
	printf("Total lines: %d\n", yylineno);
    fclose(fout);
    fclose(yyin);

    if (g_has_error) {
        remove(bytecode_filename);
    }
    yylex_destroy();
    return 0;
}

static void create_symbol() {
    cur_scope++;
    symbol_count[cur_scope] = 0;
    Symbol *new_symbol;
    for (int i = 0; i < 1000; i++) {
        new_symbol = (Symbol *)malloc(sizeof(Symbol));
        symbol_table[cur_scope][i] = new_symbol;
    }
    printf("> Create symbol table (scope level %d)\n", cur_scope);
}

static void insert_symbol(const char *name, int mul, const char *type, const char *func_sig) {
    // Set address and index
    printf("> Insert `%s` (addr: %d) to scope level %d\n", name, cur_addr, cur_scope);
    symbol_table[cur_scope][symbol_count[cur_scope]]->index = symbol_count[cur_scope];
    symbol_table[cur_scope][symbol_count[cur_scope]]->name = strdup(name);
    symbol_table[cur_scope][symbol_count[cur_scope]]->mut = mul;
    symbol_table[cur_scope][symbol_count[cur_scope]]->type = strdup(type);
    symbol_table[cur_scope][symbol_count[cur_scope]]->addr = cur_addr;
    symbol_table[cur_scope][symbol_count[cur_scope]]->lineno = yylineno + 1;    // because yylineno has not increased yet
    symbol_table[cur_scope][symbol_count[cur_scope]]->func_sig = strdup(func_sig);
    symbol_count[cur_scope]++;
    cur_addr++;
}

static Symbol* lookup_symbol(const char *name) {
    for (int i = cur_scope; i >= 0; i--) {
        for (int j = 0; j < symbol_count[i]; j++) {
            if (strcmp(symbol_table[i][j]->name, name) == 0) {
                return symbol_table[i][j];
            }
        }
    }
    return NULL;
}

static void dump_symbol(int stop_scope) {
    while (cur_scope >= stop_scope) {
        printf("\n> Dump symbol table (scope level: %d)\n", cur_scope);
        printf("%-10s%-10s%-10s%-10s%-10s%-10s%-10s\n",
               "Index", "Name", "Mut","Type", "Addr", "Lineno", "Func_sig");
               
        for (int i = 0; i < symbol_count[cur_scope]; i++) {
            printf("%-10d%-10s%-10d%-10s%-10d%-10d%-10s\n",
                   i, 
                   symbol_table[cur_scope][i]->name,
                   symbol_table[cur_scope][i]->mut,
                   symbol_table[cur_scope][i]->type,
                   symbol_table[cur_scope][i]->addr,
                   symbol_table[cur_scope][i]->lineno,
                   symbol_table[cur_scope][i]->func_sig);
        }
        cur_scope--;
    }
}

static void check_mismatch(const char* lhs, const char* opt, const char* rhs) {
    if (strcmp(lhs, rhs) != 0 ) {
        printf("error:%d: invalid operation: %s (mismatched types %s and %s)\n", yylineno + 1, opt, lhs, rhs);
        HAS_ERROR = true;
    }
}