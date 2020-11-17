
# -----------------------------------------------------------------------------
# HolaQueTal.py
#
# Analizador de léxico y sintaxis para Tarea "HolaQueTal"
# Aurora Tijerina Berzosa - A01196690
# -----------------------------------------------------------------------------

import ply.lex as lex
import ply.yacc as yacc
import sys

reserved = {
    'read' : 'READ',
    'print' : 'PRINT',
    'if' : 'IF',
    'else' : 'ELSE',
    'while' : 'WHILE',
    'do' : 'DO',
    'for' : 'FOR',
    'func' : 'FUNC'
}

tokens = [
    'COMMA',
    'IS_VALUE',
    'OPEN_PARENTHESIS',
    'CLOSE_PARENTHESIS',
    'OPEN_BRACES',
    'CLOSE_BRACES',
    'ID',
    'VAL',
    'COLON',
    'SUMOP',
    'MULOP',
    'SUBOP',
    'DIVOP',
    'EQ',
    'NE',
    'LT',
    'GT',
    'LE',
    'GE',
    'OR',
    'AND'
] + list(reserved.values())

t_ignore = r' \n\t'
 # RegEx para tokens
# t_ENDL = r'\n'
t_COMMA = r'\,'
t_IS_VALUE = r'<-'
# t_READ = r'read'
t_OPEN_PARENTHESIS  = r'\('
t_CLOSE_PARENTHESIS  = r'\)'
# t_PRINT = r'print'
# t_IF = r'if'
t_OPEN_BRACES = r'\{'
t_CLOSE_BRACES = r'\}'
t_COLON = r'\:'
t_SUMOP = r'\+'
t_MULOP = r'\*'
t_SUBOP = r'\-'
t_DIVOP = r'\/'
t_EQ = r'\='
t_NE = r'!='
t_LT = r'\<'
t_GT = r'\>'
t_LE = r'<='
t_GE = r'>='
t_AND = r'&&'
t_OR = r'°°'

# t_ELSE = r'else'
# t_WHILE = r'while'
# t_DO = r'do'
# t_FOR = r'for'
# t_FUNC = r'func'
# t_ID = r'[a-zA-Z_][a-zA-Z0-9_]*'`
# t_VAL = r'([0-9]+)'

def t_IF(t):
    r'if'
    t.type = reserved.get(t.value, 'if')

    # print("HOLAAAAAAAAAAA")
    # global expression_stack
    # global exp_list
    # print(exp_list)
    # if(len(exp_list)>0):
    #     expression_stack.append(exp_list.copy())
    #     print(exp_list)
    #     print("------------------->appended in exp stack")
    #     exp_list.clear()
    return t

def t_VAL(t):
    r'([0-9]+)'
    t.type = reserved.get(t.value, 'VAL')
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'ID')
    return t
    
def t_error(t):
    print("Caracter inválido.")
    t.lexer.skip(1)

# def t_SEQUENCE(t):
# 	r'([0-9]+:[0-9]+)'
# 	t.type = reserved.get(t.value, 'SEQUENCE')
# 	return t

# digit            = r'([0-9])'
# nondigit         = r'([_A-Za-z])'
# identifier       = r'(' + nondigit + r'(' + digit + r'|' + nondigit + r')*)'  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Empty/Error
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

def p_empty(p):
    'empty :'
    pass

def p_error(p):
    print("\t", p.type, " ERROR")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Estructuras
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start = 'MAIN_PROGRAM'

def p_MAIN_PROGRAM(p):
    '''
    MAIN_PROGRAM : STRUCTURES
    '''
    print("\tCORRECTO MAIN")
    
def p_STRUCTURES(p):
    '''
    STRUCTURES  : FOR_LOOP STRUCTURES
                | DO_WHILE_LOOP STRUCTURES
                | WHILE_LOOP STRUCTURES
                | ELSE_CONDITIONAL STRUCTURES
                | IF_CONDITIONAL STRUCTURES
                | OUTPUT STRUCTURES
                | INPUT STRUCTURES
                | VAR STRUCTURES
                | FUNCTION STRUCTURES
                | empty
    '''
    print("\tCORRECTO STRUCTURES")

def p_VAR(p):
    '''
    VAR : ID COMMA VAR
        | ID IS_VALUE EXP
    '''
    global var_val
    global exp_list
    global var_name
    global curren_var
    global instruction_stack
    global expression_stack
    # for x in p:
    #     print("hey")
    #     if x:
    #         print("VAR: " + x)
    # global var_name
    # global curren_var
    curren_var = p[1]
    exp = expression_stack.pop()
    if not  p[1] in var_name:
        curren_var = p[1]
        var_name.append(curren_var)
        var_val.append(exp)
        instruction_stack.append(["=", exp, "", curren_var ])
        var_counter=+1
    else:
        var_val[var_name.index(curren_var)] = exp
        instruction_stack.append(["=", exp, "", curren_var ])
    print(exp_list)
    print("\tCORRECTO VAR")

def p_INPUT(p):
    #TODO agregar path expression
    '''
    INPUT 	: ID COMMA INPUT
            | ID IS_VALUE READ OPEN_PARENTHESIS CLOSE_PARENTHESIS
    '''
    print("\tCORRECTO INPUT")

def p_OUTPUT(p):
    #TODO agregar path expression
    '''
    OUTPUT : PRINT OPEN_PARENTHESIS ID CLOSE_PARENTHESIS
    '''
    print("\tCORRECTO OUTPUT")

def p_IF_CONDITIONAL(p):
    '''
    IF_CONDITIONAL  : IF OPEN_PARENTHESIS EXP CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES ELSE_CONDITIONAL
                    | IF OPEN_PARENTHESIS EXP CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES
    '''
    for x in p:
        if x:
            print("IF_cond: " + x)
    global exp_list
    global instruction_stack
    global temp_counter
    global expression_stack
    if len(exp_list)>0:
        expression_stack.append(exp_list.copy())
        exp_list.clear()
    instruction_stack.append(["=", expression_stack.pop(), "", temp_counter])
    temp_counter+=1
    instruction_stack.append(["==", 0, "", len(callback_stack)])
    callback_stack.append(len(callback_stack))

    # var_name_temporal.
    print("Finished IF: ", exp_list)
    print("\tCORRECTO IF")

def p_ELSE_CONDITIONAL(p):
    '''
    ELSE_CONDITIONAL : ELSE OPEN_BRACES STRUCTURES CLOSE_BRACES
    '''
    print("\tCORRECTO ELSE")

def p_WHILE_LOOP(p):
    '''
    WHILE_LOOP : WHILE OPEN_PARENTHESIS EXP CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES
    '''
    print("\tCORRECTO WHILE")

def p_DO_WHILE_LOOP(p):
    '''
    DO_WHILE_LOOP : DO OPEN_BRACES STRUCTURES CLOSE_BRACES WHILE OPEN_PARENTHESIS EXP CLOSE_PARENTHESIS
    '''
    print("\tCORRECTO DO WHILE")

def p_FOR_LOOP(p):
    '''
    FOR_LOOP : FOR OPEN_PARENTHESIS EXP COLON EXP CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES 
    '''
    print("\tCORRECTO FOR")

def p_FUNCTION(p):
    '''
    FUNCTION : FUNC ID OPEN_PARENTHESIS CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES 
    '''
    print("\tCORRECTO FUNC")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Expresiones
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
def p_EXP(p):
    '''
    EXP	: VALUE
    '''
    
    global expression_stack
    global exp_list
    expression_stack.append(exp_list.copy())
    exp_list.clear()
    print("\tCORRECTO EXP")


def p_VALUE(p):
    # TODO ad ELOGIC & EARITH
    # global var_value_list
    # global var_counter
    # var_value_list.append(p.value)
    # var_counter = var_counter + 1
    '''
    VALUE	: EARITH
            | ELOGIC
            | OPEN_PARENTHESIS VALUE CLOSE_PARENTHESIS
            | VAL
    '''
    global exp_list
    for x in p:
        if x:
            print("Value------------>: " + x)
            exp_list.append(x)
    print("\tCORRECTO VALUE")

def p_EARITH(p):
    '''
    EARITH 	: VALUE MULOP VALUE
            | VALUE DIVOP VALUE
            | VALUE SUMOP VALUE
            | VALUE SUBOP VALUE
    '''
    global exp_list
    for x in p:
        if x:
            # print("Value: " + x)
            exp_list.append(x)

    print("\tCORRECTO EARITH")

def p_ELOGIC(p):
    '''
    ELOGIC	: VALUE EQ VALUE
            | VALUE NE VALUE
            | VALUE LT VALUE
            | VALUE GT VALUE
            | VALUE LE VALUE
            | VALUE GE VALUE
            | VALUE AND VALUE
            | VALUE OR VALUE
    '''
    global exp_list
    for x in p:
        if x:
            # print("Value: " + x)
            exp_list.append(x)
    print("\tCORRECTO ELOGIC")

# def p_DIV_OP(p):
#     '''
#     DIV_OP	: VALUE DIVOP VALUE
#     '''
#     print("\tCORRECTO DIV_OP")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Main program
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

def parse_subops(exp_list):
    print("Lista inicial de expresiones: ", exp_list)
    x=0
    exp_list_copy = exp_list.copy()
    while x < len(exp_list_copy):
        if exp_list_copy[x] == "(":
            op_position = 1
            while(exp_list_copy[x-op_position] in ["*", "-", "+", "/"]):
                op_position+=1
            begining_exp = x-(op_position-1)*2-1
            subop = exp_list_copy[begining_exp:x]
            print(op_position, x)
            print("Subop: ", subop)
            simplification = operate(subop)
            exp_list_copy = exp_list_copy[:begining_exp] + [str(simplification)] + exp_list_copy[x+2:]
            print("Exp simplificada: ", exp_list_copy)
            x=begining_exp + 1
        x+=1
    return operate(exp_list_copy)

def operate(exp_list):
    length = len(exp_list)
    sim_list = []
    op=length-1
    num = 0
    while num < op:
        print("op: ", exp_list[op])
        print("num: ", exp_list[num])
        if exp_list[op] not in ["*", "/"]:
            sim_list.append(exp_list[num])
            sim_list.append(exp_list[op])
            op-=1
            num+=1
        else:
            num_accumul = float(exp_list[num])
            num+=1
            while exp_list[op] in ["*", "/", ">", ">=", "<", "<=", "=", "!="] and op > num:
                if exp_list[op] == "*":
                    print(num_accumul, " multiplied by ", float(exp_list[num]))
                    num_accumul*=float(exp_list[num])
                elif exp_list[op] == "/":
                    print(num_accumul, " divided by ", float(exp_list[num]))
                    num_accumul/=float(exp_list[num])
                op-=1
                num+=1
            sim_list.append(str(num_accumul))
            if op > num:
                sim_list.append(exp_list[op])
                op-=1
        print("Sim list: ", sim_list)
    if(num == op):
        sim_list.append(exp_list[num])
    print("Sim list: ", sim_list)
    x=1
    while x < len(sim_list):
        helper = 0
        if sim_list[x] == "+":
            print(sim_list[x-1], " adds ", float(sim_list[x+1]))
            helper = float(sim_list[x-1]) + float(sim_list[x+1])
            if x+2 < len(sim_list):
                sim_list = sim_list[:x-1] + [helper] + sim_list[x+2:]
            else:
                sim_list = [helper]
            x=1
        elif sim_list[x] == "-":
            print(sim_list[x-1], " substracted by ", float(sim_list[x+1]))
            helper = float(sim_list[x-1]) - float(sim_list[x+1])
            if x+2 < len(sim_list):
                sim_list = sim_list[:x-1] + [helper] + sim_list[x+2:]
            else:
                sim_list = [helper]
            x=1
        else:
            x+=2
        print("Current: ", sim_list)
    if(len(sim_list) == 1):
        return sim_list[0]

    ##############################################################################
    print("Lista antes de bool logic: ", sim_list)

    length = len(sim_list)
    pointer = 1
    while pointer < len(sim_list):
        while pointer < len(sim_list) and sim_list[pointer] in [">", ">=", "<", "<=", "=", "!="]:
            accumul = float(sim_list[pointer - 1])
            if sim_list[pointer] == ">":
                print(accumul, " bigger than ", float(sim_list[pointer+1]))
                accumul = accumul>float(sim_list[pointer+1])
            elif sim_list[pointer] == ">=":
                print(accumul, " bigger or equat to ", float(sim_list[pointer+1]))
                accumul = accumul>=float(sim_list[pointer+1])
            elif sim_list[pointer] == "<":
                print(accumul, " smaller than ", float(sim_list[pointer+1]))
                accumul = accumul<float(sim_list[pointer+1])
            elif sim_list[pointer] == "<=":
                print(accumul, " smaller or equal to ", float(sim_list[pointer+1]))
                accumul = accumul<=float(sim_list[pointer+1])
            elif sim_list[pointer] == "=":
                print(accumul, " equal to ", float(sim_list[pointer+1]))
                accumul = accumul==float(sim_list[pointer+1])
            elif sim_list[pointer] == "=":
                print(accumul, " equal to ", float(sim_list[pointer+1]))
                accumul = accumul==float(sim_list[pointer+1])
            elif sim_list[pointer] == "!=":
                print(accumul, " not equal to ", float(sim_list[pointer+1]))
                accumul = accumul != float(sim_list[pointer+1])
            sim_list = sim_list[:pointer-1] + [accumul] + sim_list[pointer+2:]
            pointer+=2
            print("Sim list: ", sim_list)
        else:
            pointer+=2

    length = len(sim_list)
    pointer = 1
    while pointer < len(sim_list):
            accumul = sim_list[pointer - 1]
            if sim_list[pointer] == "&&":
                print(accumul, " AND ", sim_list[pointer+1])
                accumul = accumul and sim_list[pointer+1]
            elif sim_list[pointer] == "°°":
                print(accumul, " OR ", sim_list[pointer+1])
                accumul = accumul or sim_list[pointer+1]
            sim_list = sim_list[:pointer-1] + [accumul] + sim_list[pointer+2:]
            pointer=1
            print("Sim list: ", sim_list, pointer)
    print("Result", sim_list[0])
    return sim_list[0]

def programStatus():
    global exp_list
    global var_name
    global var_val
    global instruction_stack
    global expression_stack

    print("Lista de expresiones: ", exp_list)
    print("Nombre de variables: ", var_name)
    print("Valor de variables: ", var_val)
    print("Instruction stack:")
    for instruction in instruction_stack:
        print(instruction)
    print("Expression stack:")
    for exp in expression_stack:
        print(exp)


def reset():
    global exp_list
    global var_name
    global var_val
    global instruction_stack
    global expression_stack

    exp_list.clear()
    var_name.clear()
    var_val.clear()
    instruction_stack.clear()
    expression_stack.clear()    

#######################################################################




#######################################################################

#lexer = lex.lex()
lexer = lex.lex(debug=1)
parser = yacc.yacc(debug=True)
var_val = []
var_name = []
exp_list = []
exp_op = []
var_counter = 0
curren_var = ""

var_val_temporal = []
temp_counter=0


#######################################################################
########### Quadruplos de instrucciones
#######################################################################

instruction_stack = []
temp_counter=0
callback_stack=[]
expression_stack = []


#######################################################################

while True:
    try:
        print("Write the code instruction here:")
        s = input()
        if(s == 'x'):
            break
    except EOFError:
        break
    lexer.input(s)
    for tok in lexer:
        print(tok)
    parser.parse(s)
    programStatus()
    # print("Respuesta: ", parse_subops(exp_list))
    # print(exp_op)
    # exp_op.clear()
    reset()
