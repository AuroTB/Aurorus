
# -----------------------------------------------------------------------------
# HolaQueTal.py
#
# Analizador de léxico y sintaxis para Tarea "HolaQueTal"
# Aurora Tijerina Berzosa - A01196690
# -----------------------------------------------------------------------------

import ply.lex as lex
import ply.yacc as yacc
from executor import Executor
import sys

reserved = {
    'read' : 'READ',
    'print' : 'PRINT',
    'if' : 'IF',
    'else' : 'ELSE',
    'while' : 'WHILE',
    'do' : 'DO',
    'for' : 'FOR',
    'func' : 'FUNC',
    '<-' : 'IS_VALUE'
}

tokens = [
    'COMMA',
    'OPEN_PARENTHESIS',
    'CLOSE_PARENTHESIS',
    'OPEN_BRACES',
    'CLOSE_BRACES',
    'ID',
    'VAL',
    'MATRIX',
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
    'AND',
    'SEQUENCE',
    'MATRIX_VAL',
    'STRING'
] + list(reserved.values())

t_ignore = r' \n'
 # RegEx para tokens
# t_ENDL = r'\n'
# # t_READ = r'read'
# t_OPEN_PARENTHESIS  = r'\('
# t_CLOSE_PARENTHESIS  = r'\)'
# # t_PRINT = r'print'
# # t_IF = r'if'

# t_OPEN_BRACES = r'\{'
t_OPEN_PARENTHESIS  = r'\('
t_CLOSE_PARENTHESIS  = r'\)'
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
t_COMMA = r'\,'

# t_IS_VALUE = r'<-'

# t_ELSE = r'else'
# t_WHILE = r'while'
# t_DO = r'do'
# t_FOR = r'for'
# t_FUNC = r'func'
# t_ID = r'[a-zA-Z_][a-zA-Z0-9_]*'`
# t_VAL = r'([0-9]+)'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Logic of structures
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

def t_READ(t):
    r'read'
    t.type = reserved.get(t.value, 'READ')

    global instruction_stack
    global var_stack_helper
    index = len(instruction_stack)-1
    print("READ instruction stack")
    print(instruction_stack)
    while(instruction_stack[index][1] != "0"):
        index-=1
    instruction_stack.insert(index, ["read", "", "", len(temp_var_stack)])
    index+=1
    while(index < len(instruction_stack)):
        instruction_stack[index][1] = len(temp_var_stack)
        index+=1
    temp_var_stack.append(0)
    var_stack_helper.clear()
    print("READ code")
    return t

def t_PRINT(t):
    r'print'
    t.type = reserved.get(t.value, 'PRINT')

    global instruction_stack

    instruction_stack.append(["print", "", "", ""])
    global var_stack_helper
    var_stack_helper.clear()
    print("PRINT code")
    return t

def t_FUNC(t):
    r'func'
    t.type = reserved.get(t.value, 'FUNC')

    global func_names
    global instruction_stack
    global jump_stack
    global jumps

    instruction_stack.append(["go", "", "", len(jumps)])
    func_jumps.append(len(instruction_stack))
    jump_stack.append(len(jumps))
    jumps.append(0)
    struc_stack.append([2,0])
    global var_stack_helper
    var_stack_helper.clear()
    print(func_jumps)
    print("Inicio de FUNC")
    return t

def t_DO(t):
    r'do'
    t.type = reserved.get(t.value, 'DO')

    global struc_stack
    struc_stack.append([3,len(instruction_stack)])
    global var_stack_helper
    var_stack_helper.clear()
    print("Inicio de DO")
    return t

def t_WHILE(t):
    r'while'
    t.type = reserved.get(t.value, 'WHILE')

    if(struc_stack[-1][0] == 1):
        global instruction_stack
        global jump_stack
        global temp_var_stack
        global jumps
        struc_stack.append([1,len(instruction_stack)])
        instruction_stack.append(["=", "", "", len(temp_var_stack)])
        instruction_stack.append(["==", False, len(temp_var_stack), len(jumps)])
        temp_var_stack.append(0)
        jump_stack.append(len(jumps))
        jumps.append(0)
        print("Inicio de WHILE")
        global var_stack_helper
        var_stack_helper.clear()
    return t

def t_FOR(t):
    r'for'
    t.type = reserved.get(t.value, 'FOR')
    global var_stack_helper
    var_stack_helper.clear()
    print("Inicio de FOR")
    return t

def t_IF(t):
    r'if'
    t.type = reserved.get(t.value, 'IF')

    global instruction_stack
    global jump_stack
    global temp_var_stack
    global jumps
    global struc_stack
    instruction_stack.append(["=", "", "", len(temp_var_stack)])
    instruction_stack.append(["==", False, len(temp_var_stack), len(jumps)])
    temp_var_stack.append(0)
    jump_stack.append(len(jumps))
    jumps.append(0)
    struc_stack.append([0,0])
    global var_stack_helper
    var_stack_helper.clear()
    print("Inicio de IF")
    return t

def t_ELSE(t):
    r'else'
    t.type = reserved.get(t.value, 'ELSE')

    global instruction_stack
    global jump_stack
    global temp_var_stack
    global jumps
    global struc_stack
    instruction_stack.append(["go", "", "", len(jumps)])
    jumps[-1]+=1
    temp_var_stack.append(0)
    jump_stack.append(len(jumps))
    jumps.append(0)
    struc_stack.append([0,0])
    global var_stack_helper
    var_stack_helper.clear()
    print("Inicio de ELSE")
    return t

def t_CLOSE_BRACES(t):
    r'\}'
    t.type = reserved.get(t.value, 'CLOSE_BRACES')

    global instruction_stack
    global jump_stack
    global jumps
    global struc_stack
    print(struc_stack)
    print(jump_stack)
    print(jumps)
    struc = struc_stack.pop()
    if(struc[0] == 1):
        instruction_stack.append(["go", "", "", len(jumps)])
        jumps.append(struc[1])
        jumps[jump_stack.pop()] = len(instruction_stack)
    elif(struc[0] == 3):
        instruction_stack.append(["=", "", "", len(temp_var_stack)])
        instruction_stack.append(["==", True, len(temp_var_stack), len(jumps)])
        temp_var_stack.append(0)
        jumps.append(struc[1])
    else:
        instruction_stack.append(["return", "", "", ""])
        jumps[jump_stack.pop()] = len(instruction_stack)
    global var_stack_helper
    var_stack_helper.clear()
    print("Final de estrucutura")
    print(struc_stack)
    print(jump_stack)
    print(jumps)
    return t

def t_OPEN_BRACES(t):
    r'\{'
    t.type = reserved.get(t.value, 'OPEN_BRACES')

    global var_stack_helper
    var_stack_helper.clear()
    return t

def t_SEQUENCE(t):
    r'([0-9]+:[0-9]+)'
    t.type = reserved.get(t.value, 'SEQUENCE')

    global instruction_stack
    global jump_stack
    global temp_var_stack
    global jumps
    instruction_stack.append(["=", t.value, "", len(temp_var_stack)])
    struc_stack.append([1, len(instruction_stack)])
    instruction_stack.append(["==", False, len(temp_var_stack), len(jumps)])
    instruction_stack.append(["-", len(temp_var_stack), 1, len(temp_var_stack)])
    temp_var_stack.append(0)
    jump_stack.append(len(jumps))
    jumps.append(0)
    global var_stack_helper
    var_stack_helper.clear()
    print("Inicio de FOR")
    return t

def t_IS_VALUE(t):
    r'<-'
    t.type = reserved.get(t.value, 'IS_VALUE')

    global var_stack_helper
    global instruction_stack
    print(var_stack_helper)
    for index, var in enumerate(var_stack_helper):
        instruction_stack.append(["=", str(index), "", var])
    var_stack_helper.clear()
    return t
    print("Assigned to")

def t_MATRIX_VAL(t):
    r'([a-zA-Z_][a-zA-Z0-9_]*(\[[0-9]+])+)'
    t.type = reserved.get(t.value, 'MATRIX_VAL')
    print("matrix VAL")
    return t

def t_MATRIX(t):
    r'(\[[0-9]+([:][0-9]+)+])'
    t.type = reserved.get(t.value, 'MATRIX')

    # global var_stack_helper
    # var_stack_helper.append(t.value)
    print("iniciación matrix")
    return t

def t_STRING(t):
    r'"[a-zA-Z0-9[\], +*/~?¿-]+"'
    t.type = reserved.get(t.value, 'STRING')
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'ID')

    global var_stack_helper
    global struc_stack
    global func_dic
    global func_jumps
    var_stack_helper.append(t.value)
    if len(struc_stack)>1:
        if len(struc_stack) and struc_stack[1][0] == 2 and not var_stack_helper[0] in func_dic:
            func_dic[var_stack_helper[0]] = func_jumps.pop()
            var_stack_helper.clear()
        elif var_stack_helper[0] in func_dic:
                instruction_stack.append(["func_go", "", "", func_dic[var_stack_helper[0]]])
                var_stack_helper.clear()
    return t

def t_VAL(t):
    r'([0-9]+)'
    t.type = reserved.get(t.value, 'VAL')
    return t
    
def t_error(t):
    print("Caracter inválido.")
    t.lexer.skip(1)

# digit            = r'([0-9])'
# nondigit         = r'([_A-Za-z])'
# identifier       = r'(' + nondigit + r'(' + digit + r'|' + nondigit + r')*)'

# t_IS_VALUE = r'<-'

# def t_LE(t):
#     r'<='
#     t.type = reserved.get(t.value, 'LE')
#     return t

# def t_COMMA(t):
#     r'\,'
#     t.type = reserved.get(t.value, 'COMMA')
#     return t

# def t_CLOSE_PARENTHESIS(t):
#     r'\)'
#     t.type = reserved.get(t.value, 'CLOSE_PARENTHESIS')

#     lexer
#     return t

# def t_SUMOP(t):
#     r'\+'
#     t.type = reserved.get(t.value, 'SUMOP')
#     return t

# def t_MULOP(t):
#     r'\*'
#     t.type = reserved.get(t.value, 'MULOP')
#     return t

# def t_SUBOP(t):
#     r'\-'
#     t.type = reserved.get(t.value, 'SUBOP')
#     return t

# def t_DIVOP(t):
#     r'\/'
#     t.type = reserved.get(t.value, 'DIVOP')
#     return t

# def t_EQ(t):
#     r'\='
#     t.type = reserved.get(t.value, 'EQ')
#     return t

# def t_NE(t):
#     r'!='
#     t.type = reserved.get(t.value, 'NE')
#     return t

# def t_LT(t):
#     r'\<'
#     t.type = reserved.get(t.value, 'LT')
#     return t

# def t_GT(t):
#     r'\>'
#     t.type = reserved.get(t.value, 'GT')
#     return t

# def t_GE(t):
#     r'>='
#     t.type = reserved.get(t.value, 'GE')
#     return t

# def t_AND(t):
#     r'&&'
#     t.type = reserved.get(t.value, 'AND')
#     return t

# def t_OR(t):
#     r'°°'
#     t.type = reserved.get(t.value, 'OR')
#     return t

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
                | FUNC_CALL STRUCTURES
                | empty
    '''
    print("\tCORRECTO STRUCTURES")

def p_VAR(p):
    '''
    VAR : ID COMMA VAR
        | ID IS_VALUE EXP
        | ID IS_VALUE MATRIX
    '''
    for x in p:
        print("hey")
        if x and '[' in x:
            print("VAR: " + x)
            global expression_stack
            expression_stack.append([x])
    print("\tCORRECTO VAR")

def p_INPUT(p):
    '''
    INPUT 	: ID COMMA INPUT
            | ID IS_VALUE READ OPEN_PARENTHESIS CLOSE_PARENTHESIS
    '''
    print("\tCORRECTO INPUT")

def p_OUTPUT(p):
    '''
    OUTPUT  : PRINT OPEN_PARENTHESIS EXP CLOSE_PARENTHESIS
    '''
    print("\tCORRECTO OUTPUT")

def p_IF_CONDITIONAL(p):
    '''
    IF_CONDITIONAL  : IF OPEN_PARENTHESIS EXP CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES ELSE_CONDITIONAL
                    | IF OPEN_PARENTHESIS EXP CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES
    '''
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
    FOR_LOOP : FOR OPEN_PARENTHESIS SEQUENCE CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES 
    '''
    print("\tCORRECTO FOR")

def p_FUNCTION(p):
    '''
    FUNCTION : FUNC ID OPEN_PARENTHESIS CLOSE_PARENTHESIS OPEN_BRACES NON_FUNCTION CLOSE_BRACES 
    '''
    print("\tCORRECTO FUNC")

def p_NON_FUNCTION(p):
    '''
    NON_FUNCTION    : FOR_LOOP NON_FUNCTION
                    | DO_WHILE_LOOP NON_FUNCTION
                    | WHILE_LOOP NON_FUNCTION
                    | ELSE_CONDITIONAL NON_FUNCTION
                    | IF_CONDITIONAL NON_FUNCTION
                    | OUTPUT NON_FUNCTION
                    | INPUT NON_FUNCTION
                    | VAR NON_FUNCTION
                    | FUNC_CALL NON_FUNCTION
                    | empty
    '''

def p_FUNC_CALL(p):
    '''
    FUNC_CALL   : ID OPEN_PARENTHESIS CLOSE_PARENTHESIS
    '''
    print("\tCORRECTO FUNC_CALL")

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
    print(expression_stack)
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
            | ID
            | MATRIX_VAL
            | STRING
    '''
    global exp_list
    for x in p:
        if x:
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
    global main_instruction_stack
    global jumps
    global func_dic

    print("Lista de expresiones: ", exp_list)
    print("Nombre de variables: ", var_name)
    print("Valor de variables: ", var_val)
    print("Instruction stack:")
    for instruction in instruction_stack:
        print(instruction)
    print("Expression stack:")
    for exp in expression_stack:
        print(exp)
    print("Jumps: ", jumps)
    print("Functions: ", func_dic)

def reset():
    global exp_list
    global var_name
    global var_val
    global instruction_stack
    global expression_stack
    global main_instruction_stack
    global jumps
    global func_dic

    exp_list.clear()
    var_name.clear()
    var_val.clear()
    instruction_stack.clear()
    expression_stack.clear()
    jumps.clear()
    func_dic.clear()

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
func_jumps = []
func_dic = {}
var_stack_helper = []

var_val_temporal = []
temp_counter=0


#######################################################################
########### Quadruplos de instrucciones
#######################################################################

instruction_stack = []
expression_stack = []
temp_var_stack = []
jump_stack = []
jumps = []
for_jumps = []
struc_stack = [(0,0)]

#######################################################################

def main():
    while True:
        try:
            print("Write the code instruction here:")
            s = input()
            if(s == 'x'):
                break
        except EOFError:
            break
        # file = open("code.txt")
        # line = file.read().replace("\n", " ")
        lexer.input(s)
        # file.close()
        for tok in lexer:
            print(tok)
        parser.parse(s)
        global instruction_stack
        global expression_stack
        global jumps
        instruction_stack = instruction_stack[0:int(len(instruction_stack)/2)]
        print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        programStatus()
        index = 0
        while index < len(instruction_stack):
            operation = instruction_stack[index][0]
            print(instruction_stack[index])
            if(operation == "=" and isinstance(instruction_stack[index][1], str)):
                val = expression_stack.pop(0)
                instruction_stack[index][1] = val
                index+=1
                while(index < len(instruction_stack) and isinstance(instruction_stack[index][1], str) and instruction_stack[index][0] == "=" and instruction_stack[index][1] != '0'):
                    instruction_stack[index][1] = val
                    index+=1
                index-=1
            elif(operation == "print"):
                instruction_stack[index][1] = expression_stack.pop(0)
            index+=1
        print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        programStatus()
        exec = Executor()
        exec.execute(instruction_stack, jumps, func_dic)
        reset()

if __name__ == "__main__":
    main()
