
# -----------------------------------------------------------------------------
# Aurorus2.py
#
# Analizador de léxico y sintaxis para el lenguaje "Aurorus2"
# Aurora Tijerina Berzosa - A01196690
# -----------------------------------------------------------------------------

import ply.lex as lex
import ply.yacc as yacc
from executor import Executor
import sys

reserved = {
    'read' : 'READ',
    '# print' : 'PRINT',
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

# t_ignore = r' \n'
 # RegEx para tokens
# t_ENDL = r'\n'
# # t_READ = r'read'
# t_OPEN_PARENTHESIS  = r'\('
# t_CLOSE_PARENTHESIS  = r'\)'
# # t_PRINT = r'# print'
# # t_IF = r'if'

# t_OPEN_BRACES = r'\{'
# t_OPEN_PARENTHESIS  = r'\('
# t_CLOSE_PARENTHESIS  = r'\)'
# t_SUMOP = r'\+'
# t_MULOP = r'\*'
# t_SUBOP = r'\-'
# t_DIVOP = r'\/'
# t_LE = r'<='
# t_GE = r'>='
# t_EQ = r'\='
# t_NE = r'!='
# t_LT = r'\<'
# t_GT = r'\>'
# t_AND = r'&&'
# t_OR = r'°°'
# t_COMMA = r'\,'

# t_IS_VALUE = r'<-'

# t_ELSE = r'else'
# t_WHILE = r'while'
# t_DO = r'do'
# t_FOR = r'for'
# t_FUNC = r'func'
# t_ID = r'[a-zA-Z_][a-zA-Z0-9_]*'`
# t_VAL = r'([0-9]+)'

t_ignore = r' \n'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Logic of structures
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# def t_ignore(t):
#     r' \n'
#     if t.value == "\n":
#         global var_stack_helper
#         var_stack_helper.clear()
#         print("New line!!!!!!!!!!!!!!!!!!!!!!!!")
#     return t


def t_READ(t):
    r'read'
    t.type = reserved.get(t.value, 'READ')

    global instruction_stack
    global var_stack_helper
    index = len(instruction_stack)-1
    # # print("READ instruction stack")
    # # print(instruction_stack)
    while(instruction_stack[index][1] != "0"):
        index-=1
    instruction_stack.insert(index, ["read", "", "", len(temp_var_stack)])
    index+=1
    while(index < len(instruction_stack)):
        instruction_stack[index][1] = len(temp_var_stack)
        index+=1
    temp_var_stack.append(0)
    var_stack_helper.clear()
    global assign_flag
    assign_flag = False
    # print("READ code")
    return t

def t_PRINT(t):
    r'print'
    t.type = reserved.get(t.value, 'PRINT')

    global instruction_stack

    instruction_stack.append(["print", "", "", ""])
    # print("IS" , instruction_stack[-1])
    global var_stack_helper
    var_stack_helper.clear()
    global assign_flag
    assign_flag = False
    # print("PRINT code")
    return t

def t_FUNC(t):
    r'func'
    t.type = reserved.get(t.value, 'FUNC')

    global func_jumps
    global instruction_stack
    global jump_stack
    global jumps
    global var_stack_helper
    global func_flag

    instruction_stack.append(["go", "", "", len(jumps)])
    # print("IS" , instruction_stack[-1])
    func_jumps.append(len(instruction_stack))
    jump_stack.append(len(jumps))
    jumps.append(0)
    struc_stack.append([2,0])
    # print("Appended FUNC to struc stack")
    var_stack_helper.clear()
    func_flag=True
    global assign_flag
    assign_flag = False
    # # print(func_jumps)
    # print("Inicio de FUNC")
    return t

def t_DO(t):
    r'do'
    t.type = reserved.get(t.value, 'DO')

    global struc_stack
    struc_stack.append([3,len(instruction_stack)])
    global var_stack_helper
    var_stack_helper.clear()
    global assign_flag
    assign_flag = False
    # print("Inicio de DO")
    return t

def t_WHILE(t):
    r'while'
    t.type = reserved.get(t.value, 'WHILE')

    # if(struc_stack[-1][0] == 1):
    global instruction_stack
    global jump_stack
    global temp_var_stack
    global jumps
    struc_stack.append([1,len(instruction_stack)])
    instruction_stack.append(["=", "", "", len(temp_var_stack)])
    # print("IS" , instruction_stack[-1])
    instruction_stack.append(["==", 0, len(temp_var_stack), len(jumps)])
    # print("IS" , instruction_stack[-1])
    temp_var_stack.append(0)
    jump_stack.append(len(jumps))
    jumps.append(0)
    # print("Inicio de WHILE")
    global var_stack_helper
    var_stack_helper.clear()
    global assign_flag
    assign_flag = False
    return t

def t_FOR(t):
    r'for'
    t.type = reserved.get(t.value, 'FOR')
    global var_stack_helper
    var_stack_helper.clear()
    global assign_flag
    assign_flag = False
    # print("Inicio de FOR")
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
    # print("IS" , instruction_stack[-1])
    instruction_stack.append(["==", 0, len(temp_var_stack), len(jumps)])
    # print("IS" , instruction_stack[-1])
    temp_var_stack.append(0)
    jump_stack.append(len(jumps))
    jumps.append(0)
    struc_stack.append([0,0])
    global var_stack_helper
    var_stack_helper.clear()
    global assign_flag
    assign_flag = False
    # print("Inicio de IF")
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
    # print("IS" , instruction_stack[-1])
    jumps[jumps.index(len(instruction_stack) - 1)] = len(instruction_stack)
    temp_var_stack.append(0)
    jump_stack.append(len(jumps))
    jumps.append(0)
    struc_stack.append([0,0])
    global var_stack_helper
    var_stack_helper.clear()
    global assign_flag
    assign_flag = False
    # print("Inicio de ELSE")
    return t

def t_CLOSE_BRACES(t):
    r'\}'
    t.type = reserved.get(t.value, 'CLOSE_BRACES')

    global instruction_stack
    global jump_stack
    global jumps
    global struc_stack
    # for inst in instruction_stack:
    #     # print(inst)
    # # print(struc_stack)
    # # print(jump_stack)
    # # print(jumps)
    struc = struc_stack.pop()
    while(struc[0] == 4):
        struc = struc_stack.pop()
    # print("Fin de estructura", struc)
    if(struc[0] == 1):
        instruction_stack.append(["go", "", "", len(jumps)])
        # print("IS" , instruction_stack[-1])
        jumps.append(struc[1])
        jumps[jump_stack.pop()] = len(instruction_stack)
    elif(struc[0] == 3):
        instruction_stack.append(["=", "", "", len(temp_var_stack)])
        # print("IS" , instruction_stack[-1])
        instruction_stack.append(["==", 1, len(temp_var_stack), len(jumps)])
        # print("IS" , instruction_stack[-1])
        temp_var_stack.append(0)
        jumps.append(struc[1])
    elif(struc[0] == 2):
        instruction_stack.append(["return", "", "", ""])
        # print("Function ended")
        jumps[jump_stack.pop()] = len(instruction_stack)
    elif(struc[0] == 0):
        jumps[jump_stack.pop()] = len(instruction_stack)
    global var_stack_helper
    var_stack_helper.clear()
    global assign_flag
    assign_flag = False
    # print("Final de estrucutura")
    # # print(struc_stack)
    # # print(jump_stack)
    # # print(jumps)
    return t

def t_OPEN_BRACES(t):
    r'\{'
    t.type = reserved.get(t.value, 'OPEN_BRACES')

    global var_stack_helper
    var_stack_helper.clear()
    return t

def t_MATRIX_VAL(t):
    r'([a-zA-Z_][a-zA-Z0-9_]*(\[([a-zA-Z0-9_])+])+)'
    t.type = reserved.get(t.value, 'MATRIX_VAL')

    global assign_flag
    global var_stack_helper
    if assign_flag:
        var_stack_helper.clear()
        var_stack_helper.append(t.value)
    else:
        var_stack_helper.append(t.value)
    # else:
    #     print("matrix VAL not saved", t.value)
    return t

def t_MATRIX(t):
    # r'(\[[a-zA-Z_0-9]+([:][a-zA-Z_0-9]+)+])'
    r'(\[[a-zA-Z_0-9]+([:][a-zA-Z_0-9]+)*])'
    t.type = reserved.get(t.value, 'MATRIX')
    # print("iniciación matrix")
    return t

def t_SEQUENCE(t):
    r'([a-zA-Z_0-9]+:[a-zA-Z_0-9]+)'
    t.type = reserved.get(t.value, 'SEQUENCE')

    global instruction_stack
    global jump_stack
    global temp_var_stack
    global jumps
    instruction_stack.append(["=", t.value, "", len(temp_var_stack)])
    # print("IS" , instruction_stack[-1])
    struc_stack.append([1, len(instruction_stack)])
    instruction_stack.append(["==", 0, len(temp_var_stack), len(jumps)])
    # print("IS" , instruction_stack[-1])
    instruction_stack.append(["-", len(temp_var_stack), 1, len(temp_var_stack)])
    # print("IS" , instruction_stack[-1])
    temp_var_stack.append(0)
    jump_stack.append(len(jumps))
    jumps.append(0)
    global var_stack_helper
    var_stack_helper.clear()
    # print("Inicio de FOR")
    return t

def t_IS_VALUE(t):
    r'<-'
    t.type = reserved.get(t.value, 'IS_VALUE')

    global var_stack_helper
    global instruction_stack
    global assign_flag
    # print(var_stack_helper)
    for index, var in enumerate(var_stack_helper):
        instruction_stack.append(["=", str(index), "", var])
        # print("Exp saved" , var)
    var_stack_helper.clear()
    assign_flag = True
    # print("Assigned to")
    return t

def t_OR(t):
    r'\%'
    t.type = reserved.get(t.value, 'OR')
    global var_stack_helper
    var_stack_helper.clear()
    return t

def t_STRING(t):
    r'"[.!a-zA-Z0-9[\],\\ +*/~?¿:-]+"'
    t.type = reserved.get(t.value, 'STRING')
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'ID')

    global var_stack_helper
    global struc_stack
    global func_dic
    global func_jumps
    global instruction_stack
    global func_flag
    global assign_flag
    # # print(struc_stack)
    # # print(func_jumps)
    # # print(func_dic)
    # # print(var_stack_helper)
    # # print(instruction_stack)
    if t.value in func_dic:
            instruction_stack.append(["func_go", "", "", func_dic[t.value]])
            # print("IS" , instruction_stack[-1])
            var_stack_helper.clear()
    elif(struc_stack[0][0] == 4):
        struc_stack.pop(0)
        var_stack_helper.append(t.value)
    elif struc_stack[-1][0] == 2 and not t.value in func_dic and func_flag:
        func_dic[t.value] = func_jumps.pop()
        func_flag = False
    elif not len(var_stack_helper):
        var_stack_helper.append(t.value)
    elif assign_flag:
        var_stack_helper.clear()
        var_stack_helper.append(t.value)
    else:
        var_stack_helper.append(t.value)
    return t

def t_VAL(t):
    r'([0-9]+)'
    t.type = reserved.get(t.value, 'VAL')
    return t
    
def t_error(t):
    if(t.value == "\n"):
        t.lexer.skip(1)
    else:
        print("Caracter inválido, terminando programa.")
        exit()

def t_COMMA(t):
    r'\,'
    t.type = reserved.get(t.value, 'COMMA')
    global struc_stack
    global assign_flag
    assign_flag = False
    struc_stack.append([4,0])
    return t

def t_LE(t):
    r'<='
    t.type = reserved.get(t.value, 'LE')
    global var_stack_helper
    var_stack_helper.clear()
    return t

def t_OPEN_PARENTHESIS(t):
    r'\('
    t.type = reserved.get(t.value, 'OPEN_PARENTHESIS')
    global var_stack_helper
    var_stack_helper.clear()
    return t

def t_CLOSE_PARENTHESIS(t):
    r'\)'
    t.type = reserved.get(t.value, 'CLOSE_PARENTHESIS')
    global var_stack_helper
    var_stack_helper.clear()
    return t

def t_SUMOP(t):
    r'\+'
    t.type = reserved.get(t.value, 'SUMOP')
    global var_stack_helper
    var_stack_helper.clear()
    return t

def t_MULOP(t):
    r'\*'
    t.type = reserved.get(t.value, 'MULOP')
    global var_stack_helper
    var_stack_helper.clear()
    return t

def t_SUBOP(t):
    r'\-'
    t.type = reserved.get(t.value, 'SUBOP')
    global var_stack_helper
    var_stack_helper.clear()
    return t

def t_DIVOP(t):
    r'\/'
    t.type = reserved.get(t.value, 'DIVOP')
    global var_stack_helper
    var_stack_helper.clear()
    return t

def t_EQ(t):
    r'\='
    t.type = reserved.get(t.value, 'EQ')
    global var_stack_helper
    var_stack_helper.clear()
    return t

def t_NE(t):
    r'!='
    t.type = reserved.get(t.value, 'NE')
    global var_stack_helper
    var_stack_helper.clear()
    return t

def t_LT(t):
    r'\<'
    t.type = reserved.get(t.value, 'LT')
    global var_stack_helper
    var_stack_helper.clear()
    return t

def t_GE(t):
    r'>='
    t.type = reserved.get(t.value, 'GE')
    global var_stack_helper
    var_stack_helper.clear()
    return t

def t_GT(t):
    r'\>'
    t.type = reserved.get(t.value, 'GT')
    global var_stack_helper
    var_stack_helper.clear()
    return t

def t_AND(t):
    r'&&'
    t.type = reserved.get(t.value, 'AND')
    global var_stack_helper
    var_stack_helper.clear()
    return t

# def t_NEW_LINE(t):
#     r'\n'
#     t.type = reserved.get(t.value, 'NEW_LINE')
#     global var_stack_helper
#     var_stack_helper.clear()
#     return t


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Empty/Error
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

def p_empty(p):
    'empty :'
    pass

def p_error(p):
    print("\t", p.type, " ERROR, termiando programa.")
    exit()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Estructuras
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start = 'MAIN_PROGRAM'

def p_MAIN_PROGRAM(p):
    '''
    MAIN_PROGRAM : STRUCTURES
    '''
    # print("\tCORRECTO MAIN")
    
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
    # print("\tCORRECTO STRUCTURES")

def p_VAR(p):
    '''
    VAR : ID COMMA VAR
        | MATRIX_VAL COMMA VAR
        | ID IS_VALUE EXP
        | MATRIX_VAL IS_VALUE EXP
    '''
    # for x in p:
    #     # # print("hey")
    #     if x and '[' in x:
    #         # # print("VAR: " + x)
    #         global expression_stack
    #         expression_stack.append([x])
    # print("\tCORRECTO VAR")

def p_INPUT(p):
    '''
    INPUT 	: ID COMMA INPUT
            | MATRIX_VAL COMMA INPUT
            | ID IS_VALUE READ OPEN_PARENTHESIS CLOSE_PARENTHESIS
            | MATRIX_VAL IS_VALUE READ OPEN_PARENTHESIS CLOSE_PARENTHESIS
    '''
    # print("\tCORRECTO INPUT")

def p_OUTPUT(p):
    '''
    OUTPUT  : PRINT OPEN_PARENTHESIS EXP CLOSE_PARENTHESIS
    '''
    # print("\tCORRECTO OUTPUT")

def p_IF_CONDITIONAL(p):
    '''
    IF_CONDITIONAL  : IF OPEN_PARENTHESIS EXP CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES ELSE_CONDITIONAL
                    | IF OPEN_PARENTHESIS EXP CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES
    '''
    # print("\tCORRECTO IF")

def p_ELSE_CONDITIONAL(p):
    '''
    ELSE_CONDITIONAL : ELSE OPEN_BRACES STRUCTURES CLOSE_BRACES
    '''
    # print("\tCORRECTO ELSE")

def p_WHILE_LOOP(p):
    '''
    WHILE_LOOP : WHILE OPEN_PARENTHESIS EXP CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES
    '''
    # print("\tCORRECTO WHILE")

def p_DO_WHILE_LOOP(p):
    '''
    DO_WHILE_LOOP : DO OPEN_BRACES STRUCTURES CLOSE_BRACES WHILE OPEN_PARENTHESIS EXP CLOSE_PARENTHESIS
    '''
    # print("\tCORRECTO DO WHILE")

def p_FOR_LOOP(p):
    '''
    FOR_LOOP : FOR OPEN_PARENTHESIS SEQUENCE CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES
    '''
    # print("\tCORRECTO FOR")

def p_FUNCTION(p):
    '''
    FUNCTION : FUNC ID OPEN_PARENTHESIS CLOSE_PARENTHESIS OPEN_BRACES NON_FUNCTION CLOSE_BRACES
    '''
    # print("\tCORRECTO FUNC")

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
    # print("\tCORRECTO FUNC_CALL")

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
    # print("Exp list", exp_list)
    exp_list.clear()
    # print("\tCORRECTO EXP")


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
            | MATRIX
            | SEQUENCE
    '''
    global exp_list
    for x in p:
        if x:
            exp_list.append(x)
    # print(exp_list)
    # print("\tCORRECTO VALUE")

# def p_SEQUENCE(p):
#     '''
#     EARITH 	: VALUE MULOP VALUE
#     '''
#     global exp_list
#     for x in p:
#         if x:
#             # # print("Value: " + x)
#             exp_list.append(x)

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
            # # print("Value: " + x)
            exp_list.append(x)
    # print(exp_list)
    # print("\tCORRECTO EARITH")

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
            # # print("Value: " + x)
            exp_list.append(x)
    # print(exp_list)
    # print("\tCORRECTO ELOGIC")

# def p_DIV_OP(p):
#     '''
#     DIV_OP	: VALUE DIVOP VALUE
#     '''
#     # print("\tCORRECTO DIV_OP")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Main program
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
def programStatus():
    global exp_list
    global var_name
    global var_val
    global instruction_stack
    global expression_stack
    global main_instruction_stack
    global jumps
    global func_dic
    global struc_stack

    print("Lista de expresiones: ", exp_list)
    print("Nombre de variables: ", var_name)
    print("Valor de variables: ", var_val)
    print("Instruction stack:")
    for index, instruction in enumerate(instruction_stack):
        print(index, ".", instruction)
    print("Expression stack:")
    for exp in expression_stack:
        print(exp)
    print("Jumps: ", jumps)
    print("Functions: ", func_dic)
    print("Struc stack:", struc_stack)

def reset():
    global exp_list
    global var_name
    global var_val
    global instruction_stack
    global expression_stack
    global jumps
    global func_dic
    global var_counter
    global func_jumps
    global func_flag
    global temp_var_stack
    global jump_stack
    global struc_stack

    exp_list.clear()
    var_name.clear()
    var_val.clear()
    instruction_stack.clear()
    expression_stack.clear()
    jumps.clear()
    func_dic.clear()
    func_jumps.clear()
    temp_var_stack.clear()
    jump_stack.clear()
    var_counter = 0
    func_flag = False
    struc_stack = [(0,0)]

#######################################################################
#######################################################################

lexer = lex.lex()
parser = yacc.yacc()
# lexer = lex.lex(debug=1)
# parser = yacc.yacc(debug=True)
var_val = []
var_name = []
exp_list = []
var_counter = 0
func_jumps = []
func_dic = {}
var_stack_helper = []
func_flag = False


#######################################################################
########### Quadruplos de instrucciones
#######################################################################

instruction_stack = []
expression_stack = []
temp_var_stack = []
jump_stack = []
jumps = []
struc_stack = [(0,0)]

#######################################################################

def main():
    # while True:
        # try:
        #     # print("Write the code instruction here:")
        #     s = input()
        #     if(s == 'x'):
        #         break
        # except EOFError:
        #     break
        # lexer.input(s)
    file = open("code.txt")
    line = file.read().replace("\n", " ")
    lexer.input(line)
    file.close()
    # for tok in lexer:
        # print(tok)
    reset()
    parser.parse(line)
    global instruction_stack
    global expression_stack
    global jumps
   #  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    # programStatus()
    index = 0
    while index < len(instruction_stack):
        operation = instruction_stack[index][0]
        if(operation == "=" and isinstance(instruction_stack[index][1], str) and not ":" in instruction_stack[index][1]):
            val = expression_stack.pop(0)
            instruction_stack[index][1] = val
            # print(instruction_stack[index])
            index+=1
            while(index < len(instruction_stack) and isinstance(instruction_stack[index][1], str) and instruction_stack[index][0] == "=" and instruction_stack[index][1] != '0' and not ":" in instruction_stack[index][1] and not isinstance(instruction_stack[index][3], int)):
                instruction_stack[index][1] = val
                # print(instruction_stack[index])
                index+=1
        elif(operation == "print"):
            instruction_stack[index][1] = expression_stack.pop(0)
            # print(instruction_stack[index])
            index+=1
        else:
            # print(instruction_stack[index])
            index+=1
    # programStatus()
    print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    exec = Executor()
    exec.execute(instruction_stack, jumps, func_dic)
    reset()

if __name__ == "__main__":
    main()
