
# -----------------------------------------------------------------------------
# Aurorus.py
#
# Analizador de léxico y sintaxis para lenguaje Aurorus
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
    # 'SUMOP',
    # 'MULOP',
    # 'SUBOP',
    # 'DIVOP'
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
# t_SUMOP = r'\+'
# t_MULOP = r'\*'
# t_SUBOP = r'\-'
# t_DIVOP = r'\\'

# t_ELSE = r'else'
# t_WHILE = r'while'
# t_DO = r'do'
# t_FOR = r'for'
# t_FUNC = r'func'
# t_ID = r'[a-zA-Z_][a-zA-Z0-9_]*'`
# t_VAL = r'([0-9]+)'

def t_VAL(t):
    r'([0-9]+)'
    t.type = reserved.get(t.value, 'VAL')
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'ID')
    global var_name_list
    var_name_list.append(t.value)
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
    #TODO def expression VALUE y ID
    '''
    VAR : ID COMMA VAR
        | ID IS_VALUE VALUE
    '''
    # global var_dicc[p.data]
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
    #TODO agregar BOOL expression
    '''
    IF_CONDITIONAL  : IF OPEN_PARENTHESIS CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES ELSE_CONDITIONAL
                    | IF OPEN_PARENTHESIS CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES
    '''
    print("\tCORRECTO IF")

def p_ELSE_CONDITIONAL(p):
    '''
    ELSE_CONDITIONAL : ELSE OPEN_BRACES STRUCTURES CLOSE_BRACES
    '''
    print("\tCORRECTO ELSE")

def p_WHILE_LOOP(p):
    '''
    WHILE_LOOP : WHILE OPEN_PARENTHESIS CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES
    '''
    print("\tCORRECTO WHILE")

def p_DO_WHILE_LOOP(p):
    '''
    DO_WHILE_LOOP : DO OPEN_BRACES STRUCTURES CLOSE_BRACES WHILE OPEN_PARENTHESIS CLOSE_PARENTHESIS
    '''
    print("\tCORRECTO DO WHILE")

def p_FOR_LOOP(p):
    '''
    FOR_LOOP : FOR OPEN_PARENTHESIS VALUE COLON VALUE CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES 
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

def p_VALUE(p):
    # TODO ad ELOGIC & EARITH
    # global var_value_list
    # global var_counter
    # var_value_list.append(p.value)
    # var_counter = var_counter + 1
    '''
    VALUE	: VAL
    '''

# def p_EARITH(p):
#     '''
#     EARITH 	: VALUE SUMOP VALUE
#             | VALUE SUBOP VALUE
#             | MULT_OP
#             | DIV_OP
#     '''
#     print("\tCORRECTO EARITH")

# def p_MULT_OP(p):
#     '''
#     MULT_OP	: VALUE MULOP VALUE
#     '''
#     print("\tCORRECTO MULT_OP")

# def p_DIV_OP(p):
#     '''
#     DIV_OP	: VALUE DIVOP VALUE
#     '''
#     print("\tCORRECTO DIV_OP")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Main program
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#lexer = lex.lex()
lexer = lex.lex(debug=1)
parser = yacc.yacc(debug=True)
var_name_list = []
var_value_list = []
var_counter = 0

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
