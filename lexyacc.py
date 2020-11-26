
# -----------------------------------------------------------------------------
# HolaQueTal.py
#
# Analizador de léxico y sintaxis para Tarea "HolaQueTal"
# Aurora Tijerina Berzosa - A01196690
# -----------------------------------------------------------------------------

import ply.lex as lex
import ply.yacc as yacc
import sys

class LexYacc:

    def __init__(self):
        self.var_val = []
        self.var_name = []
        self.exp_list = []
        self.exp_op = []
        self.var_counter = 0
        self.curren_var = ""
        self.func_names = []
        self.var_stack_helper = []
        self.var_val_temporal = []
        self.temp_counter=0
        self.instruction_stack = []
        self.expression_stack = []
        self.temp_var_stack = []
        self.jump_stack = []
        self.jumps = []
        self.for_jumps = []
        self.struc_stack = [(0,0)]

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
    ] + list(reserved.values())

    t_ignore = r' \n'
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
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Logic of structures
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    def t_READ(self, t):
        r'read'
        t.type = reserved.get(t.value, 'READ')

        index = len(self.instruction_stack)-1
        print("READ instruction stack")
        print(self.instruction_stack)
        while(self.instruction_stack[index][1] != "0"):
            index-=1
        self.instruction_stack.insert(index, ["read", "", "", len(self.temp_var_stack)])
        index+=1
        while(index < len(self.instruction_stack)):
            self.instruction_stack[index][1] = len(self.temp_var_stack)
            index+=1
        self.temp_var_stack.append(0)
        self.var_stack_helper.clear()
        print("READ code")
        return t

    def t_PRINT(self, t):
        r'print'
        t.type = reserved.get(t.value, 'PRINT')

        self.instruction_stack.append(["print", "", "", ""])

        self.var_stack_helper.clear()
        print("PRINT code")
        return t

    def t_FUNC(self, t):
        r'func'
        t.type = reserved.get(t.value, 'FUNC')

        self.func_names.append(("", len(self.instruction_stack)))
        self.instruction_stack.append(["go", "", "", len(self.jumps)])
        self.func_names.append(("", len(self.instruction_stack)))
        self.jump_stack.append(len(self.jumps))
        self.jumps.append(0)
        self.struc_stack.append([2,0])
        
        self.var_stack_helper.clear()
        print("Inicio de FUNC")
        return t

    def t_DO(self, t):
        r'do'
        t.type = reserved.get(t.value, 'DO')

        self.struc_stack.append([3,len(self.instruction_stack)])

        self.var_stack_helper.clear()
        print("Inicio de DO")
        return t

    def t_WHILE(self, t):
        r'while'
        t.type = reserved.get(t.value, 'WHILE')

        if(struc_stack[-1][0] == 1):
            self.struc_stack.append([1,len(self.instruction_stack)])
            self.instruction_stack.append(["=", "", "", len(self.temp_var_stack)])
            self.instruction_stack.append(["==", False, len(self.temp_var_stack), len(self.jumps)])
            self.temp_var_stack.append(0)
            self.jump_stack.append(len(jumps))
            self.jumps.append(0)
            print("Inicio de WHILE")
            
            self.var_stack_helper.clear()
        return t

    def t_FOR(self, t):
        r'for'
        t.type = self.reserved.get(t.value, 'FOR')
        
        self.var_stack_helper.clear()
        print("Inicio de FOR")
        return t

    def t_IF(self, t):
        r'if'
        t.type = reserved.get(t.value, 'IF')

        self.instruction_stack.append(["=", "", "", len(self.temp_var_stack)])
        self.instruction_stack.append(["==", False, len(self.temp_var_stack), len(self.jumps)])
        self.temp_var_stack.append(0)
        self.jump_stack.append(len(self.jumps))
        self.jumps.append(0)
        self.struc_stack.append([0,0])

        self.var_stack_helper.clear()
        print("Inicio de IF")
        return t

    def t_ELSE(self, t):
        r'else'
        t.type = reserved.get(t.value, 'ELSE')
        
        self.instruction_stack.append(["go", "", "", len(self.jumps)])
        self.jumps[-1]+=1
        self.temp_var_stack.append(0)
        self.jump_stack.append(len(jumps))
        self.jumps.append(0)
        self.struc_stack.append([0,0])

        self.var_stack_helper.clear()
        print("Inicio de ELSE")
        return t

    def t_CLOSE_BRACES(self, t):
        r'\}'
        t.type = reserved.get(t.value, 'CLOSE_BRACES')
        
        print(self.struc_stack)
        print(self.jump_stack)
        print(self.jumps)
        struc = self.struc_stack.pop()
        if(struc[0] == 1):
            self.instruction_stack.append(["go", "", "", len(self.jumps)])
            self.jumps.append(struc[1])
            self.jumps[self.jump_stack.pop()] = len(self.instruction_stack)
        elif(struc[0] == 3):
            self.instruction_stack.append(["=", "", "", len(self.temp_var_stack)])
            self.instruction_stack.append(["==", True, len(self.temp_var_stack), len(self.jumps)])
            self.temp_var_stack.append(0)
            self.jumps.append(struc[1])
        else:
            self.jumps[self.jump_stack.pop()] = len(self.instruction_stack)
        
        self.var_stack_helper.clear()
        print("Final de estrucutura")
        print(self.struc_stack)
        print(self.jump_stack)
        print(self.jumps)
        return t

    def t_OPEN_BRACES(self, t):
        r'\{'
        t.type = reserved.get(t.value, 'OPEN_BRACES')
        
        self.var_stack_helper.clear()
        return t

    def t_SEQUENCE(self, t):
        r'([0-9]+:[0-9]+)'
        t.type = reserved.get(t.value, 'SEQUENCE')
        
        self.instruction_stack.append(["=", t.value, "", len(self.temp_var_stack)])
        self.struc_stack.append([1, len(self.instruction_stack)])
        self.instruction_stack.append(["==", False, len(self.temp_var_stack), len(self.jumps)])
        self.instruction_stack.append(["-", len(self.temp_var_stack), 1, len(self.temp_var_stack)])
        self.temp_var_stack.append(0)
        self.jump_stack.append(len(jumps))
        self.jumps.append(0)
        
        self.var_stack_helper.clear()
        print("Inicio de FOR")
        return t

    def t_IS_VALUE(self, t):
        r'<-'
        t.type = reserved.get(t.value, 'IS_VALUE')
        
        for index, var in enumerate(self.var_stack_helper):
            self.instruction_stack.append(["=", str(index), "", var])
        
        self.var_stack_helper.clear()
        return t
        print("Assigned to")

    def t_ID(self, t):
        r'[a-zA-Z_][a-zA-Z0-9_]*'
        t.type = reserved.get(t.value, 'ID')
        
        self.var_stack_helper.append(t.value)
        return t

    def t_VAL(self, t):
        r'([0-9]+)'
        t.type = reserved.get(t.value, 'VAL')
        return t
        
    def t_error(self, t):
        print("Caracter inválido.")
        t.lexer.skip(1)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Empty/Error
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    def p_empty(self, p):
        'empty :'
        pass

    def p_error(self, p):
        print("\t", p.type, " ERROR")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Estructuras
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    start = 'MAIN_PROGRAM'
    def p_MAIN_PROGRAM(self, p):
        '''
        MAIN_PROGRAM : STRUCTURES
        '''
        print("\tCORRECTO MAIN")
        
    def p_STRUCTURES(self, p):
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

    def p_VAR(self, p):
        '''
        VAR : ID COMMA VAR
            | ID IS_VALUE EXP
        '''
        print("\tCORRECTO VAR")

    def p_INPUT(self, p):
        '''
        INPUT 	: ID COMMA INPUT
                | ID IS_VALUE READ OPEN_PARENTHESIS CLOSE_PARENTHESIS
        '''
        print("\tCORRECTO INPUT")

    def p_OUTPUT(self, p):
        '''
        OUTPUT : PRINT OPEN_PARENTHESIS EXP CLOSE_PARENTHESIS
        '''
        print("\tCORRECTO OUTPUT")

    def p_IF_CONDITIONAL(self, p):
        '''
        IF_CONDITIONAL  : IF OPEN_PARENTHESIS EXP CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES ELSE_CONDITIONAL
                        | IF OPEN_PARENTHESIS EXP CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES
        '''
        print("\tCORRECTO IF")

    def p_ELSE_CONDITIONAL(self, p):
        '''
        ELSE_CONDITIONAL : ELSE OPEN_BRACES STRUCTURES CLOSE_BRACES
        '''
        print("\tCORRECTO ELSE")

    def p_WHILE_LOOP(self, p):
        '''
        WHILE_LOOP : WHILE OPEN_PARENTHESIS EXP CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES
        '''
        print("\tCORRECTO WHILE")

    def p_DO_WHILE_LOOP(self, p):
        '''
        DO_WHILE_LOOP : DO OPEN_BRACES STRUCTURES CLOSE_BRACES WHILE OPEN_PARENTHESIS EXP CLOSE_PARENTHESIS
        '''
        print("\tCORRECTO DO WHILE")

    def p_FOR_LOOP(self, p):
        '''
        FOR_LOOP : FOR OPEN_PARENTHESIS SEQUENCE CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES 
        '''
        print("\tCORRECTO FOR")

    def p_FUNCTION(self, p):
        '''
        FUNCTION : FUNC ID OPEN_PARENTHESIS CLOSE_PARENTHESIS OPEN_BRACES STRUCTURES CLOSE_BRACES 
        '''
        print("\tCORRECTO FUNC")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Expresiones
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    def p_EXP(self, p):
        '''
        EXP	: VALUE
        '''
        print(exp_list)
        self.expression_stack.append(self.exp_list)
        print(self.expression_stack)
        self.exp_list.clear()
        print("\tCORRECTO EXP")


    def p_VALUE(self, p):
        '''
        VALUE	: EARITH
                | ELOGIC
                | OPEN_PARENTHESIS VALUE CLOSE_PARENTHESIS
                | VAL
                | ID
        '''
        for x in p:
            if x:
                self.exp_list.append(x)
        print("\tCORRECTO VALUE")

    def p_EARITH(self, p):
        '''
        EARITH 	: VALUE MULOP VALUE
                | VALUE DIVOP VALUE
                | VALUE SUMOP VALUE
                | VALUE SUBOP VALUE
        '''
        for x in p:
            if x:
                self.exp_list.append(x)

        print("\tCORRECTO EARITH")

    def p_ELOGIC(self, p):
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
        for x in p:
            if x:
                self.exp_list.append(x)
        print("\tCORRECTO ELOGIC")


    def programStatus(self):
        print("Lista de expresiones: ", self.exp_list)
        print("Nombre de variables: ", self.var_name)
        print("Valor de variables: ", self.var_val)
        print("Instruction stack:")
        for instruction in self.instruction_stack:
            print(instruction)
        print("Expression stack:")
        for exp in self.expression_stack:
            print(exp)
        print(self.jumps)

    def build(self):
        self.lexer = lex.lex(debug=1)
        self.parser = yacc.yacc(debug=True)

    def reset(self):
        self.exp_list.clear()
        self.var_name.clear()
        self.var_val.clear()
        self.instruction_stack.clear()
        self.expression_stack.clear()
        self.jumps.clear()

    def parseCode(self, s):
        self.lexer.input(s)
        for tok in self.lexer:
            print(tok)
        self.parser.parse(s)

        self.instruction_stack = self.instruction_stack[0:int(len(instruction_stack)/2)]
        self.programStatus()
        for instruction in self.instruction_stack:
            operation = instruction[0]
            print(instruction)
            if(operation == "=" and instruction[1] != 0):
                instruction[1] = self.expression_stack.pop(0)
            elif(operation == "print"):
                instruction[1] = self.expression_stack.pop(0)
            print(instruction)
        
        self.reset()