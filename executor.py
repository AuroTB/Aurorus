import numpy as np

class Executor:
    def __init__(self):
        self.var_stack = {}
        self.return_stack = []
        self.inst_index = 0

    def execute(self, instruction_stack, jumps, func_dic):
        print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        print("EXECUTION STARTS HERE")
        print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        # mat1 = np.array([[1, 2, 3], [4, 5, 6]])
        # mat2 = np.array([[7, 8], [9, 10], [11, 12]])
        # print(mat1)
        # print(mat2)
        # # mat3<-[f1:c2]
        # mat3 = np.zeros([2,2])
        # print(mat3)
        # index = 0
        # for row in range(0,2):
        #     for col in range(0,2):
        #         for counter in range(0,3):
        #             print(mat3[row, col], mat1[row, counter] ,mat2[counter, col])
        #             mat3[row, col]= mat3[row, col]+mat1[row, counter]*mat2[counter, col]
        #             # mat3[row, col]=index
        #         index+=1
        # print(mat3)
        # exit()

        x = 0
        while(x < len(instruction_stack)):
            instruction = instruction_stack[x]
            # print(x, ".", instruction)
            # ######################
            # Assing val instruction
            # ######################
            if(instruction[0] == '='):
                var = ""
                value = 0
                dimentions = []
                # get var to write
                if isinstance(instruction[3], str):
                    # position in matrix to write
                    if "[" in instruction[3]:
                        exp = instruction[3]
                        matrix_var = 0
                        var = exp[:exp.index("[")]
                        exp = exp[exp.index("[")+1:]
                        while "]" in exp:
                            dimention = exp[:exp.index("]")]
                            if not dimention.isnumeric():
                                if not dimention.isnumeric():
                                    if dimention not in self.var_stack:
                                        print("Variable", dimention, "no inicializada, terminando programa.")
                                        exit()
                                    dimention = self.var_stack[dimention]
                                dimentions.append(int(dimention))
                            else:
                                dimentions.append(int(dimention))
                            if not "[" in exp:
                                break
                            else:
                                exp = exp[exp.index("[")+1:]
                        # # print("Dimentions: ", dimentions)
                    # var to write
                    else:
                        var = instruction[3]
                # internal var to write
                elif isinstance(instruction[3], int):
                    var = instruction[3]
                else:
                    print("No se puede asignar un valor a", instruction[3])
                    exit()
                # get value to write
                if(isinstance(instruction[1], int)):
                    if(instruction[1] in self.var_stack):
                        value=self.var_stack[instruction[1]]
                        if not len(dimentions):
                            self.var_stack[var] = value
                            # # print(instruction[3], self.var_stack[var])
                        else:
                            position = dimentions[0]
                            m = len(dimentions) - 2
                            mat_shape = self.var_stack[var].shape[1:]
                            mat_shape_accumul = 1
                            for index, dimention in enumerate(dimentions[1:]):
                                mat_shape_accumul *= mat_shape[m - index]
                                # # print(position, dimention, mat_shape_accumul)
                                position+= mat_shape_accumul*dimention
                            np.put(self.var_stack[var], [position], [value])
                            # # print(self.var_stack[var])
                    else:
                        print("Variable", instruction[1], "no inicializada, terminando programa.")
                        exit()
                elif(isinstance(instruction[1], list)):
                    value = self.parse_subops(instruction[1])
                    if not len(dimentions):
                        self.var_stack[var] = value
                    else:
                        position = dimentions[0]
                        m = len(dimentions) - 2
                        mat_shape = self.var_stack[var].shape[1:]
                        mat_shape_accumul = 1
                        for index, dimention in enumerate(dimentions[1:]):
                            mat_shape_accumul *= mat_shape[m - index]
                            # # print(position, dimention, mat_shape_accumul)
                            position+= mat_shape_accumul*dimention
                        np.put(self.var_stack[var], [position], [value])
                        # # print(self.var_stack[var])
                elif(isinstance(instruction[1], str)):
                        num2 = instruction[1][instruction[1].index(":")+1:]
                        num1 = instruction[1][:instruction[1].index(":")]
                        # print(num1, num2)
                        # self.status()
                        if not num2.isnumeric():
                            if not num2 in self.var_stack:
                                print("Variable", num2, "no inicializada, terminando programa.")
                                exit()
                            num2 = self.var_stack[num2]
                        if not num1.isnumeric():
                            if not num1 in self.var_stack:
                                print("Variable", num1, "no inicializada, terminando programa.")
                                exit()
                            num1 = self.var_stack[num1]
                        # print(num1, num2)
                        self.var_stack[var] = abs(int(num2) - int(num1))
                else:
                    print("Plz fix u.u")
            # ######################
            # Comparison instruction
            # ######################
            elif(instruction[0] == '=='):
                # print(float(instruction[1]), float(self.var_stack[instruction[2]]))
                # print(float(instruction[1]) == float(self.var_stack[instruction[2]]))
                if(float(instruction[1]) == float(self.var_stack[instruction[2]])):
                    x = jumps[instruction[3]] - 1
            # ######################
            # Read instruction
            # ######################
            elif(instruction[0] == 'read'):
                input_given = input()
                if input_given.isnumeric():
                    self.var_stack[instruction[3]] = input_given
                    # print(instruction[3], self.var_stack[instruction[3]])
                elif(input_given[0] == "-" and input_given[1:].isnumeric()):
                    self.var_stack[instruction[3]] = int(input_given[1:])*-1
                else:
                    print("Por favor ingrese un valor numérico.")
                    x-=1
            # ######################
            # Print instruction
            # ######################
            elif(instruction[0] == 'print'):
                if '"' in instruction[1][0]:
                    var_to_print = instruction[1][0][1:-1]
                    print(var_to_print)
                elif len(instruction[1]) == 1:
                    print(self.var_stack[instruction[1][0]])
                else:
                    var_to_print = self.parse_subops(instruction[1])
                    print(var_to_print)
            # ######################
            # Go instruction
            # ######################
            elif(instruction[0] == 'go'):
                x = jumps[instruction[3]] - 1
            # ######################
            # Return instruction
            # ######################
            elif(instruction[0] == 'return'):
                x = self.return_stack.pop() - 1
                # print(x+1)
            # ######################
            # Function call instruction
            # ######################
            elif(instruction[0] == 'func_go'):
                self.return_stack.append(x+1)
                x = instruction[3] - 1
            # ######################
            # Control var instruction
            # ######################
            elif(instruction[0] == '-'):
                self.var_stack[instruction[3]] = self.var_stack[instruction[1]] - instruction[2]
            x+=1
            self.inst_index = x
        # self.status()
    
    def status(self):
        print(self.inst_index)
        print(self.var_stack)
        print(self.return_stack)

    def parse_subops(self, exp_list):
        # print("Lista inicial de expresiones: ", exp_list)
        # # print(self.var_stack)
        exp_list_copy = exp_list.copy()
        # matrix initialization
        if(isinstance(exp_list_copy[0], str) and exp_list_copy[0][0]=="["):
            dimentions = []
            exp = exp_list_copy[0]
            exp = exp[1:len(exp)-1]
            while len(exp):
                if ":" in exp:
                    var = exp[:exp.index(":")]
                    if not var.isnumeric():
                        if not var in self.var_stack:
                            print("Variable", var, "no inicializada, terminando programa.")
                            exit()
                        dimentions.append(int(self.var_stack[var]))
                    else:
                        dimentions.append(int(var))
                    exp = exp[exp.index(":")+1:]
                else:
                    if not exp.isnumeric():
                        if not exp in self.var_stack:
                            print("Variable", var, "no inicializada, terminando programa.")
                            exit()
                        dimentions.append(int(self.var_stack[exp]))
                    else:
                        dimentions.append(int(exp))
                    break
            # # print(dimentions)
            return np.zeros(dimentions)

        # Substitution of vars
        for index, exp in enumerate(exp_list_copy):
            # Matrix value
            if "[" in exp:
                # Ej: mat1[r]
                # # print(exp)
                var = exp[:exp.index("[")]
                exp = exp[exp.index("[")+1:]
                dimentions = []
                while "]" in exp:
                    dimention = exp[:exp.index("]")]
                    if not dimention.isnumeric():
                        if not dimention.isnumeric():
                            if dimention not in self.var_stack:
                                print("Variable", dimention, "no inicializada, terminando programa.")
                                exit()
                            dimention = self.var_stack[dimention]
                        dimentions.append(int(dimention))
                    else:
                        dimentions.append(int(dimention))
                    if not "[" in exp:
                        break
                    else:
                        exp = exp[exp.index("[")+1:]
                # print("": dimentions)
                if not var in self.var_stack:
                    print("Variable", var, "no inicializada, terminando programa.")
                    exit()
                value = self.var_stack[var]
                for dimention in dimentions:
                    # print("Dimensión", dimention)
                    value = value[dimention]
                exp_list_copy[index]= float(value)
            # Number
            elif exp.isnumeric():
                exp_list_copy[index] = float(exp)
            # Variable
            elif exp in self.var_stack:
                exp_list_copy[index] = float(self.var_stack[exp])
            # Variable no identificada en sistema
            elif exp not in ["*", "-", "+", "/", "(", ")", ">", ">=", "<", "<=", "=", "!=", "&&", "%"]:
                print("Variable", var, "no inicializada, terminando programa.")
                exit()

        # print("Parsed expression: ", exp_list_copy)

        exp_iterator=0
        while exp_iterator < len(exp_list_copy):
            if exp_list_copy[exp_iterator] == "(":
                op_position = 1
                while(exp_list_copy[exp_iterator-op_position] in ["*", "-", "+", "/", "(", ")", ">", ">=", "<", "<=", "=", "!=", "&&", "%"]):
                    op_position+=1
                begining_exp = exp_iterator-(op_position-1)*2-1
                subop = exp_list_copy[begining_exp:exp_iterator]
                simplification = self.parse_subops(subop)
                exp_list_copy = exp_list_copy[:begining_exp] + [str(simplification)] + exp_list_copy[exp_iterator+2:]
                exp_iterator=begining_exp + 1
            exp_iterator+=1
        return self.operate(exp_list_copy)

    def operate(self, exp_list):
        # # print("Exp list to operate", exp_list)
        length = len(exp_list)
        sim_list = []
        op=length-1
        num_index = 0
        while num_index < op:
            num = exp_list[num_index]
            if exp_list[op] not in ["*", "/"]:
                sim_list.append(exp_list[num_index])
                sim_list.append(exp_list[op])
                op-=1
                num_index+=1
            else:
                num_index_accumul = num
                num_index+=1
                while exp_list[op] in ["*", "/"] and op > num_index:
                    if exp_list[op] == "*":
                        # # print(num_index_accumul, " multiplied by ", num)
                        num_index_accumul*=exp_list[num_index]
                    elif exp_list[op] == "/":
                        # # print(num_index_accumul, " divided by ", num)
                        num_index_accumul/=exp_list[num_index]
                    op-=1
                    num_index+=1
                sim_list.append(str(num_index_accumul))
                if op > num_index:
                    sim_list.append(exp_list[op])
                    op-=1
            # # print("Sim list: ", sim_list)
        
        if(num_index == op):
            sim_list.append(exp_list[num_index])
        # # print("Sim list: ", sim_list)
        x=1
        while x < len(sim_list):
            helper = 0
            if sim_list[x] == "+":
                # # print(sim_list[x-1], " adds ", float(sim_list[x+1]))
                helper = float(sim_list[x-1]) + float(sim_list[x+1])
                if x+2 < len(sim_list):
                    sim_list = sim_list[:x-1] + [helper] + sim_list[x+2:]
                else:
                    sim_list = [helper]
                x=1
            elif sim_list[x] == "-":
                # # print(sim_list[x-1], " substracted by ", float(sim_list[x+1]))
                helper = float(sim_list[x-1]) - float(sim_list[x+1])
                if x+2 < len(sim_list):
                    sim_list = sim_list[:x-1] + [helper] + sim_list[x+2:]
                else:
                    sim_list = [helper]
                x=1
            else:
                x+=2
            # # print("Current: ", sim_list)
        if(len(sim_list) == 1):
            return sim_list[0]

        ##############################################################################
        # # print("Lista antes de bool logic: ", sim_list)

        length = len(sim_list)
        pointer = 1
        while pointer < len(sim_list):
            while pointer < len(sim_list) and sim_list[pointer] in [">", ">=", "<", "<=", "=", "!="]:
                accumul = float(sim_list[pointer - 1])
                if sim_list[pointer] == ">":
                    # # print(accumul, " bigger than ", float(sim_list[pointer+1]))
                    accumul = accumul>float(sim_list[pointer+1])
                elif sim_list[pointer] == ">=":
                    # # print(accumul, " bigger or equat to ", float(sim_list[pointer+1]))
                    accumul = accumul>=float(sim_list[pointer+1])
                elif sim_list[pointer] == "<":
                    # # print(accumul, " smaller than ", float(sim_list[pointer+1]))
                    accumul = accumul<float(sim_list[pointer+1])
                elif sim_list[pointer] == "<=":
                    # # print(accumul, " smaller or equal to ", float(sim_list[pointer+1]))
                    accumul = accumul<=float(sim_list[pointer+1])
                elif sim_list[pointer] == "=":
                    # # print(accumul, " equal to ", float(sim_list[pointer+1]))
                    accumul = accumul==float(sim_list[pointer+1])
                elif sim_list[pointer] == "!=":
                    # # print(accumul, " not equal to ", float(sim_list[pointer+1]))
                    accumul = accumul != float(sim_list[pointer+1])
                sim_list = sim_list[:pointer-1] + [accumul] + sim_list[pointer+2:]
                pointer+=2
                # # print("Sim list: ", sim_list)
            else:
                pointer+=2

        length = len(sim_list)
        pointer = 1
        while pointer < len(sim_list):
            accumul = sim_list[pointer - 1]
            if sim_list[pointer] == "&&":
                # # print(accumul, " AND ", sim_list[pointer+1])
                accumul = accumul and sim_list[pointer+1]
            elif sim_list[pointer] == "%":
                # # print(accumul, " OR ", sim_list[pointer+1])
                accumul = accumul or sim_list[pointer+1]
            sim_list = sim_list[:pointer-1] + [accumul] + sim_list[pointer+2:]
            pointer=1
            # # print("Sim list: ", sim_list, pointer)
        # # print("Result", sism_list[0])
        return sim_list[0]