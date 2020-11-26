import numpy as np

class Executor:
    def __init__(self):
        self.var_stack = {}
        self.return_stack = []

    def execute(self, instruction_stack, jumps, func_dic):
        print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        print("EXECUTION STARTS HERE")
        print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        x = 0
        while(x < len(instruction_stack)):
            instruction = instruction_stack[x]
            if(instruction[0] == '='):
                if(isinstance(instruction[1], int)):
                    self.var_stack[instruction[3]] = self.var_stack[instruction[1]]
                elif(isinstance(instruction[1], list)):
                    value = self.parse_subops(instruction[1])
                    self.var_stack[instruction[3]] = value
            elif(instruction[0] == '=='):
                if(self.var_stack[instruction[2]] == instruction[1]):
                    x = jumps[instruction[3]]
            elif(instruction[0] == 'read'):
                self.var_stack[instruction[3]] = input()
            elif(instruction[0] == 'print'):
                print(self.parse_subops(instruction[1]))
            elif(instruction[0] == 'go'):
                x = jumps[instruction[3]]
            elif(instruction[0] == 'return'):
                x = self.return_stack.pop(0)
            elif(instruction[0] == 'func_go'):
                self.return_stack.append(x+1)
            x+=1
        self.status()
    
    def status(self):
        print(self.var_stack)

    def parse_subops(self, exp_list):
        print("Lista inicial de expresiones: ", exp_list)
        x=0
        exp_list_copy = exp_list.copy()
        if(exp_list_copy[0][0]=="["):
            dimentions = []
            exp = exp_list_copy[0]
            exp = exp[1:len(exp)-1]
            while len(exp):
                if ":" in exp:
                    dimentions.append(int(exp[:exp.index(":")]))
                    exp = exp[exp.index(":")+1:]
                else:
                    dimentions.append(int(exp))
                    break
            print(dimentions)
            return np.zeros(dimentions)

        for index, exp in enumerate(exp_list_copy):
            if "[" in exp:
                # [678][74][53]
                var = exp[:exp.index("]")]
                exp = exp[exp.index("[")+1:len(exp)-1]
                dimentions_index = []
                while len(exp):
                    if "[" in exp:
                        dimentions.append(int(exp[:exp.index("]")]))
                        exp = exp[exp.index("[")+1:]
                    else:
                        dimentions.append(int(exp[:len(exp)-1]))
                        break
                print(dimentions)
                value = var_stack[var]
                for dimention in dimentions:
                    value = value[dimention]
                exp_list_copy[index]=value

        while x < len(exp_list_copy):
            if exp_list_copy[x] == "(":
                op_position = 1
                while(exp_list_copy[x-op_position] in ["*", "-", "+", "/"]):
                    op_position+=1
                begining_exp = x-(op_position-1)*2-1
                subop = exp_list_copy[begining_exp:x]
                simplification = parse_subops(subop)
                exp_list_copy = exp_list_copy[:begining_exp] + [str(simplification)] + exp_list_copy[x+2:]
                x=begining_exp + 1
            x+=1
        return self.operate(exp_list_copy)

    def operate(self, exp_list):
        length = len(exp_list)
        sim_list = []
        op=length-1
        num_index = 0
        while num_index < op:
            # print("op: ", exp_list[op])
            # print("num: ", exp_list[num_index])
            num = 0
            if exp_list[num_index] in self.var_stack:
                num = var_stack[exp_list[num_index]]
            else:
                num = float(exp_list[num_index])
            if exp_list[op] not in ["*", "/"]:
                sim_list.append(exp_list[num_index])
                sim_list.append(exp_list[op])
                op-=1
                num_index+=1
            else:
                num_index_accumul = num
                num_index+=1
                while exp_list[op] in ["*", "/", ">", ">=", "<", "<=", "=", "!="] and op > num_index:
                    if exp_list[op] == "*":
                        # print(num_index_accumul, " multiplied by ", num)
                        num_index_accumul*=num
                    elif exp_list[op] == "/":
                        # print(num_index_accumul, " divided by ", num)
                        num_index_accumul/=num
                    op-=1
                    num_index+=1
                sim_list.append(str(num_index_accumul))
                if op > num_index:
                    sim_list.append(exp_list[op])
                    op-=1
            # print("Sim list: ", sim_list)
        
        if(num_index == op):
            sim_list.append(exp_list[num_index])
        # print("Sim list: ", sim_list)
        x=1
        while x < len(sim_list):
            helper = 0
            if sim_list[x] == "+":
                # print(sim_list[x-1], " adds ", float(sim_list[x+1]))
                helper = float(sim_list[x-1]) + float(sim_list[x+1])
                if x+2 < len(sim_list):
                    sim_list = sim_list[:x-1] + [helper] + sim_list[x+2:]
                else:
                    sim_list = [helper]
                x=1
            elif sim_list[x] == "-":
                # print(sim_list[x-1], " substracted by ", float(sim_list[x+1]))
                helper = float(sim_list[x-1]) - float(sim_list[x+1])
                if x+2 < len(sim_list):
                    sim_list = sim_list[:x-1] + [helper] + sim_list[x+2:]
                else:
                    sim_list = [helper]
                x=1
            else:
                x+=2
            # print("Current: ", sim_list)
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
                    # print(accumul, " bigger than ", float(sim_list[pointer+1]))
                    accumul = accumul>float(sim_list[pointer+1])
                elif sim_list[pointer] == ">=":
                    # print(accumul, " bigger or equat to ", float(sim_list[pointer+1]))
                    accumul = accumul>=float(sim_list[pointer+1])
                elif sim_list[pointer] == "<":
                    # print(accumul, " smaller than ", float(sim_list[pointer+1]))
                    accumul = accumul<float(sim_list[pointer+1])
                elif sim_list[pointer] == "<=":
                    # print(accumul, " smaller or equal to ", float(sim_list[pointer+1]))
                    accumul = accumul<=float(sim_list[pointer+1])
                elif sim_list[pointer] == "=":
                    # print(accumul, " equal to ", float(sim_list[pointer+1]))
                    accumul = accumul==float(sim_list[pointer+1])
                elif sim_list[pointer] == "=":
                    # print(accumul, " equal to ", float(sim_list[pointer+1]))
                    accumul = accumul==float(sim_list[pointer+1])
                elif sim_list[pointer] == "!=":
                    # print(accumul, " not equal to ", float(sim_list[pointer+1]))
                    accumul = accumul != float(sim_list[pointer+1])
                sim_list = sim_list[:pointer-1] + [accumul] + sim_list[pointer+2:]
                pointer+=2
                # print("Sim list: ", sim_list)
            else:
                pointer+=2

        length = len(sim_list)
        pointer = 1
        while pointer < len(sim_list):
                accumul = sim_list[pointer - 1]
                if sim_list[pointer] == "&&":
                    # print(accumul, " AND ", sim_list[pointer+1])
                    accumul = accumul and sim_list[pointer+1]
                elif sim_list[pointer] == "°°":
                    # print(accumul, " OR ", sim_list[pointer+1])
                    accumul = accumul or sim_list[pointer+1]
                sim_list = sim_list[:pointer-1] + [accumul] + sim_list[pointer+2:]
                pointer=1
                # print("Sim list: ", sim_list, pointer)
        # print("Result", sism_list[0])
        return sim_list[0]

['=', ['3', '4', '>'], '', 0]
['==', False, 0, 0]
['read', '', '', 1]
['=', 1, '', 'x']
['print', ['5', '6', '>'], '', '']
['=', ['7', '4', '>'], '', 2]
['==', True, 2, 1]