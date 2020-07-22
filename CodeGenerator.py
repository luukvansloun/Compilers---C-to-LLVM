import SymbolTable
from AST import AST

TYPES = {
			"int": "i32",
			"float": "float",
			"void": "void",
			"char": "i8"
		}

class CodeGenerator:

	def __init__(self, AST, SymbolTable):
		self.filestring = ""
		self.AST = AST
		self.SymbolTable = SymbolTable
		self.regCounter = 0
		self.stringCounter = 0
		self.labelCounter = 0
		self.currentDepth = 0
		self.currentFunction = ""
		self.current_symboltable = SymbolTable.root

	def writeToFile(self, filename):
		file = open(filename, "w+")
		file.write(self.filestring)
		file.close()

	def getType(self, C_Type):
		if C_Type[-1] == "*":
			C_Type = C_Type[:-1]
			return TYPES[C_Type] + "*"

		return TYPES[C_Type]

	def generateTypeID(self, current_ast_node):
		return "{} {}".format(self.getType(current_ast_node.type), "%" + current_ast_node.value)

	def typeCast(self, register, fromType, toType):
		returnType = ""

		# don't cast if they're the same type
		if fromType == toType:
			return register, fromType
		elif fromType == "i32":
			if toType == "float":
				self.filestring += "{} = sitofp i32 {} to float\n".format(self.getNewRegister(), register)
			elif toType == "i8":
				self.filestring += "{} = trunc i32 {} to i8\n".format(self.getNewRegister(), register)

		elif fromType == "float":
			if toType == "i32":
				self.filestring += "{} = fpext float {} to double\n".format(self.getNewRegister(), register)
				temp_register = "%" + str(self.regCounter)
				self.filestring += "{} = fptosi double {} to i32\n".format(self.getNewRegister(), temp_register)
			elif toType == "i8":
				self.filestring += "{} = fptosi float {} to i8\n".format(self.getNewRegister(), register)
			elif toType == "double":
				self.filestring += "{} = fpext float {} to double\n".format(self.getNewRegister(), register)

		elif fromType == "i8":
			if toType == "float":
				self.filestring += "{} = sitofp i8 {} to float\n".format(self.getNewRegister(), register)
			elif toType == "i32":
				self.filestring += "{} = sext i8 {} to i32\n".format(self.getNewRegister(), register)

		elif fromType == "i1":
			if toType == "i32":
				self.filestring += "{} = sext i1 {} to i32\n".format(self.getNewRegister(), register)
			elif toType == "i8":
				self.filestring += "{} = sext i1 {} to i8\n".format(self.getNewRegister(), register)

		return "%{}".format(str(self.regCounter)), toType


	def placeholder(self, left, right):
		"""Checks if 2 values are the same type and converts them if necessary"""
		type_order = {
			"i1": 0,
			"i8": 1,
			"i32": 2,
			"float":3
		}

		l_order = type_order[left[1]]
		r_order = type_order[right[1]]

		if l_order < r_order:
			left = self.typeCast(left[0], left[1], right[1])
		elif l_order > r_order:
			right = self.typeCast(right[0], right[1], left[1])

		return left, right


	def getNewRegister(self):
		self.regCounter += 1
		return "%" + str(self.regCounter)

	def getNewLabel(self):
		self.labelCounter += 1
		return str(self.labelCounter)

	def generate(self):
		"""Main function that starts the entire code generation"""

		for child in self.AST.children:
			if child.name == "include":
				self.generateInclude()
			elif child.name == "function":
				self.currentFunction = child.value
				self.generateFunction(child)
			elif child.name == "EOF":
				continue
			else:
				self.generateGlobalDeclaration(child)

		self.filestring += "\ndeclare void @llvm.memcpy.p0i8.p0i8.i32(i8* nocapture writeonly, i8* nocapture readonly, i32, i32, i1)"

	def generateInclude(self):
		self.filestring = "declare i32 @scanf(i8*, ...) nounwind\n\n" + self.filestring
		self.filestring = "declare i32 @printf(i8*, ...) nounwind\n" + self.filestring

	def generateFunction(self, current_ast_node):
		"""Generates the code for a function declaration/definition"""

		arguments = list()
		argumentsToDecl = list()
		for child in current_ast_node.children:
			if child.name == "argument":
				arguments.append(self.getType(child.type))
				argumentsToDecl.append(child)

		if len(current_ast_node.children) != 0:
			# if the function has a codeblock -> definition
			functionType = ""
			if current_ast_node.value == "main" and current_ast_node.type == "void":
				functionType = self.getType("int")
			else:
				functionType = self.getType(current_ast_node.type)

			if current_ast_node.getChild(-1).name == "code block":
				self.filestring += "\ndefine {} @{} (".format(functionType, current_ast_node.value)
				self.filestring += ','.join(arguments) + ') '
				self.regCounter = len(argumentsToDecl)
				self.generateCodeBlock(current_ast_node.getChild(-1), argumentsToDecl)



	def generateCodeBlock(self, current_ast_node, argumentList=[]):
		# Enter start of code block
		self.currentDepth += 1
		self.current_symboltable = self.current_symboltable.getCurrentChild()

		nonVoidFunc = True
		voidMain = False
		# check if every non-void function has a return
		if current_ast_node.parent.name == "function":
			if current_ast_node.parent.type != "void":
				if len(current_ast_node.children) == 0:
					raise SymbolTable.NoReturnException(current_ast_node.parent.value)
				if current_ast_node.getChild(-1).name != "return":
				 	raise SymbolTable.NoReturnException(current_ast_node.parent.value)
			else:
				nonVoidFunc = False
				if current_ast_node.parent.value == "main":
					voidMain = True

		if current_ast_node.parent.name != "if" and current_ast_node.parent.name != "while":
			self.filestring += "{\n"

		for i in range(len(argumentList)):
			self.generateDeclaration(argumentList[i])
			self.filestring += "store {} %{}, {}* %{}\n".format(self.getType(argumentList[i].type), str(i),
						self.getType(argumentList[i].type), argumentList[i].value) 


		for child in current_ast_node.children:
			if child.name == "code block":
				self.generateCodeBlock(child)
			# Declaration without definition
			elif len(child.children) == 0 and child.type is not None:
				if child.name != "array decl":
					self.generateDeclaration(child)
				else:
					raise SymbolTable.SemanticException("Size of array {} was not declared".format(child.value))
			elif child.name == "=":
				# If left child has no type -> declaration
				if child.type == "decl":
					self.generateDefinition(child)
				# Else -> assignment
				elif child.type == "ass":
					self.generateAssignment(child)
			elif child.name == "array decl":
				self.generateArrayDeclaration(child)
			elif child.name == "f call":
				# the only rvalue that can be called on its own is a function call
				# all the others are essentially useless and are thus not generated
				self.generateExpression(child)
			elif child.name == "if":
				self.generateIfStatement(child)
			elif child.name == "while":
				self.generateWhileStatement(child)
			elif child.name == "return":
				if nonVoidFunc:	
					self.generateReturn(child)
					break
				else:
					self.filestring += "ret void\n"
					break

		if current_ast_node.getChild(-1).name != "return":
			if voidMain:
				self.filestring += "ret i32 0\n"
			elif current_ast_node.parent.type == "void":
				self.filestring += "ret void\n"

		# Ending code block
		self.current_symboltable = self.current_symboltable.parent
		self.current_symboltable.incrementChildIndex()

		if current_ast_node.parent.name != "if" and current_ast_node.parent.name != "while":
			self.filestring += "}\n"

		self.currentDepth -= 1

	def generateExpression(self, current_ast_node):
		"""Function that generates all rvalue expressions"""

		node_name = current_ast_node.name
		node_value = current_ast_node.value

		if node_name == "identifier":
			node_type = self.getType(self.current_symboltable.getSymbolType(node_value))

			new_register = self.getNewRegister()
			load_register = self.current_symboltable.getSymbolRegister(node_value)
			self.filestring += "{} = load {}, {}* {}\n".format(new_register, node_type, node_type, load_register)

			return new_register, node_type

		elif node_name == "address":
			node_type = self.getType(self.current_symboltable.getSymbolType(node_value))
			new_register = self.current_symboltable.getSymbolRegister(node_value)

			return new_register, node_type

		elif node_name == "value":
			node_type = self.getType(self.current_symboltable.getSymbolType(node_value))
			load_register = self.current_symboltable.getSymbolRegister(node_value)

			self.filestring += "{} = load {}, {}* {}\n".format(self.getNewRegister(), node_type, node_type, load_register)
			new_register = "%{}".format(str(self.regCounter))
			left_type = node_type[:-1]
			self.filestring += "{} = load {}, {}* {}\n".format(self.getNewRegister(), left_type, left_type, new_register)

			return new_register, node_type 

		elif node_name == "constant":
			node_type = self.getType(current_ast_node.type)
			const_value = node_value

			if node_type == "i8":
				const_value = ord(const_value.strip("'"))
			elif node_type == "i8*":
				self.stringCounter += 1
				self.generateStringDef("string.{}".format(self.regCounter), const_value.strip('"'), False)
				const_value = self.getNewRegister()
				self.filestring += "{} = load i8*, i8** %string.{}.{}\n".format(const_value, self.regCounter-1, self.currentDepth)
			elif node_type == "float":
				new_register = self.getNewRegister()
				self.filestring += "{} = fptrunc double {} to float\n".format(new_register, const_value)
				const_value = new_register

			return const_value, node_type

		elif node_name == "f call":
			if node_value == "printf":
				self.generatePrint(current_ast_node)
			elif node_value == "scanf":
				self.generateScan(current_ast_node)
			else:
				arguments = list()
				for arg in current_ast_node.children:
					arg_reg, arg_type = self.generateExpression(arg)
					arguments.append("{} {}".format(arg_type, arg_reg))

				new_register = self.getNewRegister()
				func_type = self.current_symboltable.getSymbolType(node_value)

				self.filestring += "{} = call {} @{}({})\n".format(new_register, self.getType(func_type), node_value, ", ".join(arguments))
				return new_register, func_type

		elif node_name == "array":
			"""Access the nth element from an array"""
			node_type = self.getType(self.current_symboltable.getSymbolType(node_value))
			index_register, index_type = self.generateExpression(current_ast_node.getChild(0))

			array_length = self.current_symboltable.getSymbolLength(node_value)
			self.filestring += "{} = getelementptr inbounds [{} x {}], [{} x {}]* {}, i32 0, i32 {}\n".format(self.getNewRegister(), 
				array_length, node_type, array_length, node_type, "%" + node_value, index_register)
			self.filestring += "{} = load {}, {}* {}\n".format(self.getNewRegister(), node_type, node_type, '%' + str(self.regCounter - 1))
			return '%' + str(self.regCounter), node_type

		elif node_name in ["+", "-", "*", "/"]:
			return self.generateArithmetic(current_ast_node)

		elif node_name in ["==", "!=", "<", "<=", ">", ">="]:
			return self.generateCondition(current_ast_node)


	def generateGlobalDeclaration(self, current_ast_node):
		"""Generates a variable in the global scope"""

		globalName = "@" + current_ast_node.name

		# check if it's a declaration or definition
		value = ""
		if current_ast_node.value is not None:
			value = current_ast_node.value

		self.current_symboltable.registerDict[current_ast_node.name] = globalName
		self.filestring += "{} = global {} {}\n".format(globalName, self.getType(current_ast_node.type), value)

	def generateDeclaration(self, current_ast_node):
		register = "%" + current_ast_node.value
		self.filestring += "{} = alloca {} \n".format(register, self.getType(current_ast_node.type))
		self.current_symboltable.registerDict[current_ast_node.value] = register

	def generateDefinition(self, current_ast_node):
		"""Generates code of the form [type] [id] = [value]"""

		left_child = current_ast_node.getChild(0)
		right_child = current_ast_node.getChild(1)
		left_type = self.getType(left_child.type)

		# add the current depth(scope) to the variable name so that there is no variable hiding
		var_name = "{}.{}".format(left_child.value, self.currentDepth)

		string = False
		# strings are defined differently (in the global scope)
		if self.current_symboltable.getSymbolType(left_child.value) == "char*":
			string = True
			if right_child.name == "constant":
				self.generateStringDef(left_child.value, right_child.value.strip('"'), False)
				return
			elif right_child.name == "identifier":
				self.current_symboltable.symbolLength[left_child.value] = self.current_symboltable.getSymbolLength(right_child.value)

		# make sure the type on the right of the = is the same as on the left
		right_value, right_type = self.generateExpression(right_child)

		if right_child.name != "address" and right_child.name != "value":
			right_value = self.typeCast(right_value, right_type, left_type)[0]

		register = "%{}".format(var_name)
		self.current_symboltable.registerDict[left_child.value] = register

		self.filestring += "{} = alloca {}\n".format(register, left_type)

		if right_child.name == "value":
			self.filestring += "store {} {}, {}* {}\n".format(left_type, "%{}".format(str(self.regCounter)), left_type, register)
		else:
			self.filestring += "store {} {}, {}* {}\n".format(left_type, right_value, left_type, register)

	def generateStringDef(self, stringName, string, newline=True):
		"""Generate a string definition in the global scope"""

		stringLength = len(string) + 1
		newline_str = ""
		if newline:
			newline_str = "\\0A"
			stringLength += 1

		if stringName[0] == "%":
			stringName = stringName[1:]

		# add scope depth to avoid variable hiding
		fullName = "{}.{}".format(stringName, self.currentDepth)
		globalName = "@{}.{}".format(self.currentFunction, fullName)

		newLineCount = string.count("\\n")
		stringLength -= newLineCount
		tabCount = string.count("\\t")
		stringLength -= tabCount
		string = string.replace("\\n", "\\0A")
		string = string.replace("\\t", "\\09")

		self.current_symboltable.registerDict[stringName] = "%{}".format(fullName)
		self.current_symboltable.symbolLength[stringName] = stringLength

		self.filestring = "{} = unnamed_addr constant [{} x i8] c\"{}{}\\00\"\n".format(globalName, stringLength, string, newline_str) + self.filestring
		self.filestring += "%{} = alloca i8*\n".format(fullName)
		self.filestring += "store i8* getelementptr inbounds ([{} x i8], [{} x i8]* {}, i32 0, i32 0), i8** %{}\n".format(stringLength, stringLength,
								globalName, fullName)

	def generateAssignment(self, current_ast_node):
		left_child = current_ast_node.getChild(0)
		right_child = current_ast_node.getChild(1)

		register = self.current_symboltable.getSymbolRegister(left_child.value)
		if self.current_symboltable.getSymbolType(left_child.value) == "char*":
			self.generateStringAssignment(current_ast_node)
			return
		elif left_child.name == "array":
			self.generateArrayAssignment(left_child)
			register = "%" + str(self.regCounter)

		right_value, right_type = self.generateExpression(right_child)
		
		var_type = self.getType(self.current_symboltable.getSymbolType(left_child.value))

		if right_child.name != "address" and right_child.name != "value":
			right_value = self.typeCast(right_value, right_type, var_type)[0]

		self.filestring += "store {} {}, {}* {}\n".format(var_type, right_value, var_type, register)

	def generateArrayAssignment(self, current_ast_node):
		arraySize = self.current_symboltable.getSymbolLength(current_ast_node.value)
		arrayType = self.getType(self.current_symboltable.getSymbolType(current_ast_node.value))
		arrayReg = self.current_symboltable.getSymbolRegister(current_ast_node.value)
		element = self.generateExpression(current_ast_node.getChild(0))[0]

		self.filestring += "{} = getelementptr inbounds [{} x {}], [{} x {}]* {}, i32 0, i32 {}\n".format(self.getNewRegister(),
								arraySize, arrayType, arraySize, arrayType, arrayReg, element)

	def generateStringAssignment(self, current_ast_node):
		left_child = current_ast_node.getChild(0)
		right_child = current_ast_node.getChild(1)

		string = right_child.value.strip('"')
		stringLength = int(len(string)) + 1
		newStringReg = "@{}.{}.{}".format(self.currentFunction, left_child.value, self.stringCounter)

		self.current_symboltable.symbolLength[left_child.value] = stringLength

		self.filestring = "{} = unnamed_addr constant [{} x i8] c\"{}\\00\"\n".format(newStringReg, stringLength, string) + self.filestring
		self.filestring += "store i8* getelementptr inbounds ([{} x i8], [{} x i8]* {}, i32 0, i32 0), i8** {}\n".format(stringLength, stringLength, newStringReg, 
			self.current_symboltable.getSymbolRegister(left_child.value))


	def generateArithmetic(self, current_ast_node):
		instructions =  {
							"+": "add",
							"-": "sub",
							"*": "mul",
							"/": "div"
					    }

		operand = current_ast_node.name

		# make sure both types are the same
		left = self.generateExpression(current_ast_node.getChild(0))
		right = self.generateExpression(current_ast_node.getChild(1))
		left, right = self.placeholder(left, right)

		# this needs to be changed to a cast etc
		instr = instructions[operand]
		if left[1] == "float":
			instr = "f" + instr
		elif operand == "/":
			instr = "sdiv"

		new_register = self.getNewRegister()
		self.filestring += "{} = {} {} {}, {}\n".format(new_register, instr, left[1], left[0], right[0])

		return new_register, left[1]

	def generateCondition(self, current_ast_node):
		comp_operators = {
			"==": "eq",
			"!=": "ne",
			">": "sgt",
			">=": "sge",
			"<": "slt",
			"<=": "sle"
		}

		operator = comp_operators[current_ast_node.name]

		# make sure both types are the same
		left = self.generateExpression(current_ast_node.getChild(0))
		right = self.generateExpression(current_ast_node.getChild(1))
		left, right = self.placeholder(left, right)

		instruction = "icmp"
		# check if it's a float comparison
		if left[1] == "float":
			instruction = "fcmp"
			if operator[0] == 's':
				operator = 'o' + operator[1:]

		new_register = self.getNewRegister()
		self.filestring += "{} = {} {} {} {}, {}\n".format(new_register, instruction, operator, left[1], left[0], right[0])

		return new_register, "i1"

	def generateIfStatement(self, current_ast_node):
		labels = []
		labelIndex = 0

		for child in current_ast_node.children:
			if child.name == "code block":
				self.filestring += "\nlabel.{}:\n".format(labels[labelIndex])
				labelIndex += 1
				self.generateCodeBlock(child)
			elif child.name == "if":
				self.filestring += "\nlabel.{}:\n".format(labels[labelIndex])
				labelIndex += 1
				self.generateIfStatement(child)
			elif child.name == "while":
				labelIndex += 1
				self.generateWhileStatement(child, labels[1])
				continue
			else:
				condReg, condType = self.generateExpression(child)
				self.filestring += "br i1 {}, label %label.{}, label %label.{}\n\n".format(condReg, self.getNewLabel(), self.getNewLabel())
				labels = [self.labelCounter - 1, self.labelCounter]
				continue

			self.filestring += "br label %.placeholder.{}\n".format(str(int(self.currentDepth) + 1))

		if labelIndex == 1:
			self.filestring = self.filestring.replace("%.placeholder.{}".format(str(int(self.currentDepth) + 1)), "%label.{}".format(labels[1]))
			self.filestring += "\nlabel.{}:\n".format(labels[1])
		else:
			self.filestring = self.filestring.replace("%.placeholder.{}".format(str(int(self.currentDepth) + 1)), "%label.{}".format(self.getNewLabel()))
			self.filestring += "\nlabel.{}:\n".format(self.labelCounter)


	def generateWhileStatement(self, current_ast_node, conditionLabel=None):
		condition = current_ast_node.getChild(0)
		codeBlock = current_ast_node.getChild(1)

		not_in_if = conditionLabel is None

		# if we're in an if, a label was already made. If not, we have to make a new one
		if not_in_if:
			conditionLabel = self.getNewLabel()
			self.filestring += "br label %label.{}\n\n".format(conditionLabel)
		
		self.filestring += "\nlabel.{}:\n".format(conditionLabel)

		# generate condition block
		condReg, condType = self.generateExpression(condition)
		blockLabel = self.getNewLabel()
		# if we're in an if, the exit label was already made. If not, we have to make a new one
		exitLabel = "%.placeholder.{}".format(str(int(self.currentDepth) + 1))
		if not_in_if:
			exitLabel = "%label.{}".format(self.getNewLabel())

		self.filestring += "br i1 {}, label %label.{}, label {}\n\n".format(condReg, blockLabel, exitLabel)

		# generate code block
		self.filestring += "label.{}:\n".format(blockLabel)
		self.generateCodeBlock(codeBlock)
		self.filestring += "br label %label.{}\n\n".format(conditionLabel)

		if not_in_if:
			self.filestring += "{}:\n".format(exitLabel[1:])


	def generateArrayDeclaration(self, current_ast_node):
		"""Generates an array declaration/definition"""

		arrayName = current_ast_node.value
		arrayType = self.getType(current_ast_node.type)
		arraySize = -1
		
		# Just declarating the array
		if len(current_ast_node.children) == 1:
			arraySize = current_ast_node.getChild(0).value

			self.filestring += "%{} = alloca [{} x {}]\n".format(arrayName, arraySize, arrayType)

		# Declaration and definition
		else:
			arraySize = -1
			loopIndex = 0
			if current_ast_node.getChild(0).name == "array length":
				arraySize = int(current_ast_node.getChild(0).value)
				loopIndex = 1
			else:
				arraySize = len(current_ast_node.children)
			
			arrayElements = [0 for x in range(arraySize)]

			for i in range(loopIndex, len(current_ast_node.children)):
				if i - loopIndex >= len(arrayElements):
					break
				value = self.generateExpression(current_ast_node.getChild(i))[0]
				arrayElements[i - loopIndex] = value

			globalName = "@{}.{}".format(self.currentFunction, arrayName)

			self.filestring += "%{} = alloca [{} x {}]\n".format(arrayName, arraySize, arrayType)
			self.filestring = "{} = private unnamed_addr constant [{} x {}] [{}]\n".format(globalName, arraySize,
									arrayType, ", ".join(["{} {}".format(arrayType, x) for x in arrayElements])) + self.filestring
			self.filestring += "{} = bitcast [{} x {}]* %{} to i8*\n".format(self.getNewRegister(), arraySize, arrayType, arrayName)

			typeSize = -1
			if arrayType == "i8":
				typeSize = 1
			else:
				typeSize = 4

			self.filestring += "call void @llvm.memcpy.p0i8.p0i8.i32(i8* %{}, i8* bitcast ([{} x {}]* {} to i8*), i32 {}, i32 {}, i1 false)\n".format(
									str(self.regCounter), arraySize, arrayType, globalName, arraySize * typeSize, typeSize)

		self.current_symboltable.registerDict[arrayName] = "%{}".format(arrayName)
		self.current_symboltable.symbolLength[arrayName] = arraySize

	def generatePrint(self, current_ast_node):
		arguments = list()
		for child in current_ast_node.children:
			arg_reg, arg_type = self.generateExpression(child)
			try:
				arg_length = self.current_symboltable.getSymbolLength(child.value)
				if len(child.children) == 0 and child.name == "array":
					arg_type = "[{} x {}]*".format(arg_length, arg_type)
				
			except SymbolTable.UndeclaredException:
				if arg_type == "i8*":
					pass
				elif arg_type == "float":
					arg_reg, arg_type = self.typeCast(arg_reg, arg_type, "double")
				else:
					arg_reg, arg_type = self.typeCast(arg_reg, arg_type, "i32")
			arguments.append("{} {}".format(arg_type, arg_reg))

		self.filestring += "{} = call i32 (i8*, ...) @printf({})\n".format(self.getNewRegister(), ", ".join(arguments))


	def generateScan(self, current_ast_node):
		arguments = list()
		for child in current_ast_node.children:
			arg_reg, arg_type = self.generateExpression(child)
			# check if the variable is an array by seeing if the symboltable has a length stored for it
			try:
				arg_length = self.current_symboltable.getSymbolLength(child.value)
				arg_type = "[{} x {}]".format(arg_length, arg_type)
			except SymbolTable.UndeclaredException:
				pass

			if child.name == "address":
				arg_type += '*'
			arguments.append("{} {}".format(arg_type, arg_reg))

		self.filestring += "{} = call i32 (i8*, ...) @scanf({})\n".format(self.getNewRegister(), ", ".join(arguments))


	def generateReturn(self, current_ast_node):
		returnChild = current_ast_node.getChild(0)
		expr_register, expr_type = self.generateExpression(returnChild)
		self.filestring += "ret {} {}\n".format(expr_type, expr_register)
			

