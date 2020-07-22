import sys
from antlr4 import *
from graphviz import Digraph
from C_GrammarLexer import C_GrammarLexer
from C_GrammarParser import C_GrammarParser
from C_GrammarListener import C_GrammarListener
from C_GrammarVisitor import C_GrammarVisitor
from SymbolTable import SymbolTable
from AST import AST, GrammarListener
from CodeGenerator import CodeGenerator

def main(argv):
	input = FileStream(argv[1])
	print("\033[92mCompiling and executing {}\033[0m".format(argv[1]))

	lexer = C_GrammarLexer(input)
	stream = CommonTokenStream(lexer)
	parser = C_GrammarParser(stream)
	tree = parser.start()
	printer = GrammarListener()
	walker = ParseTreeWalker()
	walker.walk(printer, tree)

	symbol_table = SymbolTable()
	symbol_table.fill_tree(printer.AST_Root)	

	CG = CodeGenerator(printer.AST_Root, symbol_table)
	CG.generate()
	symbol_table.to_dot()	
	CG.writeToFile(argv[2])


if __name__ == '__main__':
	main(sys.argv)
