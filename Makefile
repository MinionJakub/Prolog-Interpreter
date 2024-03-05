clean:
	rm -r */*.cmi */*.cmx */*.out */*.o */*.cmo	*/*.output
	rm ./a.out ./interpreter_prologa.out	

install:
	ocamlyacc ./MyParser/yaccParser.mly
	ocamllex ./MyParser/lexer.mll
	ocamlopt ./MyParser/ast.ml
	ocamlopt -I ./MyParser/ ast.cmx ./MyParser/errors.ml
	ocamlopt -I ./MyParser/ ast.cmx ./MyParser/yaccParser.mli ./MyParser/yaccParser.ml
	ocamlopt -I ./MyParser/ errors.cmx ast.cmx yaccParser.cmx ./MyParser/lexer.mli ./MyParser/lexer.ml
	ocamlopt -I ./MyParser/ errors.cmx ast.cmx yaccParser.cmx lexer.cmx ./MyParser/parser.mli ./MyParser/parser.ml
	ocamlopt ./Monads/StateMaybeMonad.ml
	ocamlopt -I ./MyParser/ errors.cmx ast.cmx yaccParser.cmx lexer.cmx parser.cmx ./Interpreter/Common.ml
	ocamlopt -I ./MyParser/ -I ./Monads/ -I ./Interpreter/ errors.cmx ast.cmx yaccParser.cmx lexer.cmx parser.cmx StateMaybeMonad.cmx Common.cmx ./Interpreter/Robinson.ml
	ocamlopt -I ./MyParser/ -I ./Monads/ -I ./Interpreter/ errors.cmx ast.cmx yaccParser.cmx lexer.cmx parser.cmx StateMaybeMonad.cmx Common.cmx Robinson.cmx ./Interpreter/State_Uni.ml
	ocamlopt -I ./MyParser/ -I ./Monads/ -I ./Interpreter/ errors.cmx ast.cmx yaccParser.cmx lexer.cmx parser.cmx StateMaybeMonad.cmx Common.cmx Robinson.cmx State_Uni.cmx ./Interpreter/evaluator_monad.ml
	ocamlopt -I ./MyParser/ -I ./Monads/ -I ./Interpreter/ errors.cmx ast.cmx yaccParser.cmx lexer.cmx parser.cmx StateMaybeMonad.cmx Common.cmx Robinson.cmx State_Uni.cmx evaluator_monad.cmx ./Interpreter/main_monad_file.ml -o interpreter_prologa
