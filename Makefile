all: compile
compile: compile_engine compile_website
clean: clean_engine clean_website clean_emacs_vsn_files
docs: docs_engine docs_website

compile_engine:
	cd engine; make

compile_website:
	cd website; make

docs_engine:
	cd engine; make docs

docs_website:
	cd website; make docs

clean_emacs_vsn_files:
	rm -rf *~

clean_engine:
	cd engine/ && make clean

clean_website:
	cd website/ && make clean

clean_docs:
	cd engine; make clean_docs
	cd website; make clean_docs

#run: compile
#	erl

run_engine: compile_engine
	cd engine; make run

run_website: compile_website
	cd website; make run

