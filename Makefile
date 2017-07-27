ind_cmd='stack exec dumb-etl -- --input tweets --matcher Indice'
pat_cmd='stack exec dumb-etl -- --input tweets --matcher Pattern'


all: build
build:
	stack build

memory:
	$(ind_cmd) +RTS -s
	$(pat_cmd) +RTS -s
html:
	bench $(ind_cmd) --output indice.html
	bench $(pat_cmd) --output regex.html

benchmark:
	stack bench
