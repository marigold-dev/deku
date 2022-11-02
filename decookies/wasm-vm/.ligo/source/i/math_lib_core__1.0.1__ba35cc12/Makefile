ligo_compiler?=docker run --rm -v "$$PWD":"$$PWD" -w "$$PWD" ligolang/ligo:stable

PROTOCOL_OPT?=

help:
	@echo  'Usage:'
	@echo  '  test            - Run integration tests (written in Ligo)'

test: test_ligo_utils test_ligo_math

test_ligo_utils: test/test_utils.mligo 
	@echo "Running integration tests (is_implicit, bytes_to_nat)"
	@$(ligo_compiler) run test $^ $(PROTOCOL_OPT)

test_ligo_math: test/test_math.mligo 
	@echo "Running integration tests (Math)"
	@$(ligo_compiler) run test $^ $(PROTOCOL_OPT)
