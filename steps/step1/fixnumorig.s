	.text
	.def	 @feat.00;
	.scl	3;
	.type	0;
	.endef
	.globl	@feat.00
@feat.00 = 1
	.def	 _scheme_entry;
	.scl	2;
	.type	32;
	.endef
	.globl	_scheme_entry
	.p2align	4, 0x90
_scheme_entry:                          # @scheme_entry
# BB#0:
	movl	$7, %eax
	retl


