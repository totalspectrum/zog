{{
   A Just In Time compiler for ZPU on P2

   Overview:
      We translate sequences of ZPU instructions into "traces" in the "trace cache".
      The trace cache is held in HUB memory, and holds as many traces as will fit. The LUT
      holds indexes to the traces.
      A trace continues to an unconditional branch, unless the branch is to
      the start of the trace.

      Note that no individual ZPU instruction is permitted to take more than 8 P2 instructions; so we know if
      we get to that close to the end of the trace cache then we have to flush it and re-initialize the cache.
      
  Register usage:
      pb holds the current (runtime) PC
      ptrb holds the stack pointer

}}

#define TRACE_TAGS_SIZE 8

'' maximum number of P2 longs emitted for any one instruction
#define MAX_INSTR_LENGTH 8

VAR
  long cog

PUB start (params) : okay
  okay := cog := cognew(@enter, params) + 1   'Start emulator in a new COG

PUB stop
  if cog
    cogstop(cog-1)
    cog := 0

PUB getdispatch_table
  return @dispatch_table

PUB getzog
  return @enter

DAT
	org 0
enter

'
' initialization code: the parameter block holds:
'  0: zpu_memory_addr = base of ZPU memory area
'  4: zpu_memory_sz   = size of ZPU memory area
'  8: initial PC
' 12: initial stack pointer
' 16: HUB address of instruction table
' 20: <unused>
' 24: mailbox address
zpu_memory_addr
		rdlong	zpu_memory_addr, ptra++
zpu_memory_sz
		rdlong	zpu_memory_sz, ptra++
dispatch_tab_addr
		rdlong	pb, ptra++
temp
		rdlong	ptrb, ptra++
mbox_addr
		rdlong	dispatch_tab_addr, ptra++
temp2
		rdlong	temp, ptra++
cachepc
		rdlong	mbox_addr, ptra++
io_command_addr
		rdlong	io_command_addr, ptra++
io_port_addr
		rdlong	io_port_addr, ptra++
io_data_addr
		rdlong	io_data_addr, ptra++
pc_addr
		rdlong	pc_addr, ptra++
sp_addr
		rdlong	sp_addr, ptra++
tos_addr
		rdlong	tos_addr, ptra++
dm_addr
		rdlong	tos_addr, ptra++
debug_addr
		rdlong	debug_addr, ptra++

		add	ptrb, zpu_memory_addr
		add	pb, zpu_memory_addr

		''
		'' initialize the cache
		''
		mov	cachepc, #$200
		mov	temp, #15
.lp
		alts	temp, #trace_cachebase

		jmp	#start_running
		
		'' table for compiling instructions
		'' this is done in two levels, one for high nibble, then optionally tables for low nibble
nibble_table
		long	instr0x_table | $80000000
		long	addsp_compile
		long	instr2x_table | $80000000
		long	instr3x_table | $80000000
		long	storesp_compile
		long	storesp_N_compile
		long	loadsp_compile
		long	loadsp_N_compile
instr0x_table
		muxz	zpu_breakpoint_pat, basic_pat1_compile
		muxz	zpu_illegal_pat, basic_pat1_compile
		muxz	zpu_pushsp_compile, basic_pat2_compile
		muxz	zpu_illegal_pat, basic_pat1_compile
		long	zpu_poppc_compile
		add	tos, zpu_math_compile
		and	tos, zpu_math_compile
		or	tos, zpu_math_compile
		rdlong	0, zpu_load_compile
		not	tos, zpu_oneop_compile
		long	zpu_flip_compile
		long	zpu_nop_compile
		wrlong	0, zpu_store_compile
		long	zpu_popsp_compile
		long	zpu_illegal_compile
		long	zpu_illegal_compile

instr2x_table
		long	zpu_illegal_compile
		long	zpu_illegal_compile
		rdword	0, zpu_load_compile
		wrword	0, zpu_store_compile
		long	zpu_lt_compile
		long	zpu_le_compile
		long	zpu_ltu_compile
		long	zpu_leu_compile
		long	zpu_swap_compile
		long	zpu_mult_compile
		shr	0, zpu_shift_compile
		shl	0, zpu_shift_compile
		sar	0, zpu_shift_compile
		long	zpu_call_compile
		long	zpu_eq_compile
		long	zpu_ne_compile

instr3x_table
		neg	0, zpu_oneop_compile
		sub	0, zpu_math_compile
		xor	0, zpu_math_compile
		rdbyte	0, zpu_load_compile
		wrbyte	0, zpu_store_compile
		long	zpu_div_compile
		long	zpu_mod_compile
		long	zpu_eqbranch_compile
		long	zpu_nebranch_compile
		long	zpu_poppcrel_compile
		long	zpu_config_compile
		long	zpu_pushpc_compile
		long	zpu_syscall_compile
		long	zpu_pushspadd_compile
		mulu	0, zpu_math_op
		long	zpu_callpcrel_compile
		
		'' patterns for compilation
mov_pat		mov	tos, #0-0
neg_pat		neg	tos, #0-0
aug_pat		augs	#0
aug_mask	long	$007fffff	' 23 bits
locpb_pat
	_ret_	loc	pb, #0-0
locpb_mask	long	$000fffff	' 20 bits
pushtos_pat
		wrlong	tos, --ptrb
poppb_pat
		rdlong	pb, ptrb++
	_ret_	add	pb, zpu_memory_addr
poptemp_pat
		mov	temp, tos
		rdlong	tos, ptrb++
add_to_temp_pat
		add	temp, #0-0
branch_to_temp_pat
	_ret_	mov	pb, temp
		
zpu_illegal_pat
zpu_breakpoint_pat
		call	#\runtime_break

zpu_pushsp_pat
		call	#\runtime_pushsp
zpu_nop_pat
		nop
		
		'''''''''''''''''''''''''''''''''''''''''''''
		''  runtime support code: must be present while
		''  compiled code is running
		'''''''''''''''''''''''''''''''''''''''''''''
runtime_pushsp
			wrlong	tos, ptrb--
			mov	tos, ptrb
			add	tos, #4
	_ret_		sub	tos, zpu_memory_addr

runtime_popsp
			mov	ptrb, tos
			add	ptrb, zpu_memory_addr
	_ret_		mov	tos, ptrb
	
runtime_break
			mov	memp, pb
			sub	memp, zpu_memory_addr
	                wrlong  memp, pc_addr             'Dump registers to HUB.
			mov	memp, ptrb
			sub	memp, zpu_memory_addr
                        wrlong  memp, sp_addr
                        wrlong  tos, tos_addr
                        wrlong  data, dm_addr
                        mov     temp, #io_cmd_break     'Set I/O command to BREAK
                        wrlong  temp, io_command_addr
.wait                   rdlong  temp, io_command_addr wz
              if_nz     jmp     #.wait
	                ret

		'''''''''''''''''''''''''''''''''''''''''''''
		'' main interpreter entry point:
		'''''''''''''''''''''''''''''''''''''''''''''
		
		''
		'' jump to address in pb (which is already adjusted to HUB)
		''
set_pc
		getnib	temp, pb, #0
		alts	temp, #trace_zpc
		mov	temp2, 0-0		' fetch start of trace
		cmp	temp2, pb wz
	if_nz	jmp	#cache_miss		' if not in cache, recompile
		
		alts	temp, #trace_cachebase
		mov	cachepc, 0-0
		push	#set_pc
		jmp	cachepc

		' OK, we got a cache miss here
		' so we have to recompile
		altd temp, #trace_zpc
		mov  0-0, pb		' update trace zpc
		mov  orig_cachepc, cachepc	 ' save the starting cachepc
		mov  orig_pb, pb   		 ' save starting pb address
		mov  pendingPush, #0		 ' no immediate push is pending
compile
		rdbyte	pa, pb wc	' fetch next opcode
		add	pb, #1
	if_c	jmp	#is_imm		' if high bit set, this is an immediate instruction
		mov	pendingImm, #0
compile_non_imm
		getnib	temp, pa, #1	' get high nibble
		getnib	temp2, pa, #0	' get low nibble
		alts	temp, #nibble_table
		mov	opdata, 0-0 wc
		'' if high bit set, then we have another level of table
	if_c	alts	temp2, opdata
	if_c	mov	opdata, 0-0
		'' low 9 bits of opdata has the actual address
		mov	temp2, opdata
		and	temp2, #$1FF
		call	temp2
		
		' is there room for another instruction?
		' if not, close out the cache line
		mov	temp, cachepc
		sub	temp, orig_cachepc
		cmp	temp, #31-MAX_INSTR_LENGTH wcz
	if_b	jmp	#compile
	
		' emit a loc instruction to finish the trace
		mov	opcode, locpb_pat
		and    	pb, locpb_mask
		or     	opcode, pb
emit_opcode_and_done_compiling
		call   	#emit_opcode
done_compiling
		' OK, all done compiling
		push	#set_pc
		jmp	orig_cachepc

		''''''''''''''''''''''''''''''''''''''
		'' compilation routines
		''''''''''''''''''''''''''''''''''''''

		'' utility function: if pendingImm is
		'' nonzero, push tos and move immval to
		'' tos; this can emit up to 3 longs
		'' (1 for the push, 2 for the immediate move)
push_if_imm
		tjz	pendingImm, #.no_push

		'' emit a push; this is 1 long
		mov	opcode, pushtos_pat
		call	#emit_opcode
		'' now emit a mov of the immediate to tos
		'' this can emit up to 2 longs
		jmp	#emit_mov
.no_push
		ret

		''''''''''''''''''''''''''''''''''''''
		'' compile a single instruction
		'' make sure if there's a pending immediate
		'' it's pushed on the stack first
		''''''''''''''''''''''''''''''''''''''
basic_pat1_compile
		call	#push_if_imm

		'' DEST register of opdata has the actual pattern to emit
		mov	opptr, opdata
		shr	opptr, #9
		and	opptr, #$1ff
		jmp	#emit1

		'' emit 1-2 longs starting at "opptr"
emit2
		altd	opptr, #0
		wrlut	0-0, cachepc
		add	cachepc, #1
		add	opptr, #1
emit1
		altd	opptr, #0
		wrlut	0-0, cachepc
	_ret_	add	cachepc, #1

		'' compile zpu_poppc
zpu_poppc_compile
		tjz	pendingImm, #emit_tos_branch
		'' we know the address, it's in the immval field
		'' FIXME: if we're jumping back to the start of this trace, we could emit
		'' a JMP instruction directly
		add   	immval, zpu_addr_base
jmp_immval
		mov	opcode, locpb_pat
		and   	immval, locpb_mask
		or    	opcode, immval
		jmp	#emit_opcode_and_done_compiling
emit_tos_branch
		mov	opptr, #poppb_pat
		call	#emit2
		'' there's a branch here, so we're done compiling
		jmp	#done_compiling

zpu_poppcrel_compile
		tjz	pendingImm, #emit_tos_relative_branch
		'' at this point we know the address
		add	immval, pb
		sub	immval, #1	' we already incremented the PC, undo it
		jmp	#jmp_immval
emit_tos_relative_branch
		mov	opptr, #poptemp_pat
		call	#emit2
		'' now emit code to add current pb to temp
		mov	immval, pb
		sub    	immval, #1
		mov    	opcode, add_to_temp_pat
		call   	#emit_opcode_imm
		'' finally pop and return
		mov	opcode, branch_to_temp_pat
		jmp	#emit_opcode_and_done_compiling
		
		'''''''''''''''''''''''''''''''''''''
		'' compile a math operation
		'' opdata has the instruction pattern
		'''''''''''''''''''''''''''''''''''''
zpu_math_compile
		mov	opcode, opdata
		tjnz	pendingImm, #emit_opcode_imm
		'' OK, have to do things on the stack here
		mov    opptr, #poptemp_pat
		call   #emit2
		sets   opcode, #temp
		jmp    #emit_opcode

		''''''''''''''''''''''''''''''''''''''
		'' compile a loadh/loadb
		''''''''''''''''''''''''''''''''''''''
zpu_load_compile
		call	#push_if_imm
		mov	opptr, #update_addr_pat
		call	#emit1
		mov	opcode, opdata
		sets	opcode, #tos
		jmp	#emit_opcode
		
		''''''''''''''''''''''''''''''''''''''
		'' compile a loadh/loadb
		''''''''''''''''''''''''''''''''''''''
		''''''''''''''''''''''''''''''''''''''
		'' handle accumulating an immediate
		'' into immval
		'''''''''''''''''''''''''''''''''''''''
is_imm
		mov	immval, pa
		signx	immval, #7	' sign extend from bit 7
imm_loop
		rdbyte	pa, pb wc
		add	pb, #1
	if_nc	jmp	#done_imm
		shl	immval, #7
		and	pa, #$7f
		or	immval, pa
		jmp	#imm_loop
done_imm
		mov	pendingImm, #1
		jmp	#compile_non_imm

emit_mov
		abs	immval, immval wc
	if_c	mov	opcode, neg_pat
	if_nc	mov	opcode, mov_pat


		' emit an opcode with an immediate source value
		' "opcode" holds the instruction, including destination register
		' this may need augmenting
emit_opcode_imm
		bith	opcode, #IMM_BIT	'make sure opcode is immediate
		cmp	immval, #511 wcz
	if_be	jmp	#skip_aug
		andn	aug_pat, aug_mask
		mov	temp2, immval
		shr	temp2, #9
		or	aug_pat, temp2
		wrlut	cachepc, aug_pat
		add	cachepc, #1
skip_aug
		sets	opcode, immval
emit_opcode
		wrlut	opcode, cachepc
	_ret_	add	cachepc, #1


''''''''''''''''''''''
'' re-initialize the trace cache
''''''''''''''''''''''
reinit_cache
	neg	temp, #1
	mov	temp2, #0
	rep	#@.endloop, #TRACE_TAGS_SIZE
	altd	temp2, #trace_zpc
	mov	0-0, temp
	add	temp2, #1
.endloop
	ret
	
''''''''''''''''''''''
'' variables
''''''''''''''''''''''
trace_flags
		long	0
trace_zpc
		res	TRACE_TAGS_SIZE		' 16 longs for ZPU PC of start of cache
trace_cachebase
		res	TRACE_TAGS_SIZE		' 16 longs for LUT addresses of cache base
orig_cachepc
		res	1
pendingImm
		res	1		' if non-zero, immval holds an immediate which needs to be dealt with
immval
		res	1		' immediate to apply to instruction, if pendingImm is non-zero
opcode
		res	1
tos
		res	1		' top of stack
