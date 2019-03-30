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

''
'' various bits used in instructions
#define IMM_BIT  18

'' number of bytes used in
#define TRACE_TAGS_SIZE 8

'' maximum number of P2 longs emitted for any one instruction
#define MAX_INSTR_LENGTH 8

CON
'I/O control block commands
io_cmd_out      = $01
io_cmd_in       = $02
io_cmd_break    = $03
io_cmd_syscall  = $04

VAR
  long cog

PUB start (params) : okay
  okay := cog := cognew(@enter, params) + 1   'Start emulator in a new COG

PUB stop
  if cog
    cogstop(cog-1)
    cog := 0

PUB getdispatch_table
  return $70000

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
' 20: our own address?
' 24: mailbox address
zpu_memory_addr
		rdlong	zpu_memory_addr, ptra++		' 0
zpu_memory_sz
		rdlong	zpu_memory_sz, ptra++		' 4
dispatch_tab_addr
		rdlong	pb, ptra++			' 8
temp
		rdlong	ptrb, ptra++			' 12
mbox_addr
		rdlong	dispatch_tab_addr, ptra++	' 16
temp2
		rdlong	temp, ptra++			' 20
cachepc
		rdlong	temp, ptra++		' 24
io_command_addr
		mov	io_command_addr, temp
io_port_addr
		add	temp, #4
io_data_addr
		mov	io_port_addr, temp
pc_addr
		add	temp, #4
sp_addr
		mov	io_data_addr, temp
tos_addr
		add	temp, #4
dm_addr
		mov	pc_addr, temp
debug_addr
		add	temp, #4
data
		mov	sp_addr, temp
		add	temp, #4
		mov	tos_addr, temp
		add	temp, #4
		mov	dm_addr, temp
		add	temp, #4
		mov	debug_addr, temp
		add	temp, #4

		add	ptrb, zpu_memory_addr
		add	pb, zpu_memory_addr

		''
		'' initialize the cache
		''
		call	#reinit_cache
		mov	tos, #$FF
		'' and go do everything
		jmp	#start_running
		
		'' table for compiling instructions
		'' this is done in two levels, one for high nibble, then optionally tables for low nibble
nibble_table
		long	instr0x_table | $80000000
		long	zpu_addsp_compile
		long	instr2x_table | $80000000
		long	instr3x_table | $80000000
		long	zpu_storesp_compile
		long	zpu_storesp_N_compile
		long	zpu_loadsp_compile
		long	zpu_loadsp_N_compile
instr0x_table
		muxz	zpu_breakpoint_pat, basic_pat1_compile
		long	zpu_illegal_compile
		muxz	zpu_pushsp_pat, basic_pat4_compile
		muxz	zpu_illegal_pat, basic_pat1_compile
		
		long	zpu_poppc_compile
		add	tos, zpu_math_compile
		and	tos, zpu_math_compile
		or	tos, zpu_math_compile
		
		rdlong	0, zpu_load_compile
		not	tos, zpu_oneop_compile
		long	zpu_flip_compile
		long	zpu_nop_compile
		
		muxz	zpu_store_pat, basic_pat1_compile
		muxz	zpu_popsp_pat, basic_pat3_compile
		long	zpu_illegal_compile
		long	zpu_illegal_compile

instr2x_table
		long	zpu_illegal_compile
		long	zpu_illegal_compile
		rdword	0, zpu_load_compile
		wrword	0, zpu_storeh_compile
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
		wrbyte	0, zpu_storeh_compile
		long	zpu_div_compile
		long	zpu_mod_compile
		long	zpu_eqbranch_compile
		long	zpu_nebranch_compile
		long	zpu_poppcrel_compile
		muxz	zpu_config_pat, basic_pat2_compile
		long	zpu_pushpc_compile
		long	zpu_syscall_compile
		muxz	zpu_pushspadd_pat, basic_pat3_compile
		mul	tos, zpu_math_compile
		long	zpu_callpcrel_compile
		
		'' patterns for compilation
mov_pat		mov	tos, #0-0
neg_pat		neg	tos, #0-0
aug_pat		augs	#0
aug_mask	long	$007fffff	' 23 bits
locpb_pat
	_ret_	loc	pb, #\0-0
locpb_mask	long	$000fffff	' 20 bits
pushtos_pat
		wrlong	tos, ptrb--
poppb_pat
		rdlong	pb, ++ptrb
	_ret_	add	pb, zpu_memory_addr
poptemp_pat
		mov	temp, tos
		rdlong	tos, ++ptrb
add_to_temp_pat
		add	temp, #0-0
branch_to_temp_pat
	_ret_	mov	pb, temp

add_membase_to_tos_pat
		add	tos, zpu_memory_addr

zpu_illegal_pat
zpu_breakpoint_pat
		call	#\runtime_break

zpu_pushsp_pat
		wrlong	tos,ptrb--
		mov	tos, ptrb
		add	tos, #4
		sub	tos, zpu_memory_addr
		
zpu_pushspadd_pat
		shl	tos, #2
		add	tos, ptrb
		sub	tos, zpu_memory_addr
zpu_nop_pat
		or	temp, temp
		
zpu_popsp_pat
		mov	ptrb, tos
		add	ptrb, zpu_memory_addr
		mov	tos, ptrb

loadsp_pat
		mov	temp, #0-0
		add	temp, ptrb
		wrlong	tos, ptrb--
		rdlong	tos, temp

zpu_config_pat
		mov	cpu, tos
		rdlong	tos, ++ptrb

zpu_store_pat
		call	#\runtime_store
		
		'''''''''''''''''''''''''''''''''''''''''''''
		''  runtime support code: must be present while
		''  compiled code is running
		'''''''''''''''''''''''''''''''''''''''''''''
runtime_break
			mov	memp, pb
			sub	memp, zpu_memory_addr
	                wrlong  memp, pc_addr             'Dump registers to HUB.
			mov	memp, ptrb
			sub	memp, zpu_memory_addr
                        wrlong  memp, sp_addr
                        wrlong  tos, tos_addr
                        wrlong  cachepc, dm_addr
                        mov     temp, #io_cmd_break     'Set I/O command to BREAK
                        wrlong  temp, io_command_addr
.wait                   rdlong  temp, io_command_addr wz
              if_nz     jmp     #.wait
	                ret

runtime_store
			rdlong	data, ++ptrb
			mov	memp, tos wc
			rdlong	tos, ++ptrb
		if_c	jmp	#write_long_zpu		' write special address
			add	memp, zpu_memory_addr
		_ret_	wrlong	data, memp

write_long_zpu
'Write a LONG from "data" to ZPU memory at "address"
write_long_zpu          cmp     memp, zpu_cog_start wc
              if_c      jmp     #write_io_long

                        shr     memp, #2
                        altd    memp, #0
    _ret_               mov     0-0, data
   

write_io_long           wrlong  memp, io_port_addr    'Set port address
                        wrlong  data, io_data_addr    'Set port data
                        mov     temp, #io_cmd_out     'Set I/O command to OUT
                        wrlong  temp, io_command_addr
.wait                   rdlong  temp, io_command_addr wz 'Wait for command to be completed
              if_nz     jmp     #.wait
	      		ret

		'''''''''''''''''''''''''''''''''''''''''''''
		'' main interpreter entry point:
		'''''''''''''''''''''''''''''''''''''''''''''
start_running		
		''
		'' jump to address in pb (which is already adjusted to HUB)
		''
set_pc
		call	#runtime_break		' DEBUG CODE

		'' check here for a cache hit
		getnib	opdata, pb, #0
#ifdef NEVER
#warn not finished yet
		alts	opdata, #trace_zpc
		mov	temp2, 0-0		' fetch start of trace
		cmp	temp2, pb wz
	if_nz	jmp	#cache_miss		' if not in cache, recompile
		
		alts	opdata, #trace_cachebase
		mov	cachepc, 0-0
		push	#set_pc
		jmp	cachepc
#endif

cache_miss
		' OK, we got a cache miss here
		' so we have to compile a new trace

		' if the cache is full, flush it
		call	#reinit_cache
		
		altd opdata, #trace_zpc
		mov  0-0, pb		' update trace zpc
		mov  orig_cachepc, cachepc	 ' save the starting cachepc
		mov  orig_pb, pb   		 ' save starting pb address
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

done_instruction
		' are we finishing the trace?
		test	trace_flags, #1 wc
	if_c	jmp	#close_trace
	
		' is there room for another instruction?
		' if not, close out the cache line
		mov	temp, cachepc
		sub	temp, orig_cachepc
		cmp	temp, #31-MAX_INSTR_LENGTH wcz
	if_b	jmp	#compile
close_trace
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

zpu_nop_compile
		call	#push_if_imm
		mov	opptr, #zpu_nop_pat
		jmp	#emit1
		
		''''''''''''''''''''''''''''''''''''''
		'' compile a single instruction
		'' make sure if there's a pending immediate
		'' it's pushed on the stack first
		''''''''''''''''''''''''''''''''''''''
basic_pat2_compile
		mov	emitcnt, #2
		jmp	#basic_pat_compile
basic_pat3_compile
		mov	emitcnt, #3
		jmp	#basic_pat_compile
basic_pat4_compile
		mov	emitcnt, #4
		jmp	#basic_pat_compile

basic_pat1_compile
		mov	emitcnt, #1
		'' fall through
basic_pat_compile
		call	#push_if_imm

		'' DEST register of opdata has the actual pattern to emit
		mov	opptr, opdata
		shr	opptr, #9
		and	opptr, #$1ff
		jmp	#emit

		'' emit 1-3 longs starting at "opptr"
emitcnt		long	0
emit4
		mov	emitcnt, #4
		jmp	#emit
emit3
		mov	emitcnt, #3
		jmp	#emit
emit2
		mov	emitcnt, #2
		jmp	#emit
emit1
		mov	emitcnt, #1
emit
		setd	emit_instr, opptr
		nop
		sub	emitcnt, #1 wz
	if_nz	setq	emitcnt
emit_instr
		wrlong	0-0, cachepc
		add	emitcnt, #1
		shl	emitcnt, #2
	_ret_	add	cachepc, emitcnt

		'' compile zpu_poppc
zpu_poppc_compile
		tjz	pendingImm, #emit_tos_branch
		'' we know the address, it's in the immval field
		'' FIXME: if we're jumping back to the start of this trace, we could emit
		'' a JMP instruction directly
		add   	immval, zpu_memory_addr
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
		' add zpu memory base to tos
		mov	opptr, #add_membase_to_tos_pat
		call	#emit1

		' now actually emit the rdbyte tos, tos or similar instruction
		mov	opcode, opdata
		sets	opcode, #tos
		jmp	#emit_opcode

zpu_loadsp_compile
zpu_loadsp_N_compile
		call	#push_if_imm
		and	pa, #$1F
		xor	pa, #$10		' weird that we need this
		shl	pa, #2
		'' now compile
		'' mov temp, #pa
		'' add temp, ptrb
		'' wrlong tos, ptrb--
		'' rdlong tos, temp
		sets	loadsp_pat, pa
		mov	opptr, #loadsp_pat
		jmp	#emit4

zpu_storeh_compile	
zpu_addsp_compile
zpu_storesp_compile
zpu_storesp_N_compile
zpu_call_compile
zpu_callpcrel_compile
zpu_pushpc_compile
zpu_shift_compile
zpu_eq_compile
zpu_ne_compile
zpu_lt_compile
zpu_le_compile
zpu_ltu_compile
zpu_leu_compile
zpu_swap_compile
zpu_flip_compile
zpu_oneop_compile
zpu_mult_compile
zpu_div_compile
zpu_mod_compile
zpu_eqbranch_compile
zpu_nebranch_compile
zpu_syscall_compile
zpu_pushspadd_compile
zpu_illegal_compile
		mov	opptr, #zpu_breakpoint_pat
		jmp	#basic_pat1_compile
		
		''''''''''''''''''''''''''''''''''''''
		'' handle accumulating an immediate
		'' into immval
		'''''''''''''''''''''''''''''''''''''''
is_imm
		mov	immval, pa
		shl	immval, #25	' sign extend from bit 6
		sar	immval, #25
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
		wrlong	aug_pat, cachepc
		add	cachepc, #4
skip_aug
		sets	opcode, immval
emit_opcode
		wrlong	opcode, cachepc
	_ret_	add	cachepc, #4


''''''''''''''''''''''
'' re-initialize the trace cache
''''''''''''''''''''''
reinit_cache
	neg	temp, #1
	mov	temp2, #0
	rep	@.endloop, #TRACE_TAGS_SIZE
	altd	temp2, #trace_zpc
	mov	0-0, temp
	add	temp2, #1
.endloop

	mov	cachepc, dispatch_tab_addr
	ret
	
''''''''''''''''''''''
'' variables
''''''''''''''''''''''
cpu
		long	0
trace_flags
		long	1

zpu_io_base             long $80000000  'Start of IO access window
zpu_cog_start           long $80008000  'Start of COG access window in ZPU memory space

trace_zpc
		res	TRACE_TAGS_SIZE		' 16 longs for ZPU PC of start of cache
trace_cachebase
		res	TRACE_TAGS_SIZE		' 16 longs for LUT addresses of cache base
orig_cachepc
		res	1
orig_pb
		res	1
pendingImm
		res	1		' if non-zero, immval holds an immediate which needs to be dealt with
immval
		res	1		' immediate to apply to instruction, if pendingImm is non-zero
opcode
		res	1
opdata
		res	1
tos
		res	1		' top of stack
memp
		res	1		' pointer to memory
opptr
		res	1		' pointer to some code to emit