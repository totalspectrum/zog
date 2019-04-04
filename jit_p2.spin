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
'' useful def for turning stuff on and off
#define ALWAYS
'#define DEBUG

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
t2
		add	temp, #4
tos
		mov	tos_addr, temp
memp
		add	temp, #4
opcode
		mov	dm_addr, temp
opdata
		add	temp, #4
opptr
		mov	debug_addr, temp
condition
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
		long	zpu_storesp_compile
		long	zpu_loadsp_compile
		long	zpu_loadsp_compile
instr0x_table
		muxz	zpu_breakpoint_pat, basic_pat1_compile
		long	zpu_illegal_compile
		muxz	zpu_pushsp_pat, basic_pat4_compile
		muxz	zpu_illegal_pat, basic_pat1_compile
		
		long	zpu_poppc_compile
		add	tos, zpu_math_compile
		and	tos, zpu_math_compile
		or	tos, zpu_math_compile
		
		rdlong	tos, zpu_load_compile
		muxz	zpu_not_pat, basic_pat1_compile
		muxz	zpu_flip_pat, basic_pat1_compile
		long	zpu_nop_compile
		
		muxz	zpu_store_pat, basic_pat1_compile
		muxz	zpu_popsp_pat, basic_pat3_compile
		long	zpu_illegal_compile
		long	zpu_illegal_compile

instr2x_table
		long	zpu_illegal_compile
		long	zpu_illegal_compile
		rdword	tos, zpu_load_compile
		wrword	tos, zpu_storeh_compile
		cmps	tos, zpu_lt_compile wcz
		cmps	tos, zpu_le_compile wcz
		cmp	tos, zpu_ltu_compile wcz
		cmp	tos, zpu_leu_compile wcz
		muxz	zpu_swap_pat, basic_pat1_compile
		long	zpu_mult_compile
		shr	tos, zpu_math_compile
		shl	tos, zpu_math_compile
		sar	tos, zpu_math_compile
		long	zpu_call_compile
		long	zpu_eq_compile
		long	zpu_ne_compile

instr3x_table
		muxz	zpu_neg_pat, basic_pat1_compile
		sub	tos, zpu_math_compile
		xor	tos, zpu_math_compile
		rdbyte	tos, zpu_load_compile
		
		wrbyte	tos, zpu_storeh_compile
		muxz	zpu_div_pat, basic_pat1_compile
		muxz	zpu_mod_pat, basic_pat1_compile
	if_z	muxz	0, zpu_eqbranch_compile
	
	if_nz	muxz	0, zpu_nebranch_compile
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
		loc	pb, #\0-0

locpb_ret_pat
	_ret_	loc	pb, #\0-0

locpa_and_tos_pat
locpa_pat
		loc	pa, #\0-0
		mov	tos, pa

end_branch_pat
		add	pb, temp
		ret
		
loc_mask	long	$000fffff	' 20 bits
pushtos_pat
		wrlong	tos, ptrb--

pushpb_pat
		wrlong	pb, ptrb--

mov_tos_into_temp_pat
poptemp_pat
		mov	temp, tos
poptos_pat	rdlong	tos, ++ptrb

add_to_temp_pat
		add	temp, #0-0
branch_to_temp_pat
	_ret_	mov	pb, temp

add_membase_to_tos_pat
		add	tos, zpu_memory_addr

zpu_illegal_pat
zpu_breakpoint_pat
		call	#\runtime_break

zpu_swap_pat
		ror	tos, #16
zpu_flip_pat
		rev	tos
zpu_neg_pat
		neg	tos,tos
zpu_not_pat
		not	tos,tos
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
		rdlong	tos, ptrb

loadsp_pat
		mov	temp, #0-0
		add	temp, ptrb
		wrlong	tos, ptrb--
		rdlong	tos, temp
storesp_pat
		mov	temp, #0-0
		add	temp, ptrb
		wrlong	tos, temp
		rdlong	tos, ++ptrb

set_le_pat
		mov	tos, #0
   if_le	mov	tos, #1

cmp_pat		cmp	tos, 0 wcz
wrnc_pat
		wrnc	tos
wrz_pat
		wrz	tos
wrnz_pat
		wrnz	tos
qmul_pat
		qmul	tos, 0
getqx_pat
		getqx	tos
zpu_div_pat
		call	#\runtime_do_div
zpu_mod_pat
		call	#\runtime_do_mod

addsp_0_pat
		add	tos, tos
addsp_N_pat
		rdlong	temp, ptrb[1]
		add	tos, temp

zpu_config_pat
		mov	cpu, tos
		rdlong	tos, ++ptrb

zpu_store_pat
		call	#\runtime_store

zpu_call_pat
		mov	pb, tos
		loc	pa, #\0
		mov	tos, pa
		
		'''''''''''''''''''''''''''''''''''''''''''''
		''  runtime support code: must be present while
		''  compiled code is running
		'''''''''''''''''''''''''''''''''''''''''''''
div_zero_error
runtime_break
			mov	memp, pb
			sub	memp, zpu_memory_addr
	                wrlong  memp, pc_addr             'Dump registers to HUB.
			mov	memp, ptrb
			sub	memp, zpu_memory_addr
                        wrlong  memp, sp_addr
                        wrlong  tos, tos_addr
                        wrlong  debug_info, debug_addr
                        mov     temp, #io_cmd_break     'Set I/O command to BREAK
                        wrlong  temp, io_command_addr
.wait                   rdlong  temp, io_command_addr wz
              if_nz     jmp     #.wait
	                ret

debug_info long 0

runtime_store
			rdlong	data, ++ptrb
			mov	memp, tos wc
			rdlong	tos, ++ptrb
		if_c	jmp	#write_long_zpu		' write special address
			add	memp, zpu_memory_addr
		_ret_	wrlong	data, memp

		'' calculate tos / data on stack
runtime_do_mod
			mov	temp2, #1	' flag for remainder
			jmp	#do_divmod
runtime_do_div
			mov	temp2, #2	' flag for division
do_divmod
			rdlong data, ++ptrb wz
		if_z	jmp    #div_zero_error
			abs    tos, tos wc
			muxc   t2, #%11	' store sign of tos
			abs    data, data wc,wz
	if_c		xor    t2, #%10			' store sign of data
			qdiv   tos, data
			test   temp2, #1 wz
	if_nz		jmp    #do_remainder
			getqx  tos			' get quotient
			test   t2, #%10 wc
	_ret_		negc   tos,tos
do_remainder
			getqy	tos		' get remainder
			test	t2, #%1 wc
	_ret_		negc	tos,tos
	
			
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
#ifdef DEBUG
		call	#runtime_break		' DEBUG CODE
#endif
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
#ifdef FIXME
		' are we finishing the trace?
		test	trace_flags, #1 wc
	if_c	jmp	#close_trace

		' is there room for another instruction?
		' if not, close out the cache line
		mov	temp, cachepc
		sub	temp, orig_cachepc
		cmp	temp, #31-MAX_INSTR_LENGTH wcz
	if_b	jmp	#compile
#endif	
close_trace
		' emit a loc instruction to finish the trace
		mov	opcode, locpb_ret_pat
		andn	opcode, loc_mask
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

		'' compile zpu_call:
		'' branch to tos and put old address in tos
		''
		'' if immval is valid we emit
		''   wrlong tos, ptrb--
		'' otherwise
		''    mov temp, tos
		'' then
		''    mov tos, ##CUR_PC
		''    ... then an appropriate indirect jmp
		''
zpu_callpcrel_compile
		mov	t2, #1		' pc relative
		jmp	#call_common
zpu_call_compile
		mov	t2, #0		' absolute address
call_common
		tjz	pendingImm, #call_indirect
		or	t2, #2	        ' flag that immval is good
		mov	opptr, #pushtos_pat
		jmp	#set_return_addr
call_indirect
		mov	opptr, #mov_tos_into_temp_pat
set_return_addr
		call	#emit1		' emit mov or pop, as set up above
		
		'' want to issue:
		'' loc pa, #\retaddr (in memp)
		'' mov tos, pa
		call	#emit_nextpc_into_tos

		'' and now, finally, issue the branch
		jmp	#compile_uncond_branch
		
		'' compile zpu_poppc
zpu_poppc_compile
		tjz	pendingImm, #jmp_indirect
		mov	t2, #2		' flags: 2= immval valid, 1 = relative branch
		jmp	#compile_uncond_branch
jmp_indirect
		mov	opptr, #poptemp_pat
		mov	t2, #0		' flags: 2= immval valid, 1 = relative branch
		'' need to pop tos into temp
		mov	opptr, #poptemp_pat
		call	#emit2
		jmp	#compile_uncond_branch

zpu_poppcrel_compile
		tjz	pendingImm, #jmp_indirect2
		mov	t2, #3		' flags: 2= immval valid, 1 = relative branch
		jmp	#compile_uncond_branch
jmp_indirect2
		mov	opptr, #poptemp_pat
		mov	t2, #1		' flags: 2= immval valid, 1 = relative branch
		jmp	#compile_uncond_branch

		'''''''''''''''''''''''''''''''''''''
		'' utility function: compile either an
		'' OP tos, IMM
		'' or
		'' mov temp, tos
		'' pop tos
		'' OP tos, temp
		'' where "opcode" is the instruction in opcode
		'''''''''''''''''''''''''''''''''''''
compile_op_tos_or_imm
		tjnz	pendingImm, #emit_opcode_imm
		mov	opptr, #poptemp_pat
		call	#emit2
		sets	opcode, #temp
		jmp	#emit_opcode
		
		'''''''''''''''''''''''''''''''''''''
		'' compile a math operation
		'' opdata has the instruction pattern
		'''''''''''''''''''''''''''''''''''''
zpu_math_compile
		mov	opcode, opdata
		jmp	#compile_op_tos_or_imm

zpu_mult_compile
		mov	opcode, qmul_pat
		call	#compile_op_tos_or_imm
		mov	opptr, #getqx_pat
		jmp	#emit1

		'' compile newtos < tos
		'' which is equivalent to (tos <= newtos)
		'' opdata has "cmp" or "cmps" as appropriate
zpu_lt_compile
zpu_ltu_compile
		mov	opcode, opdata
		call	#compile_op_tos_or_imm
		mov	opptr, #set_le_pat
		jmp	#emit2

		' newtos <= tos
		' is the same as !(tos < newtos)
zpu_le_compile
zpu_leu_compile
		mov	opcode, opdata
		call	#compile_op_tos_or_imm
		mov	opptr, #wrnc_pat
		jmp	#emit1

zpu_eq_compile
		mov	opcode, cmp_pat
		call	#compile_op_tos_or_imm
		mov	opptr, #wrz_pat
		jmp	#emit1
zpu_ne_compile
		mov	opcode, cmp_pat
		call	#compile_op_tos_or_imm
		mov	opptr, #wrnz_pat
		jmp	#emit1

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
		call	#push_if_imm
		and	pa, #$1F
		xor	pa, #$10		' weird that we need this
		shl	pa, #2 wz
	if_z	jmp	#loadsp_0
		'' now compile
		'' mov temp, #pa
		'' add temp, ptrb
		'' wrlong tos, ptrb--
		'' rdlong tos, temp
		sets	loadsp_pat, pa
		mov	opptr, #loadsp_pat
		jmp	#emit4
loadsp_0
		mov	opptr, #pushtos_pat
		jmp	#emit1
		
zpu_storesp_compile
		call	#push_if_imm
		and	pa, #$1F
		xor	pa, #$10
		shl	pa, #2 wz
	if_z	jmp	#do_storesp_0
		sets	storesp_pat, pa
		mov	opptr, #storesp_pat
		jmp	#emit4
do_storesp_0
		mov	opptr, #poptos_pat
		jmp	#emit1
zpu_addsp_compile
		call	#push_if_imm
		and	pa, #$F wz
	if_z	mov	opptr, #addsp_0_pat
	if_z	jmp	#emit1
		andn	addsp_N_pat, #$f
		or	addsp_N_pat, pa
		mov	opptr, #addsp_N_pat
		jmp	#emit2

		' emit code to put current pc into tos
		' temp should contain the offset (-1 or 0)
		' mov PB - ZPU_BASE_ADDR into tos
emit_nextpc_into_tos
		mov	temp, pb
		sub	temp, zpu_memory_addr
		jmp	#emit_temp_into_tos
emit_curpc_into_tos
		mov	temp, pb
		sub	temp, #1
		sub	temp, zpu_memory_addr
		jmp	#emit_temp_into_tos
emit_temp_into_tos
		andn	locpa_pat, loc_mask
		or	locpa_pat, temp
		mov	opptr, #locpa_and_tos_pat
		jmp	#emit2
		
zpu_pushpc_compile
		call	#push_if_imm
		mov	opptr, #pushtos_pat
		call	#emit1
		jmp	#emit_curpc_into_tos

		' compile a branch
		' there are 4 cases:
		'   t2&1: 0 = absolute branch, 1 = relative branch
		'   t2&2: 0 = no immediate, address was placed in temp register earlier
		'         1 = use value in immval
		'   "condition" has the P2 condition flags we want to use ($f0000000 for unconditional)
		' for immediate branches the compiled code looks like:
		'       loc pb, #\immval + CUR_PC-1  or loc pb, #\immval + ZPU_BASE_ADDR
		'  if_x ret
		'
		' for computed branches the compiled code looks like:
		'     loc pb, #\CUR_PC-1 or loc pb, #\ZPU_BASE_ADDR (relative or absolute)
		'     add pb, temp
		' if_x ret
compile_uncond_branch
		mov	condition, cond_mask
		'' fall through
compile_cond_branch
		test	t2, #1 wz ' check for absolute vs. relative
	if_z	mov	temp, zpu_memory_addr
	if_nz	mov	temp, pb
	if_nz	sub	temp, #1
		test	t2, #2 wz ' check for direct (immval good) vs. indirect
	if_nz	add	temp, immval
	
		mov	opcode, locpb_pat
		or	opcode, temp

		call	#emit_opcode
		
		' fix up the condition on the ret
		andn	end_branch_pat+1, cond_mask
		or	end_branch_pat+1, condition

		' now emit the actual branch
		mov	emitcnt, #2
		mov	opptr, #end_branch_pat
		
		test	t2, #2 wz   	   	' see if immval was valid   	
	if_nz	sub	emitcnt, #1		' emit 2 or 1 instruction
	if_nz	add	opptr, #1

		call	#emit
		cmp	condition, cond_mask wz
	if_z	jmp	#done_compiling
		ret
		
		' 
		' conditional branches
		' these test next-on-stack for 0, and if z/nz they do a relative
		' branch to the address in tos
		'
		' if there is a pending immediate immval, then that is the branch target:
		'          cmp    tos, #0 wz
		'          rdlong tos, ++ptrb
		'    	   loc	  pb, #\immval + CUR_PC - 1
		'   if_z   ret
		'
		'  IDEALLY we'd like the last two instructions to become
		'   if_z   jmp  #\CACHE_START(immval+CUR_PC-1)
		'
		' if there is no pending immediate
		'        mov temp, tos
		'        rdlong  tos, ++ptrb wz
		'        rdlong  tos, ++ptrb
		'        loc pb, #\CUR_PC-1
		'        add pb, temp
		' if_z   ret

pop_test_imm_pat
		cmp	tos, #0 wz
		rdlong	tos, ++ptrb
pop_test_alt_pat
		mov	temp, tos
		rdlong	tos, ++ptrb wz
		rdlong	tos, ++ptrb
		
zpu_eqbranch_compile
zpu_nebranch_compile
		mov	t2, #1			' set up for relative branch
		mov	condition, opdata
		and	condition, cond_mask
		tjz	pendingImm, #br_no_imm
		or	t2, #2			' immediate value valid
		mov	opptr, #pop_test_imm_pat
		mov	emitcnt, #2
		jmp	#br_common
br_no_imm
		mov	opptr, #pop_test_alt_pat
		mov	emitcnt, #3
br_common
		call	#emit
		jmp	#compile_cond_branch
		
zpu_storeh_compile	
zpu_syscall_compile
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
		mov	debug_info, immval
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

_ret_	mov	cachepc, dispatch_tab_addr
	
''''''''''''''''''''''
'' variables
''''''''''''''''''''''
cpu
		long	0
trace_flags
		long	1

zpu_io_base             long $80000000  'Start of IO access window
zpu_cog_start           long $80008000  'Start of COG access window in ZPU memory space

cond_mask		long $f0000000  ' mask for conditional execution bits in instruction

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

		fit	$1d0
