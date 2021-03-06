{{
   A Just In Time compiler for ZPU on P2

   Overview:
      We translate sequences of ZPU instructions into "traces" in the "trace cache".
      The trace cache is held in HUB memory, and holds as many traces as will fit. The LUT
      holds indexes to the traces.
      A trace continues to an unconditional branch, unless the branch is to
      the start of the trace.

      Note that no individual ZPU instruction is permitted to take more than 10 P2 instructions; so we know if
      we get to that close to the end of the trace cache then we have to flush it and re-initialize the cache.
      
  Register usage:
      pb holds the current (runtime) PC
      ptrb holds the stack pointer

}}
'' useful def for turning stuff on and off
#define ALWAYS
'#define DEBUG
'#define DEBUG_TRAMPOLINE

''
'' various bits used in instructions
#define IMM_BIT  18

'' maximum number of P2 longs emitted for any one instruction
#define MAX_INSTR_LENGTH 10

#define CACHE_SIZE $C000 - (MAX_INSTR_LENGTH*4)
#define CACHE_START $70000

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
  return CACHE_START

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
base_of_cache_mem
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
top_of_cache_mem
		add	pb, zpu_memory_addr
orig_cachepc
		mov	top_of_cache_mem, base_of_cache_mem
orig_pb
		add	top_of_cache_mem, ##CACHE_SIZE
		''
		'' initialize the cache
		''
		call	#reinit_cache
		'' and go do everything
		jmp	#start_running

		'' address for getcnt() routine
io_timer_address           long $80000100

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
		
		muxz	zpu_load_pat, basic_pat1_compile
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
		muxz	zpu_storeh_pat, basic_pat1_compile
		cmps	tos, zpu_lt_compile wcz
		cmps	tos, zpu_le_compile wcz
		cmp	tos, zpu_ltu_compile wcz
		cmp	tos, zpu_leu_compile wcz
		muxz	zpu_swap_pat, basic_pat1_compile
		long	zpu_mult_compile
		shr	tos, zpu_math_compile
shl_pat		shl	tos, zpu_math_compile
		sar	tos, zpu_math_compile
		long	zpu_call_compile
		long	zpu_eq_compile
		long	zpu_ne_compile

instr3x_table
		muxz	zpu_neg_pat, basic_pat1_compile
		sub	tos, zpu_math_compile
		xor	tos, zpu_math_compile
		rdbyte	tos, zpu_load_compile
		
		muxz	zpu_storeb_pat, basic_pat1_compile
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

end_cacheline_pat
		loc	pb, #\0-0
		calld	pa, #\trampoline_set_pc
		
locpa_and_tos_pat
locpa_pat
		loc	pa, #\0-0
		mov	tos, pa

direct_branch_pat
		calld	pa, #\trampoline_set_pc
		
indirect_branch_pat
		add	pb, temp
		jmp	#\set_pc
plain_jmp_pat
		jmp	#\$abcd
		
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
		call	#\@@@runtime_break

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

loadsp_N_pat
		wrlong	tos, ptrb--
		rdlong	tos, ptrb[1]
loadsp_pat
		mov	temp, #0-0
		add	temp, ptrb
		wrlong	tos, ptrb--
		rdlong	tos, temp
storesp_N_pat
		wrlong	tos, ptrb[1]
		rdlong	tos,++ptrb
storesp_pat
		mov	temp, #0-0
		add	temp, ptrb
		wrlong	tos, temp
		rdlong	tos, ++ptrb

set_gt_pat
		mov	tos, #1
   if_le	mov	tos, #0

set_ge_pat
		mov	tos, #1
   if_lt	mov	tos, #0

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
		call	#\@@@runtime_do_div
zpu_mod_pat
		call	#\@@@runtime_do_mod

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
zpu_storeh_pat
		call	#\runtime_storeh
zpu_storeb_pat
		call	#\runtime_storeb
zpu_load_pat
		call	#\runtime_load

zpu_call_pat
		mov	pb, tos
		loc	pa, #\0
		mov	tos, pa
		
debug_info long 0

		'''''''''''''''''''''''''''''''''''''''''''''
		''  runtime support code: must be present while
		''  compiled code is running
		'''''''''''''''''''''''''''''''''''''''''''''
runtime_store
			rdlong	data, ++ptrb
			mov	memp, tos wc
			rdlong	tos, ++ptrb
		if_c	jmp	#write_long_zpu		' write special address
			add	memp, zpu_memory_addr
		_ret_	wrlong	data, memp

runtime_storeh
			rdlong	data, ++ptrb
			mov	memp, tos wc
			rdlong	tos, ++ptrb
		if_c	jmp	#write_long_zpu		' write special address
			add	memp, zpu_memory_addr
		_ret_	wrword	data, memp

runtime_storeb
			rdlong	data, ++ptrb
			mov	memp, tos wc
			rdlong	tos, ++ptrb
		if_c	jmp	#write_long_zpu		' write special address
			add	memp, zpu_memory_addr
		_ret_	wrbyte	data, memp

runtime_load
                        mov     memp, tos wc
	if_c		jmp	#read_zpu_tos	' special ZPU address emulation
			add	memp, zpu_memory_addr
        _ret_           rdlong  tos, memp

	
			
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

'Read a LONG from ZPU memory at "address" into "tos"

read_zpu_tos
read_cog_long           cmp     memp, zpu_cog_start wc  'Check for COG memory access
              if_c      jmp     #read_io_long

                        shr     memp, #2
                        alts    memp, #0
     _ret_              mov     tos, 0-0

read_io_long            cmp     memp, io_timer_address wz 'Check for timer read
	      if_nz	jmp	#read_other
              _ret_     getct   tos

read_other                                                      'Must be other I/O address
                        wrlong  memp, io_port_addr 'Set port address
                        mov     temp, #io_cmd_in      'Set I/O command to IN
                        wrlong  temp, io_command_addr
.wait                   rdlong  temp, io_command_addr wz 'Wait for command to be completed
              if_nz     jmp     #.wait
	      _ret_     rdlong  tos, io_data_addr    'Get the port data

		'''''''''''''''''''''''''''''''''''''''''''''
		'' main interpreter entry point:
		'''''''''''''''''''''''''''''''''''''''''''''
start_running		
		''
		'' jump to address in pb (which is already adjusted to HUB)
		''
set_pc
		mov	pa, #4		' source for destination is not known
trampoline_set_pc

#ifdef DEBUG
		mov	debug_info, cachepc
		call	#\@@@runtime_break		' DEBUG CODE
#endif

		'' check here for a cache hit
		getbyte	opdata, pb, #0
		rdlut	temp2, opdata		' fetch start of trace
		cmp	temp2, pb wz
	if_nz	jmp	#cache_miss		' if not in cache, recompile

cache_hit
		'' if a cache hit, just load the cached address
		'' into orig_cachepc
		add	opdata, #$100
		rdlut	orig_cachepc, opdata

		'' if pa is > 0, then it's a return address from
		'' the instruction that came here; fix that instruction
		'' up
		'' BEWARE: pa can come in with some high bits set
		'' so don't rely on those bits
		sub	pa, #4 wz
	if_z	jmp	#goto_cache

		'' OK, pa points at the instruction to fix up now
		'' be careful to copy over the condition bits too
		rdlong	condition, pa      ' fetch original
		and	condition, cond_mask
		mov	opcode, plain_jmp_pat
		andn	opcode, cond_mask
		andn   	opcode, loc_mask
		or     	opcode, orig_cachepc
		or     	opcode, condition
		wrlong 	opcode, pa
#ifdef DEBUG_TRAMPOLINE
		mov	debug_info, pb
		call	#\@@@runtime_break		' DEBUG CODE
#endif
goto_cache
		jmp	orig_cachepc+0		' the +0 suppresses warnings about missing #

cache_miss
		' OK, we got a cache miss here
		' so we have to compile a new trace

		' if the cache is full, flush it
		mov	temp2, top_of_cache_mem
		subs	temp2, cachepc wcz

    if_be	call	#reinit_cache

    		wrlut	pb, opdata		' update cached pc
		mov	orig_cachepc, cachepc	 ' save the starting cachepc
		mov  	orig_pb, pb   		 ' save starting pb address
		add	opdata, #$100
		wrlut	orig_cachepc, opdata

		mov	trace_flags, #0
compile_loop
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

'done_instruction

		' are we finishing the trace?
		test	trace_flags, #1 wc
	if_c	jmp	#close_trace

		' is there room for another instruction?
		' if not, close out the cache line
		mov	temp, top_of_cache_mem
		subs	temp, cachepc wcz
	if_a	jmp	#compile_loop

close_trace
		' emit a loc instruction to finish the trace
		andn	end_cacheline_pat, loc_mask
		or     	end_cacheline_pat, pb
		mov	opptr, #end_cacheline_pat
		call   	#emit2

		' OK, all done compiling
		jmp	orig_cachepc+0

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
		' optimize multiplies by 4
		tjz	pendingImm, #do_qmul
		cmp	immval, #4 wz
	if_z	mov	immval, #2
	if_z	jmp	#make_shift
		cmp	immval, #2 wz
	if_z	mov	immval, #1
	if_z	jmp	#make_shift
do_qmul
		mov	opcode, qmul_pat
		call	#compile_op_tos_or_imm
		mov	opptr, #getqx_pat
		jmp	#emit1
make_shift
		mov	opcode, shl_pat
		jmp	#compile_op_tos_or_imm
		
		'' compile newtos < tos
		'' which is equivalent to (tos > newtos)
		'' opdata has "cmp" or "cmps" as appropriate
zpu_lt_compile
zpu_ltu_compile
		mov	opcode, opdata
		call	#compile_op_tos_or_imm
		mov	opptr, #set_gt_pat
		jmp	#emit2

		' newtos <= tos
		' is the same as (tos >= newtos)
zpu_le_compile
zpu_leu_compile
		mov	opcode, opdata
		call	#compile_op_tos_or_imm
		mov	opptr, #set_ge_pat
		jmp	#emit2

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
		xor	pa, #$10 wz		' weird that we need this
	if_z	jmp	#loadsp_0

		'' for indices 0..14 (0..56) we can do
		''    wrlong tos,ptrb--
		''    rdlong tos, ptrb[N+1]
		cmp	pa, #14 wcz
	if_ae	jmp	#\@@@hub_compile_big_loadsp
		add	pa, #1
		andn	loadsp_N_pat+1, #$F
		or	loadsp_N_pat+1, pa
		mov	opptr, #loadsp_N_pat
		jmp	#emit2
loadsp_0
		mov	opptr, #pushtos_pat
		jmp	#emit1
				
zpu_storesp_compile
		call	#push_if_imm
		and	pa, #$1F
		xor	pa, #$10 wz
	if_z	jmp	#do_storesp_0

		'' for indices 0..15 (bytes 0..56) we can do
		''    wrlong tos,ptrb[N+1]
		''    rdlong tos, ++ptrb
		cmp	pa, #15 wcz
	if_ae	jmp	#\@@@hub_compile_big_storesp
		mov	opptr, #storesp_N_pat
		andn	storesp_N_pat, #$F
		or	storesp_N_pat, pa
		jmp	#emit2
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
		' if_x jmp \#set_pc
compile_uncond_branch
		mov	condition, cond_mask
		'' fall through
compile_cond_branch
		test	t2, #1 wz ' check for absolute vs. relative
	if_z	mov	temp, zpu_memory_addr
	if_nz	mov	temp, pb
	if_nz	sub	temp, #1

		test	t2, #2 wz ' check for direct (immval good) vs. indirect
	if_z	jmp	#unknown_br_target
		add	temp, immval
		mov	opptr, #direct_branch_pat
		mov	emitcnt, #1
		' fix up the condition on the ret
		andn	direct_branch_pat, cond_mask
		or	direct_branch_pat, condition

		jmp	#common_br_target
unknown_br_target
		mov	opptr, #indirect_branch_pat
		mov	emitcnt, #2
		' fix up the condition on the ret
		andn	indirect_branch_pat+1, cond_mask
		or	indirect_branch_pat+1, condition

common_br_target
		mov	opcode, locpb_pat
		or	opcode, temp
		call	#emit_opcode
		

		or	trace_flags, #1
		jmp	#emit

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
	rep	@.endloop, #256
	wrlut	temp, temp2
	add	temp2, #1
.endloop
_ret_	mov	cachepc, base_of_cache_mem
	
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

cache_space_avail
		res	1
pendingImm
		res	1		' if non-zero, immval holds an immediate which needs to be dealt with
immval
		res	1		' immediate to apply to instruction, if pendingImm is non-zero

		fit	$1f0

		orgh
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
                        mov     memp, #io_cmd_break     'Set I/O command to BREAK
                        wrlong  memp, io_command_addr
.wait                   rdlong  memp, io_command_addr wz
              if_nz     jmp     #.wait
	                ret

			'' calculate tos / data on stack
runtime_do_mod
			mov	temp2, #1	' flag for remainder
			jmp	#do_divmod
runtime_do_div
			mov	temp2, #2	' flag for division
do_divmod
			rdlong data, ++ptrb wz
		if_z	jmp    #@@@div_zero_error
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

			''
			'' compile loadsp
			'' this is a frequent operation, so it's worth optimizing it a bit
			''
			
hub_compile_big_loadsp
		'' 
		'' now compile
		'' mov temp, #pa
		'' add temp, ptrb
		'' wrlong tos, ptrb--
		'' rdlong tos, temp
		''
		shl	pa, #2
		sets	loadsp_pat, pa
		mov	opptr, #loadsp_pat
		jmp	#emit4
		
hub_compile_big_storesp
		shl	pa, #2 wz
	if_z	jmp	#do_storesp_0
		sets	storesp_pat, pa
		mov	opptr, #storesp_pat
		jmp	#emit4
