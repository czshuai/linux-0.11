/*
 *  linux/kernel/fork.c
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 *  'fork.c' contains the help-routines for the 'fork' system call
 * (see also system_call.s), and some misc functions ('verify_area').
 * Fork is rather simple, once you get the hang of it, but the memory
 * management can be a bitch. See 'mm/mm.c': 'copy_page_tables()'
 */
#include <errno.h>

#include <linux/sched.h>
#include <linux/kernel.h>
#include <asm/segment.h>
#include <asm/system.h>

extern void write_verify(unsigned long address);

long last_pid=0;

void verify_area(void * addr,int size)
{
	unsigned long start;

	start = (unsigned long) addr;
	size += start & 0xfff;
	start &= 0xfffff000;
	start += get_base(current->ldt[2]);
	while (size>0) {
		size -= 4096;
		write_verify(start);
		start += 4096;
	}
}

int copy_mem(int nr,struct task_struct * p) //@@制作线性地址空间
{
	unsigned long old_data_base,new_data_base,data_limit;
	unsigned long old_code_base,new_code_base,code_limit;

	code_limit=get_limit(0x0f); //@@获取父进程的代码段和数据段限长
	data_limit=get_limit(0x17);
	old_code_base = get_base(current->ldt[1]); //@@父进程段基址
	old_data_base = get_base(current->ldt[2]);
	if (old_data_base != old_code_base)
		panic("We don't support separate I&D");
	if (data_limit < code_limit)
		panic("Bad data_limit");
	new_data_base = new_code_base = nr * 0x4000000; //2的26次 64M为单位 将4G空间划分为64块
	p->start_code = new_code_base;
	set_base(p->ldt[1],new_code_base);
	set_base(p->ldt[2],new_data_base); //@@ldt指向申请的线性得知空间
	if (copy_page_tables(old_data_base,new_data_base,data_limit)) { //@@拷贝页 页不同，内容相同 代码的加载是由父进程完成
		free_page_tables(new_data_base,data_limit);
		return -ENOMEM;sign adder_cin = op_sub | op_slt | op_sltu | op_SignSub; 
assign {adder_cout, adder_result} = {(adder_a[31] && (op_SignAdd || op_SignSub)), adder_a} + {(adder_b[31] && (op_SignAdd || op_SignSub)), adder_b} + adder_cin; 
assign ExcepOv = (op_SignAdd || op_SignSub) && ~(adder_cout == adder_result[31]);

assign add_sub_result = adder_result;

assign slt_result[31:1] = 31'b0; 
assign slt_result[0] = (alu_src1[31] & ~alu_src2[31]) | (~(alu_src1[31] ^ alu_src2[31]) & adder_result[31]);

assign sltu_result[31:1] = 31'b0; 
assign sltu_result[0] = ~adder_cout;
	}
	return 0;
}

/*
 *  Ok, this is the main fork-routine. It copies the system process
 * information (task[nr]) and sets up the necessary registers. It
 * also copies the data segment in it's entirety.
 */
int copy_process(int nr,long ebp,long edi,long esi,long gs,long none, //@@nr的值为sys_fork中的%eax
		long ebx,long ecx,long edx,
		long fs,long es,long ds,
		long eip,long cs,long eflags,long esp,long ss) //@@中断压栈的内容 这里通过传参的方法将其取出来  在system_call开始处压入的
{
	struct task_struct *p;
	int i;
	struct file *f;

	p = (struct task_struct *) get_free_page(); //@@从内存中获取一个空白页 得到task_struct 返回的是空闲页的地址
	if (!p)
		return -EAGAIN;
	task[nr] = p; 
	*p = *current;	/* NOTE! this doesn't copy the supervisor stack */ //current指向当前进程t的ask_struct 注意指针的类型，拷贝的是task_struct
	p->state = TASK_UNINTERRUPTIBLE; //@@不可中断
	p->pid = last_pid;
	p->father = current->pid;
	p->counter = p->priority;
	p->signal = 0;
	p->alarm = 0;
	p->leader = 0;		/* process leadership doesn't inherit */
	p->utime = p->stime = 0;
	p->cutime = p->cstime = 0;
	p->start_time = jiffies;
	//@@----------------------------TSS
	p->tss.back_link = 0; 
	p->tss.esp0 = PAGE_SIZE + (long) p; //@@内核栈栈顶
	p->tss.ss0 = 0x10; //@@内核数据段和代码段的限长都为16MB，覆盖整个物理地址空间
	p->tss.eip = eip; //@@用户态EIP 进程0用户态 int $0x80 的下一条指令
	p->tss.eflags = eflags; 
	p->tss.eax = 0; //@@影响fork的返回值，进入init函数，fork函数的返回值
	p->tss.ecx = ecx;
	p->tss.edx = edx;
	p->tss.ebx = ebx;
	p->tss.esp = esp;
	p->tss.ebp = ebp;
	p->tss.esi = esi;
	p->tss.edi = edi; //@@构建通用寄存器的上下文环境
	p->tss.es = es & 0xffff;
	p->tss.cs = cs & 0xffff;
	p->tss.ss = ss & 0xffff;
	p->tss.ds = ds & 0xffff;
	p->tss.fs = fs & 0xffff;
	p->tss.gs = gs & 0xffff; //@@构建段寄存器的上下文环境
	p->tss.ldt = _LDT(nr);
	p->tss.trace_bitmap = 0x80000000; //@@------------------
	if (last_task_used_math == current)
		__asm__("clts ; fnsave %0"::"m" (p->tss.i387));
	if (copy_mem(nr,p)) {
		task[nr] = NULL;
		free_page((long) p);
		return -EAGAIN;
	}
	for (i=0; i<NR_OPEN;i++)
		if (f=p->filp[i])
			f->f_count++;
	if (current->pwd)
		current->pwd->i_count++;
	if (current->root)
		current->root->i_count++;
	if (current->executable)
		current->executable->i_count++;
	set_tss_desc(gdt+(nr<<1)+FIRST_TSS_ENTRY,&(p->tss)); //@@设置进程1在GDT中的内容
	set_ldt_desc(gdt+(nr<<1)+FIRST_LDT_ENTRY,&(p->ldt));
	p->state = TASK_RUNNING;	//@@置于就绪态 /* do this last, just in case */
	return last_pid;
}

int find_empty_process(void) //@@在task数组中遍历找到空闲位
{
	int i;

	repeat:
		if ((++last_pid)<0) last_pid=1;
		for(i=0 ; i<NR_TASKS ; i++) //@@遍历所有的task，判断last_pid有没有被占用，如果被占用，go repeat ++last_pid继续遍历，直到找到未被占用的进程号
			if (task[i] && task[i]->pid == last_pid) goto repeat; //@@获取新的进程号
	for(i=1 ; i<NR_TASKS ; i++)
		if (!task[i])
			return i;   //@@在task中找到空闲的位置
	return -EAGAIN;
}
