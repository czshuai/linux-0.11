/*
 *  linux/kernel/blk_dev/ll_rw.c
 *
 * (C) 1991 Linus Torvalds
 */

/*
 * This handles all read/write requests to block devices
 */
#include <errno.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <asm/system.h>

#include "blk.h"

/*
 * The request-struct contains all necessary data
 * to load a nr of sectors into memory
 */
struct request request[NR_REQUEST];

/*
 * used to wait on when there are no free requests
 */
struct task_struct * wait_for_request = NULL;

/* blk_dev_struct is:
 *	do_request-address
 *	next-request
 */
struct blk_dev_struct blk_dev[NR_BLK_DEV] = {
	{ NULL, NULL },		/* no_dev */
	{ NULL, NULL },		/* dev mem */
	{ NULL, NULL },		/* dev fd */
	{ NULL, NULL },		/* dev hd */
	{ NULL, NULL },		/* dev ttyx */
	{ NULL, NULL },		/* dev tty */
	{ NULL, NULL }		/* dev lp */
};

static inline void lock_buffer(struct buffer_head * bh)
{
	cli();//@@在bh->b_lock=1的执行过程中被中断，可能导致两个进程上锁操作。
	while (bh->b_lock)
		sleep_on(&bh->b_wait);
	bh->b_lock=1;
	sti();
}

static inline void unlock_buffer(struct buffer_head * bh)
{
	if (!bh->b_lock)
		printk("ll_rw_block.c: buffer not locked\n\r");
	bh->b_lock = 0;
	wake_up(&bh->b_wait);
}

/*
 * add-request adds a request to the linked list.
 * It disables interrupts so that it can muck with the
 * request-lists in peace.
 */
static void add_request(struct blk_dev_struct * dev, struct request * req)
{
	struct request * tmp;

	req->next = NULL;
	cli();//@@涉及到blk_dev这个全局变量？
	if (req->bh)
		req->bh->b_dirt = 0; //@@将请求项加入队列之前，就将dirt置为0，对应到getblk的BADNESS判断中。
	if (!(tmp = dev->current_request)) {
		dev->current_request = req;
		sti();
		(dev->request_fn)(); //@@do_hd_request初始化时挂接
		return;
	}
	for ( ; tmp->next ; tmp=tmp->next)
		if ((IN_ORDER(tmp,req) ||
		    !IN_ORDER(tmp,tmp->next)) &&
		    IN_ORDER(req,tmp->next))
			break;
	req->next=tmp->next;
	tmp->next=req;   //@@不调用(dev->request_fn)();函数
	sti();
}

//@@找空闲请求项，设置关联
static void make_request(int major,int rw, struct buffer_head * bh)
{
	struct request * req;//@@....野指针 紧接*req = 0;
	int rw_ahead;

/* WRITEA/READA is special case - it is not really needed, so if the */
/* buffer is locked, we just forget about it, else it's a normal read */
	if (rw_ahead = (rw == READA || rw == WRITEA)) {
		if (bh->b_lock)
			return;
		if (rw == READA)//@@预读，读多个块
			rw = READ;
		else
			rw = WRITE;
	}
	if (rw!=READ && rw!=WRITE)
		panic("Bad block dev command, must be R/W/RA/WA");
	lock_buffer(bh);  //@@在缓冲块提出和硬盘同步请求的时候上锁。在end_request请求项执行结束，且将b_uptodate置1后解锁。
	//@@防止竞争 接下来将进行同步操作。为当前缓冲块创建请求项的过程中，b_lock置1.
	//@@这里的lock_buffe是访问遇锁的进程进入调度的位置。
	//@@得到对应的缓冲块按正常流程走，但在试图创建对应请求项，并将缓冲块上锁时。
	//@@遇到缓冲块已上锁，说明缓冲块在同步，因此进入等待队列。
	//@@返回时会进入下面的判断语句，说明缓冲块的同步操作已经完成，不需要再创建请求项。
	//@@所以unlock，返回缓冲块。
	if ((rw == WRITE && !bh->b_dirt) || (rw == READ && bh->b_uptodate)) {
		unlock_buffer(bh); //@@不需要同步操作
		return;
	}
repeat:
/* we don't allow the write-requests to fill up the queue completely:
 * we want some room for reads: they take precedence. The last third
 * of the requests are only for reads.
 */
	if (rw == READ)
		req = request+NR_REQUEST;//@@读的优先级高  32的请求项项数和buffer的项数相关
	else
		req = request+((NR_REQUEST*2)/3);
/* find an empty request */
	while (--req >= request)
		if (req->dev<0)
			break;
/* if none found, sleep on new requests: check for rw_ahead */
	if (req < request) {
		if (rw_ahead) {
			unlock_buffer(bh);
			return;
		}
		sleep_on(&wait_for_request);//@@形成空闲请求项的等待队列
		goto repeat;
	}
/* fill up the request-info, and add it to the queue */
	req->dev = bh->b_dev;
	req->cmd = rw;
	req->errors=0;
	req->sector = bh->b_blocknr<<1; //@@缓冲区数据块大小1024字节和扇区大小512字节大小
	req->nr_sectors = 2; //@@表示两个扇区
	req->buffer = bh->b_data;
	req->waiting = NULL;
	req->bh = bh;
	req->next = NULL;
	add_request(major+blk_dev,req);
}

void ll_rw_block(int rw, struct buffer_head * bh) //@@底层块设备操作
{
	unsigned int major;

	if ((major=MAJOR(bh->b_dev)) >= NR_BLK_DEV ||
	!(blk_dev[major].request_fn)) {
		printk("Trying to read nonexistent block-device\n\r");
		return;
	}
	make_request(major,rw,bh);
}

void blk_dev_init(void)
{
	int i;

	for (i=0 ; i<NR_REQUEST ; i++) {
		request[i].dev = -1;
		request[i].next = NULL;  
	}
}
