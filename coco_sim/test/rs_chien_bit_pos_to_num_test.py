import random
import cocotb
from cocotb.triggers import RisingEdge
from cocotb.triggers import FallingEdge

from cocotb.triggers import Timer

from rs_param import *
from rs_test import RsTest
from coco_axis.axis import AxisIf
from coco_axis.axis import AxisDriver
from coco_axis.axis import AxisResponder
from coco_axis.axis import AxisMonitor
from coco_axis.axis import AxisIf
from coco_env.scoreboard import Comparator

from coco_env.stimulus import reset_dut
from coco_env.stimulus import custom_clock
from coco_env.packet import Packet

from rs_lib import RsPacket
from rs_lib import RsErrBitPositionPacket
from rs_lib import RsErrPositionPacket

from rs import init_tables
from rs import gf_poly_eval
from rs import gf_pow
from rs import rs_calc_syndromes
from rs import rs_find_error_locator

class RsChienBitPosToNumTest(RsTest):
    
    def __init__(self, dut, pkt_num = T_LEN):
        super().__init__(dut)
        self.drv_pkt = []
        self.prd_pkt = []
        
    def set_if(self):
        
        # Connect DUT
        self.clock = self.dut.clock
        self.reset = self.dut.reset
        
        # Connect Error locator IF
        self.s_if = AxisIf(aclk=self.clock,
                           tdata=self.dut.io_bitPos_pos,
                           tlast=self.dut.io_bitPos_last,
                           tvalid=self.dut.io_bitPos_valid,
                           unpack='packed',
                           width=2)

        self.m_data = []
        self.m_tkeep = []
        for i in range(T_LEN):
            self.m_data.append(eval(f"self.dut.io_numArray_pos_{i}"))
            self.m_tkeep.append(eval(f"self.dut.io_numArray_sel_{i}"))
            
        self.m_if = AxisIf(aclk=self.clock,
                           tdata=self.m_data,
                           tvalid=self.dut.io_numArray_valid,
                           tlast=self.dut.io_numArray_valid,
                           tkeep=self.m_tkeep,
                           unpack='chisel_vec',
                           width=T_LEN,
                           tkeep_type='chisel_vec')

        
    def build_env(self):
        self.s_drv = AxisDriver(name='s_drv', axis_if=self.s_if)
        self.comp = Comparator(name='comparator')
        self.m_mon = AxisMonitor(name=f'm_mon', axis_if=self.m_if, aport=self.comp.port_out)
        
    def gen_stimilus(self):        
        init_tables()
        for i in range(1, T_LEN+1):
            ref_msg = Packet(name=f'ref_msg')
            ref_msg.generate(K_LEN)
        
            enc_msg = RsPacket(name=f'enc_msg', n_len=N_LEN, roots_num=ROOTS_NUM)
            enc_msg.rs_gen_data(msg=ref_msg.data)
            
            #Create cor_msg based on the same ref_msg
            cor_msg = RsPacket(name=f'cor_msg', n_len=N_LEN, roots_num=ROOTS_NUM)
            cor_msg.rs_gen_data(msg=ref_msg.data)
            cor_msg.corrupt_pkt(i)        
            cor_msg.compare(enc_msg)
            #cor_msg.print_pkt()
    
            # Bit positions packet        
            bit_pos = RsErrBitPositionPacket(name=f'bit_pos-{i}', n_len=N_LEN, roots_num=ROOTS_NUM)
            bit_pos.rs_gen_data(msg=cor_msg.data)
            bit_pos.print_pkt()
            bit_pos.gen_delay(delay=None, delay_type='no_delay')
            self.drv_pkt.append(bit_pos)
            
            # Error position packet
            err_pos = RsErrPositionPacket(name=f'err_pos-{i}', n_len=N_LEN, roots_num=ROOTS_NUM)
            err_pos.rs_gen_data(msg=cor_msg.data)        
            err_pos.print_pkt()
            err_pos.gen_delay(delay=None, delay_type='no_delay')
            self.comp.port_prd.append(err_pos)
        
    async def run(self):
        await cocotb.start(reset_dut(self.reset,200,1))
        await Timer(50, units = "ns")
        
        clock_proc = await cocotb.start(custom_clock(self.clock, 10))
        mon_proc = await cocotb.start(self.m_mon.mon_if())
        
        await FallingEdge(self.reset)
        for i in range(10):
            await RisingEdge(self.clock)
        for pkt in self.drv_pkt:
            await self.s_drv.send_pkt(pkt)
            for i in range(CHIEN__CYCLES_NUM+5):
                await RisingEdge(self.clock)
                
    def post_run(self):
        print("post_run()")        
        self.comp.compare()

class RsChienCorruptsInRawStart(RsChienBitPosToNumTest):

    def gen_stimilus(self):
        init_tables()
        if CHIEN__CYCLES_NUM < 3:
            test_range = N_LEN-T_LEN
        else:
            test_range = CHIEN__ROOTS_PER_CYCLE
        
        for i in range(test_range):
            corrupts = []
            for k in range (T_LEN):
                corrupts.append(i+k)
            ref_msg = Packet(name=f'ref_msg')
            ref_msg.generate(K_LEN)
        
            enc_msg = RsPacket(name=f'enc_msg', n_len=N_LEN, roots_num=ROOTS_NUM)
            enc_msg.rs_gen_data(msg=ref_msg.data)
            
            #Create cor_msg based on the same ref_msg
            cor_msg = RsPacket(name=f'cor_msg', n_len=N_LEN, roots_num=ROOTS_NUM)
            cor_msg.rs_gen_data(msg=ref_msg.data)
            cor_msg.corrupt_pkt(corrupts)        
            cor_msg.compare(enc_msg)
            #cor_msg.print_pkt()
    
            # Bit positions packet        
            bit_pos = RsErrBitPositionPacket(name=f'bit_pos-{i}', n_len=N_LEN, roots_num=ROOTS_NUM)
            bit_pos.rs_gen_data(msg=cor_msg.data)        
            bit_pos.print_pkt()
            bit_pos.gen_delay(delay=None, delay_type='no_delay')
            self.drv_pkt.append(bit_pos)
            
            # Error position packet
            err_pos = RsErrPositionPacket(name=f'err_pos-{i}', n_len=N_LEN, roots_num=ROOTS_NUM)
            err_pos.rs_gen_data(msg=cor_msg.data)        
            err_pos.print_pkt()
            err_pos.gen_delay(delay=None, delay_type='no_delay')
            self.comp.port_prd.append(err_pos)

class RsChienCorruptsInRawStop(RsChienBitPosToNumTest):

    def gen_stimilus(self):
        init_tables()
        if CHIEN__CYCLES_NUM < 3:
            test_range = N_LEN-T_LEN
        else:
            test_range = CHIEN__ROOTS_PER_CYCLE
        
        for i in range(test_range):
            corrupts = []
            for k in range (T_LEN):
                corrupts.append(N_LEN-T_LEN-i-k)
            ref_msg = Packet(name=f'ref_msg')
            ref_msg.generate(K_LEN)
        
            enc_msg = RsPacket(name=f'enc_msg', n_len=N_LEN, roots_num=ROOTS_NUM)
            enc_msg.rs_gen_data(msg=ref_msg.data)
            
            #Create cor_msg based on the same ref_msg
            cor_msg = RsPacket(name=f'cor_msg', n_len=N_LEN, roots_num=ROOTS_NUM)
            cor_msg.rs_gen_data(msg=ref_msg.data)
            cor_msg.corrupt_pkt(corrupts)        
            cor_msg.compare(enc_msg)
            #cor_msg.print_pkt()
    
            # Bit positions packet        
            bit_pos = RsErrBitPositionPacket(name=f'bit_pos-{i}', n_len=N_LEN, roots_num=ROOTS_NUM)
            bit_pos.rs_gen_data(msg=cor_msg.data)        
            bit_pos.print_pkt()
            bit_pos.gen_delay(delay=None, delay_type='no_delay')
            self.drv_pkt.append(bit_pos)
            
            # Error position packet
            err_pos = RsErrPositionPacket(name=f'err_pos-{i}', n_len=N_LEN, roots_num=ROOTS_NUM)
            err_pos.rs_gen_data(msg=cor_msg.data)        
            err_pos.print_pkt()
            err_pos.gen_delay(delay=None, delay_type='no_delay')
            self.comp.port_prd.append(err_pos)
