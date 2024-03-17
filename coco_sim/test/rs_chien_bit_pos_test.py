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

class RsChienGetBitPos(RsTest):
    
    def __init__(self, dut, corrupts = 1):
        super().__init__(dut)
        self.corrupts = corrupts
        self.error_locator_if = []
        self.error_locator =[]
        self.error_locator_sel = None
        self.error_locator_vld = None
        self.bit_pos=[]
        
    def set_if(self):
        
        # Connect DUT
        self.clock    = self.dut.clock
        self.reset = self.dut.reset
        
        # Connect Error locator IF
        for i in range(T_LEN):
            self.error_locator_if.append(eval(f"self.dut.io_errLocatorIf_bits_errLocator_{i}"))
        self.error_locator_sel = self.dut.io_errLocatorIf_bits_errLocatorSel
        self.error_locator_vld = self.dut.io_errLocatorIf_valid
        
        self.m_if = AxisIf(aclk=self.clock,
                           tdata=self.dut.io_bitPos_bits,
                           tvalid=self.dut.io_bitPos_valid,
                           unpack=0,
                           width=2)

        
    def build_env(self):
        self.m_mon = AxisMonitor(name='m_mon', axis_if=self.m_if)
        self.comp = Comparator(name='comparator')
        
    def gen_stimilus(self):
        init_tables()

        ref_msg = Packet(name=f'ref_msg')
        ref_msg.generate(K_LEN)
        
        enc_msg = RsPacket(name=f'enc_msg', n_len=N_LEN, roots_num=ROOTS_NUM)
        enc_msg.rs_gen_data(msg=ref_msg.data)
        
        #Create cor_msg based on the same ref_msg
        cor_msg = RsPacket(name=f'cor_msg', n_len=N_LEN, roots_num=ROOTS_NUM)
        cor_msg.rs_gen_data(msg=ref_msg.data)
        cor_msg.corrupt_pkt(self.corrupts)        
        cor_msg.compare(enc_msg)

        syndrome = rs_calc_syndromes(cor_msg.data, ROOTS_NUM)
        self.error_locator = rs_find_error_locator(syndrome, ROOTS_NUM)[::-1]
        
        self.prd_pkt = RsErrBitPositionPacket(name=f'bit_pos', n_len=N_LEN, roots_num=ROOTS_NUM)
        self.prd_pkt.rs_gen_data(msg=cor_msg.data)        
    async def run(self):
        await cocotb.start(reset_dut(self.reset,200,1))
        await Timer(50, units = "ns")

        await cocotb.start(custom_clock(self.clock, 10))
        await cocotb.start(self.m_mon.mon_if())
        
        await FallingEdge(self.reset)
        for i in range(10):
            await RisingEdge(self.clock)
            
        # Set local error locator
        for i in range(len(self.error_locator)-1):
            print(f"error_locator[{i}] = {self.error_locator[i+1]}")
            self.error_locator_if[i].value = self.error_locator[i+1]

        error_locator_sel = 0    
        for i in range (len(self.error_locator)-1):
            error_locator_sel = error_locator_sel | 1 << i
        self.error_locator_sel.value = error_locator_sel
        self.error_locator_vld.value = 1
        await RisingEdge(self.clock)
        self.error_locator_vld.value = 0
        for i in range(CHIEN__CYCLES_NUM+4):
            await RisingEdge(self.clock)
        
    def post_run(self):
        print("post_run()")
        out_pkt = Packet(name='out_pkt')
        print(f"self.m_mon.pkt_size = {self.m_mon.pkt_size}")
        out_pkt.pkt_size = self.m_mon.pkt_size
        out_pkt.write_word_list(self.m_mon.data, self.m_mon.pkt_size, self.m_mon.width, 1)        
        out_pkt.print_pkt()
        self.prd_pkt.print_pkt()
        self.comp.port_prd.append(self.prd_pkt)
        self.comp.port_out.append(out_pkt)
        self.comp.compare()
