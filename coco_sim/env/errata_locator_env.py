import random
import cocotb
from cocotb.triggers import RisingEdge
from cocotb.triggers import FallingEdge

from cocotb.triggers import Timer

from rs_param import *
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
from rs_lib import RsErrLocatorPacket
from rs_lib import RsErrValuePacket
from rs_lib import RsErrataLocatorPacket

from rs import init_tables
from rs import gf_poly_eval
from rs import gf_pow
from rs import rs_calc_syndromes
from rs import rs_find_error_locator

class ErrataLocatorEnv():
    
    def __init__(self, dut, corrupts = 1):
        self.dut = dut
        self.drv_pkt = []
        self.prd_pkt = []
        
    def set_if(self):
        
        # Connect DUT
        self.clock = self.dut.clock
        self.reset = self.dut.reset
        
        # Connect Error locator IF
        self.s_data = []
        for i in range(T_LEN):
            self.s_data.append(eval(f"self.dut.io_posArray_pos_{i}"))
        self.s_if = AxisIf(aclk=self.clock,
                           tdata=self.s_data,
                           tvalid=self.dut.io_posArray_valid,
                           tkeep=self.dut.io_posArray_sel,
                           unpack='chisel_vec',
                           width=T_LEN,
                           tkeep_type='packed')
        
        self.m_data = []
        for i in range(T_LEN+1):
            self.m_data.append(eval(f"self.dut.io_errataLoc_bits_errataLoc_{i}"))
            
        self.m_if = AxisIf(aclk=self.clock,
                           tdata=self.m_data,
                           tvalid=self.dut.io_errataLoc_valid,
                           tlast=self.dut.io_errataLoc_valid,
                           #tkeep=self.m_tkeep,
                           unpack='chisel_vec',
                           width=T_LEN+1,
                           tkeep_type='packed')
        
    def build_env(self):
        self.comp = Comparator(name='comparator')
        self.s_drv = AxisDriver(name='s_drv', axis_if=self.s_if)
        self.m_mon = AxisMonitor(name='m_mon', axis_if=self.m_if, aport=self.comp.port_out)

        
    def gen_stimilus(self, ref_obj):
        for i in range(len(ref_obj.cor_msg)):
            cor_msg = ref_obj.cor_msg[i]
            syndrome = rs_calc_syndromes(cor_msg.data, ROOTS_NUM)
            self.error_locator = rs_find_error_locator(syndrome, ROOTS_NUM)[::-1]
            
            # Error position packet
            err_pos = RsErrPositionPacket(name=f'err_pos-{i}', n_len=N_LEN, roots_num=ROOTS_NUM)
            err_pos.rs_gen_data(msg=cor_msg.data)        
            err_pos.print_pkt()
            err_pos.gen_delay(delay=None, delay_type='no_delay')
            self.drv_pkt.append(err_pos)

            # Error value packet
            errata_loc = RsErrataLocatorPacket(name=f'errata_loc-{i}', n_len=N_LEN, roots_num=ROOTS_NUM)
            errata_loc.rs_gen_data(msg=cor_msg.data)        
            errata_loc.print_pkt()
            self.comp.port_prd.append(errata_loc)
        
                    
    async def run(self):
        await cocotb.start(reset_dut(self.reset,200,1))
        await Timer(50, units = "ns")

        await cocotb.start(custom_clock(self.clock, 10))
        await cocotb.start(self.m_mon.mon_if())
        
        await FallingEdge(self.reset)
        for i in range(10):
            await RisingEdge(self.clock)
            
        # Set local error locator
        for pkt in self.drv_pkt:
            await self.s_drv.send_pkt(pkt)
            for i in range(CHIEN__CYCLES_NUM+5):
                await RisingEdge(self.clock)
        
    def post_run(self):
        print("post_run()")
        self.comp.compare()

