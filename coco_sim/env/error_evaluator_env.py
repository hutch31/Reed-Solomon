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
from rs_lib import RsSyndromePacket
from rs_lib import RsErrataLocatorPacket
from rs_lib import RsSyndXErrataPacket
from rs_lib import RsErrValuePacket

from rs import init_tables
from rs import gf_poly_eval
from rs import gf_pow
from rs import rs_calc_syndromes
from rs import rs_find_error_locator

class ErrataLocatorEnv():
    
    def __init__(self, dut, corrupts = 1):
        self.dut = dut
        self.errata_pkt = []
        self.synd_pkt = []
        self.prd_pkt = []
        
    def set_if(self):
        
        # Connect DUT
        self.clock = self.dut.clock
        self.reset = self.dut.reset
        
        # Connect Error locator IF
        self.s_data0 = []
        for i in range(T_LEN+1):
            self.s_data0.append(eval(f"self.dut.io_errataLoc_bits_{i}"))
        self.s_if0 = AxisIf(aclk=self.clock,
                            tdata=self.s_data0,
                            tvalid=self.dut.io_errataLoc_valid,
                            unpack='chisel_vec',
                            width=T_LEN+1)

        self.s_data1 = []
        for i in range(ROOTS_NUM):
            self.s_data1.append(eval(f"self.dut.io_syndrome_{i}"))
        self.s_if1 = AxisIf(aclk=self.clock,
                            tdata=self.s_data1,
                            unpack='chisel_vec',
                            width=ROOTS_NUM)        
        
        self.m_data = []
        for i in range(ROOTS_NUM+T_LEN):
            self.m_data.append(eval(f"self.dut.io_syndXErrataLoc_bits_{i}"))
            
        self.m_if = AxisIf(aclk=self.clock,
                           tdata=self.m_data,
                           tvalid=self.dut.io_syndXErrataLoc_valid,
                           tlast=self.dut.io_syndXErrataLoc_valid,
                           unpack='chisel_vec',
                           width=ROOTS_NUM+T_LEN)
        
    def build_env(self):
        self.comp = Comparator(name='comparator')
        self.err_loc_drv = AxisDriver(name='err_loc_drv', axis_if=self.s_if0)
        self.synd_drv = AxisDriver(name='synd_drv', axis_if=self.s_if1, msb_first=1)
        self.m_mon = AxisMonitor(name='m_mon', axis_if=self.m_if, aport=self.comp.port_out)

        
    def gen_stimilus(self, ref_obj):
        for i in range(len(ref_obj.cor_msg)):
            cor_msg = ref_obj.cor_msg[i]
            # Syndrome pkt
            synd_pkt = RsSyndromePacket(name=f'synd_pkt-{i}', n_len=N_LEN, roots_num=ROOTS_NUM)
            synd_pkt.rs_gen_data(msg=cor_msg.data)        
            synd_pkt.print_pkt()
            synd_pkt.gen_delay(delay=None, delay_type='no_delay')
            self.synd_pkt.append(synd_pkt)                        
            
            # Error value packet
            errata_loc = RsErrataLocatorPacket(name=f'errata_loc-{i}', n_len=N_LEN, roots_num=ROOTS_NUM)
            errata_loc.rs_gen_data(msg=cor_msg.data)        
            errata_loc.print_pkt()
            #self.comp.port_prd.append(errata_loc)
            self.errata_pkt.append(errata_loc)
            
            synd_x_errata = RsSyndXErrataPacket(name=f'synd_x_errata-{i}', n_len=N_LEN, roots_num=ROOTS_NUM)
            synd_x_errata.rs_gen_data(msg=cor_msg.data)        
            synd_x_errata.print_pkt()
            self.comp.port_prd.append(synd_x_errata)
            #self.errata_pkt.append(errata_loc)

                    
    async def run(self):
        await cocotb.start(reset_dut(self.reset,200,1))
        await Timer(50, units = "ns")

        await cocotb.start(custom_clock(self.clock, 10))
        await cocotb.start(self.m_mon.mon_if())
        
        await FallingEdge(self.reset)
        for i in range(10):
            await RisingEdge(self.clock)
            
        # Set local error locator
        for i in range (len(self.synd_pkt)):
            await self.synd_drv.send_pkt(self.synd_pkt[i])
            await self.err_loc_drv.send_pkt(self.errata_pkt[i])
            for i in range(CHIEN__CYCLES_NUM+5):
                await RisingEdge(self.clock)
        
    def post_run(self):
        print("post_run()")
        self.comp.compare()

