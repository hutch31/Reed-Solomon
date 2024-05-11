# Set environment to load modules we need
def get_path(module_path):
    if module_path not in os.environ:
        print(f"[ERROR] {module_path} env var is not set.")
    else:
        return os.getenv(module_path)

import random
import os
import sys
import math
from pathlib import Path

# 
dig_com_path = get_path('DIGITAL_COMMUNICAITON')
coco_path = get_path('COCO_PATH')
rs_chisel = get_path('RS_CHISEL')

sys.path.append(dig_com_path + "/rs/python")
sys.path.append(dig_com_path + "/rs/sim")
sys.path.append(coco_path)
sys.path.append(rs_chisel)
sys.path.append(rs_chisel+"/coco_sim")
sys.path.append(rs_chisel+"/coco_sim/env")

# Import modules
import cocotb
from cocotb.runner import get_runner

# Load modules we need
coco_path = get_path('COCO_PATH')
sys.path.append(coco_path)

from rs_param import *

# Import test
from rs_top_env import RsTopEnv
from ref_msg import *

@cocotb.test()
async def random_error(dut):
    
    ###################################################
    # Generate stimulus
    ###################################################
    print(f"[POLY_COVER_TEST] Vary num of corruption words from 1 up to T_LEN = {T_LEN}")
    stimulus = RandomErrors(pkt_num=100)
    test = RsTopEnv(dut)
    test.set_if()
    test.build_env()
    test.gen_stimilus(stimulus)
    await test.run()
    test.post_run()

@cocotb.test()
async def cover_all_errors(dut):
    
    ###################################################
    # Generate stimulus
    ###################################################
    print(f"[POLY_COVER_TEST] Vary num of corruption words from 1 up to T_LEN = {T_LEN}")
    stimulus = CoverAllErrors()
    test = RsTopEnv(dut)
    test.set_if()
    test.build_env()
    test.gen_stimilus(stimulus)
    await test.run()
    test.post_run()

@cocotb.test()
async def burst_errors(dut):
    
    ###################################################
    # Generate stimulus
    ###################################################
    print(f"[POLY_COVER_TEST] Vary num of corruption words from 1 up to T_LEN = {T_LEN}")
    stimulus = ErrorBurst()
    test = RsTopEnv(dut)
    test.set_if()
    test.build_env()
    test.gen_stimilus(stimulus)
    await test.run()
    test.post_run()

@cocotb.test()
async def static_packet(dut):
    
    stimulus = StaticError()
    test = RsTopEnv(dut)
    test.set_if()
    test.build_env()
    test.gen_stimilus(stimulus)
    await test.run()
    test.post_run()

#@cocotb.test()
#async def uncorrectable_errors(dut):
#    
#    ###################################################
#    # Generate stimulus
#    ###################################################
#    print(f"[UNCORRECTABLE ERROR] Corrupt the block with more than T_LEN errors")
#    stimulus = UncorrectableError()
#    test = RsTopEnv(dut)
#    test.set_if()
#    test.build_env()
#    test.gen_stimilus(stimulus)
#    await test.run()
#    test.post_run()
    
    
def RsTopTb():
    
    test_module = "rs_top_test"
    hdl_toplevel = "RsTop"
    hdl_toplevel_lang = os.getenv("HDL_TOPLEVEL_LANG", "verilog")
    sim = os.getenv("SIM", "verilator")
    
    proj_path = Path(__file__).resolve().parent.parent
    print(f"proj_path: {proj_path}")

    verilog_sources = []
    includes        = []
    f_file          = []
    
    verilog_sources.append(proj_path / "RsTop.sv")
    
    # Parameters    
    parameters = {        
                  }
    
    # Defines    
    defines = {}
    
    runner = get_runner(sim)
    build_args = [ '--timing', '--assert' , '--trace' , '--trace-structs', '--trace-max-array', '512', '--trace-max-width', '512']

    runner.build(
        defines=defines,
        parameters=parameters,
        verilog_sources=verilog_sources,
        includes=includes,
        hdl_toplevel=hdl_toplevel,
        build_args=build_args,
        always=True,
    )
    
    runner.test(hdl_toplevel=hdl_toplevel,
                test_module=test_module,
                )


if __name__ == "__main__":
    os.environ["RANDOM_SEED"] = '1715370565'
    RsTopTb()
