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
sys.path.append(dig_com_path + "/rs/sim/tests")
sys.path.append(coco_path)
sys.path.append(rs_chisel)
sys.path.append(rs_chisel+"/coco_sim")
sys.path.append(rs_chisel+"/coco_sim/test")

# Import modules
import cocotb
from cocotb.runner import get_runner

# Load modules we nee
coco_path = get_path('COCO_PATH')
sys.path.append(coco_path)

from rs_param import *

# Import test
from gf_poly_eval_test import GfPolyEvalTest

@cocotb.test()
async def poly_cover(dut):
    
    ###################################################
    # Generate stimulus
    ###################################################
    for i in range(1, T_LEN):
        print(f"[POLY_COVER_TEST] corrupts = {i}")
        corrupts = i
        test = GfPolyEvalTest(dut, corrupts)
        test.set_if()
        test.build_env()
        test.gen_stimilus()
        await test.run()
        test.post_run()
        
def gf_poly_eval_tb():

    test_module = "gf_poly_eval_tb"
    hdl_toplevel = "GfPolyEval"
    hdl_toplevel_lang = os.getenv("HDL_TOPLEVEL_LANG", "verilog")
    sim = os.getenv("SIM", "verilator")

    proj_path = Path(__file__).resolve().parent.parent
    print(f"proj_path: {proj_path}")

    verilog_sources = []
    includes        = []
    f_file          = []
    
    verilog_sources.append(proj_path / "GfPolyEval.sv")
    
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
    #os.environ["RANDOM_SEED"] = '1706722271'
    gf_poly_eval_tb()



    

        
