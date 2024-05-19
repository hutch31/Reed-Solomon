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
from rs_cfg import *
import cocotb
from cocotb.runner import get_runner
import argparse

dig_com_path = get_path('DIGITAL_COMMUNICAITON')
coco_path = get_path('COCO_PATH')
rs_chisel = get_path('RS_CHISEL')
coco_path = get_path('COCO_PATH')

sys.path.append(dig_com_path + "/rs/python")
sys.path.append(dig_com_path + "/rs/sim")
sys.path.append(coco_path)
sys.path.append(rs_chisel)
sys.path.append(rs_chisel+"/coco_sim")
sys.path.append(rs_chisel+"/coco_sim/env")
sys.path.append(coco_path)

# All tests are lacoted here:
from rs_testcases import *
# Parse CMD arguments
def parse_command_line():
    
    args = argparse.ArgumentParser(add_help=True)
    
    args.add_argument("-l", "--hdl_toplevel",
                      dest="hdl_toplevel",
                      required=True,
                      help='Set hdl top level. For Example RsSynd, RsBm, RsChien, RsForney, RsDecTop.'
                      )
    
    args.add_argument("-t", "--testcase",
                      dest="testcase",
                      required=False,
                      default=None,
                      help='Set testcases you want to run. Check available test in rs_testcases.py.'
                      )

    args.add_argument("-s", "--seed",
                      dest="seed",
                      required=False,
                      default=None,
                      help='Set run seed.'
                      )
    
    return args.parse_args()    
    
def build_and_run(args):

    cfg = cfg_dict[args.hdl_toplevel]
    if not cfg:
        raise ValueError(cfg)
    runner = get_runner(cfg['sim'])

    runner.build(
        defines=cfg['defines'],
        parameters=cfg['parameters'],
        verilog_sources=cfg['verilog_sources'],
        includes=cfg['includes'],
        hdl_toplevel=cfg['hdl_toplevel'],
        build_args=cfg['build_args'],
        always=True,
    )
    
    runner.test(hdl_toplevel=cfg['hdl_toplevel'],
                test_module=cfg['test_module'],
                testcase=args.testcase
                )
    
if __name__ == "__main__":
    args = parse_command_line()
    if args.seed:
        os.environ["RANDOM_SEED"] = args.seed
    build_and_run(args)
