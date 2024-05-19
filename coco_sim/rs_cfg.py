import os
from pathlib import Path

proj_path = Path(__file__).resolve().parent.parent
sim = os.getenv("SIM", "verilator")
print(f"proj_path: {proj_path}")

cfg_forney = {
    'test_module' : 'rs_decoder',
    'hdl_toplevel' : 'RsForney',
    'hdl_toplevel_lang' : os.getenv("HDL_TOPLEVEL_LANG", "verilog"),
    'sim' : sim,
    'proj_path' : proj_path,
    'verilog_sources' : [proj_path / "RsForney.sv"],
    'parameters' : {},
    'defines' : {},
    'includes' : {},
    'build_args' : [ '--timing', '--assert' , '--trace' , '--trace-structs', '--trace-max-array', '512', '--trace-max-width', '512']
}

cfg_chien = {
    'test_module' : 'rs_decoder',
    'hdl_toplevel' : 'RsChien',
    'hdl_toplevel_lang' : os.getenv("HDL_TOPLEVEL_LANG", "verilog"),
    'sim' : sim,
    'proj_path' : proj_path,
    'verilog_sources' : [proj_path / "RsChien.sv"],
    'parameters' : {},
    'defines' : {},
    'includes' : {},
    'build_args' : [ '--timing', '--assert' , '--trace' , '--trace-structs', '--trace-max-array', '512', '--trace-max-width', '512']
}

cfg_decoder = {
    'test_module' : 'rs_decoder',
    'hdl_toplevel' : 'RsDecoder',
    'hdl_toplevel_lang' : os.getenv("HDL_TOPLEVEL_LANG", "verilog"),
    'sim' : sim,
    'proj_path' : proj_path,
    'verilog_sources' : [proj_path / "RsDecoder.sv"],
    'parameters' : {},
    'defines' : {},
    'includes' : {},
    'build_args' : [ '--timing', '--assert' , '--trace' , '--trace-structs', '--trace-max-array', '512', '--trace-max-width', '512']
}

cfg_dict = {
    'RsChien' : cfg_chien,
    'RsForney' : cfg_forney,
    'RsDecoder' : cfg_decoder,
}


