import json
import math
from pathlib import Path
import os
import sys

# Set environment to load modules we need
def get_path(module_path):
    if module_path not in os.environ:
        print(f"[ERROR] {module_path} env var is not set.")
    else:
        return os.getenv(module_path)
    
# Read JSON
def read_json(file_path):
    with open(file_path, 'r') as file:
        ip_params = json.load(file)
    return ip_params

# Load all project directories and configuration.
PRJ_DIR = Path(__file__).resolve().parent.parent
coco_path = Path(__file__).resolve().parent

sys.path.append(str(coco_path/'coco_env'))
sys.path.append(str(coco_path/'coco_axis'))


ip_params = read_json(PRJ_DIR/'rs.json')

N_LEN        = ip_params['N_LEN']
K_LEN        = ip_params['K_LEN']
REDUNDANCY   = ip_params['N_LEN'] - ip_params['K_LEN']
T_LEN        = math.floor(REDUNDANCY/2)
BUS_WIDTH    = ip_params['BUS_WIDTH']
POLY         = ip_params['POLY']
MSG_DURATION = math.ceil(N_LEN/BUS_WIDTH)
FCR          = ip_params['FCR']
SYMB_WIDTH   = ip_params['SYMB_WIDTH']
AXIS_CLOCK   = ip_params['AXIS_CLOCK']
CORE_CLOCK   = ip_params['CORE_CLOCK']

if AXIS_CLOCK/CORE_CLOCK == 1.0:
    SINGLE_CLOCK = True
else:
    SINGLE_CLOCK = False
    
print(f"PRJ_DIR : {PRJ_DIR}")
print("\n")
print("================")
print("RS CONFIGURATION")
print("================")
print(f"\t AXIS_CLOCK = {AXIS_CLOCK}")
print(f"\t CORE_CLOCK = {CORE_CLOCK}")
print(f"\t BUS_WIDTH  = {BUS_WIDTH}")
print(f"\t SINGLE_CLOCK = {SINGLE_CLOCK}")
print(f"\t POLY  = {POLY}")
print(f"\t N_LEN = {N_LEN}")
print(f"\t K_LEN = {K_LEN}")
print(f"\t T_LEN = {T_LEN}")

class RsConfig():

    def __init__(self):
        self.ip_params = read_json(PRJ_DIR/'rs.json')
        
    def get_code_cfg(self):
        code_cfg = {}
        code_cfg['SYMB_WIDTH'] = self.ip_params['SYMB_WIDTH']
        code_cfg['POLY'] = self.ip_params['POLY']
        code_cfg['N_LEN'] = self.ip_params['N_LEN']
        code_cfg['K_LEN'] = self.ip_params['K_LEN']
        code_cfg['REDUNDANCY'] = code_cfg['N_LEN'] - code_cfg['K_LEN']
        code_cfg['T_LEN'] = math.floor(code_cfg['REDUNDANCY']/2)
        return code_cfg

    def get_env_cfg(self, args):
        env_cfg = {}
        m_if = {}
        s_if = {}
        env_cfg['BUS_WIDTH'] = self.ip_params['BUS_WIDTH']
        if args.hdl_toplevel == 'RsSynd':
            s_if['sAxisIf'] = None
            m_if['syndIf'] = None
        elif args.hdl_toplevel == 'RsBm':
            s_if['syndIf'] = None
            m_if['errLocIf'] = None
        elif args.hdl_toplevel == 'RsChien':
            s_if['errLocIf'] = None
            m_if['errPosIf'] = None
        elif args.hdl_toplevel == 'RsForney':
            s_if['errPosIf'] = None
            s_if['syndIf'] = None
            m_if['errValIf'] = None
        elif args.hdl_toplevel == 'RsDecoder':
            s_if['sAxisIf'] = None
            m_if['errValIf'] = None
        else:
            raise ValueError(f"Not expected value for args.hdl_toplevel = {args.hdl_toplevel}")
        env_cfg['s_if'] = s_if
        env_cfg['m_if'] = m_if
        return env_cfg
    
