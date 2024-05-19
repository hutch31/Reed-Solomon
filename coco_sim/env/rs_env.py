from rs_forney_env import RsForneyEnv
from rs_chien_env import RsChienEnv
from rs_decoder_env import RsDecoderEnv

def gen_env(dut):
    if(dut.name == 'RsForney'):
        env = RsForneyEnv(dut)
    elif(dut.name == 'RsChien'):
        env = RsChienEnv(dut)
    elif(dut.name == 'RsDecoder'):
        env = RsDecoderEnv(dut)
    else:
        raise ValueError(dut.name)    
    return env
        
