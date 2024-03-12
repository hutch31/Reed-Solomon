import random
from rs_param import *

from rs import init_tables
from rs import gf_poly_eval
from rs import gf_pow
from rs import rs_calc_syndromes
from rs import rs_find_error_locator
from rs import gf_log

from coco_env.packet import Packet
from rs_lib import RsPacket
from rs_lib import RsErrBitPositionPacket
from rs_lib import RsErrPositionPacket

class RandomErrors():
    def __init__(self, pkt_num = T_LEN):
        init_tables()
        self.pkt_num = pkt_num
        self.ref_msg = []
        self.enc_msg = []
        self.cor_msg = []
        self.pkt_cntr = 0
        self.gen_error()

    def gen_error(self):
        for i in range(self.pkt_num):
            err_num = random.randint(1, T_LEN)
            err_pos = random.sample(range(0, N_LEN-1), err_num)
            self.gen_msg(err_pos)
        
    def gen_msg(self, err_pos):
        print(f"err_pos = {err_pos}")
        ref_msg = Packet(name=f'ref_msg{self.pkt_cntr}')
        ref_msg.generate(K_LEN)
        enc_msg = RsPacket(name=f'enc_msg{self.pkt_cntr}', n_len=N_LEN, roots_num=ROOTS_NUM)
        enc_msg.rs_gen_data(msg=ref_msg.data)
            
        #Create cor_msg based on the same ref_msg
        cor_msg = RsPacket(name=f'cor_msg{self.pkt_cntr}', n_len=N_LEN, roots_num=ROOTS_NUM)
        cor_msg.rs_gen_data(msg=ref_msg.data)
        cor_msg.corrupt_pkt(err_pos)        
        #cor_msg.compare(enc_msg)
        
        self.ref_msg.append(ref_msg)
        self.enc_msg.append(enc_msg)
        self.cor_msg.append(cor_msg)
        self.pkt_cntr += self.pkt_cntr
    
class CoverAllErrors(RandomErrors):

    def __init__(self, pkt_num=T_LEN):
        super().__init__(pkt_num)
        
    def gen_error(self):
        for i in range(1, T_LEN+1):
            err_pos = random.sample(range(0, N_LEN-T_LEN), i)
            self.gen_msg(err_pos)            

class ErrorBurst(CoverAllErrors):

    def __init__(self, pkt_num = T_LEN):
        super().__init__(pkt_num)

    def gen_error(self):
        base_pos = random.sample(range(0, N_LEN-T_LEN), self.pkt_num)
        for pos in base_pos:
            err_pos = []
            for k in range (T_LEN):
                err_pos.append(pos+k)
            self.gen_msg(err_pos)            

            
