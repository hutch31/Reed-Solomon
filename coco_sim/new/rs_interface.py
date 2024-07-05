from axis import AxisIf
from config import BUS_WIDTH, T_LEN, REDUNDANCY

class RsIfBuilder():

    def __init__(self, dut):
        self._builder = {}
        self._dut = dut
        self.register_if('sAxisIf', self.gen_sAxisIf)
        self.register_if('syndIf', self.gen_syndIf)
        self.register_if('errLocIf', self.gen_errLocIf)
        self.register_if('errPosIf', self.gen_errPosIf)
        self.register_if('errValIf', self.gen_errValIf)
        self.register_if('errPosOutIf', self.gen_errPosOutIf)
        
    def register_if(self, key, value):
        self._builder[key] = value
        
    def get_if(self, if_name):
        # Gen function that builds an interface
        gen_if_func = self._builder.get(if_name)
        if not gen_if_func:
            raise ValueError(f"Not expected value for if_name = {if_name}.")
        return gen_if_func()

    '''
    Interface generation methods
    '''
    
    def gen_sAxisIf(self):
        tdata = []        
        for i in range(BUS_WIDTH):
            tdata.append(eval(f"self._dut.io_sAxisIf_bits_tdata_{i}"))
        if_inst = AxisIf(name='sAxisIf',
                         aclk=self._dut.clock,
                         tdata=tdata,
                         tvalid=self._dut.io_sAxisIf_valid,
                         tkeep=self._dut.io_sAxisIf_bits_tkeep,
                         tlast=self._dut.io_sAxisIf_bits_tlast,
                         unpack='chisel_vec',
                         width=BUS_WIDTH)

        return if_inst

    def gen_syndIf(self):
        tdata = []
        for i in range(REDUNDANCY):
            tdata.append(eval(f"self._dut.io_syndIf_bits_{i}"))
            
        if_inst = AxisIf(name='syndIf',
                         aclk=self._dut.clock,
                         tvalid=self._dut.io_syndIf_valid,
                         tlast=self._dut.io_syndIf_valid,
                         tdata=tdata,
                         unpack='chisel_vec',
                         width=REDUNDANCY)
        return if_inst

    def gen_errLocIf(self):
        tdata = []
        for i in range(T_LEN+1):
            tdata.append(eval(f"self._dut.io_errLocIf_bits_vec_{i}"))
        if_inst = AxisIf(name='errLocIf',
                         aclk=self._dut.clock,
                         tdata=tdata,
                         tvalid=self._dut.io_errLocIf_valid,
                         tlast=self._dut.io_errLocIf_valid,
                         tkeep=self._dut.io_errLocIf_bits_ffs,
                         tkeep_type='ffs',
                         unpack='chisel_vec',
                         width=T_LEN+1)
        return if_inst

    def gen_errPosIf(self):
        t_data = []
        for i in range(T_LEN):
            t_data.append(eval(f"self._dut.io_errPosIf_bits_vec_{i}"))            
        if_inst = AxisIf(name='errPosIf',
                         aclk=self._dut.clock,
                         tdata=t_data,
                         tvalid=self._dut.io_errPosIf_valid,
                         tlast=self._dut.io_errPosIf_valid,
                         tkeep=self._dut.io_errPosIf_bits_ffs,
                         unpack='chisel_vec',
                         width=T_LEN)
        
        return if_inst

    def gen_errValIf(self):
        tdata = []
        for i in range(T_LEN):
            tdata.append(eval(f"self._dut.io_errValIf_bits_vec_{i}"))            
        if_inst = AxisIf(name='errValIf',
                         aclk=self._dut.clock,
                         tdata=tdata,
                         tvalid=self._dut.io_errValIf_valid,
                         tlast=self._dut.io_errValIf_valid,
                         tkeep=self._dut.io_errValIf_bits_ffs,
                         unpack='chisel_vec',
                         width=T_LEN,
                         tkeep_type='ffs')
        return if_inst

    def gen_errPosOutIf(self):
        t_data = []
        for i in range(T_LEN):
            t_data.append(eval(f"self._dut.io_errPosIf_bits_vec_{i}"))            
        if_inst = AxisIf(name='errPosIf',
                         aclk=self._dut.clock,
                         tdata=t_data,
                         tvalid=self._dut.io_errValIf_valid,
                         tlast=self._dut.io_errValIf_valid,
                         tkeep=self._dut.io_errPosIf_bits_ffs,
                         unpack='chisel_vec',
                         width=T_LEN)
        
        return if_inst
        
