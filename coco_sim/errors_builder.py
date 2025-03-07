# ----------------------------------------------------------------------
#  Copyright (c) 2024 Egor Smirnov
#
#  Licensed under terms of the MIT license
#  See https://github.com/egorman44/Reed-Solomon/blob/main/LICENSE
#    for license terms
# ----------------------------------------------------------------------

import random

class ErrorsBuilder():

    def __init__(self, N_LEN, T_LEN, SYMB_WIDTH):
        self.cntr = 0
        self.T_LEN = T_LEN
        self.N_LEN = N_LEN
        self.SYMB_WIDTH = SYMB_WIDTH
        self._builder_pos = {}
        self._builder_val = {}
        self.register_builder()

    
    def register_builder(self):
        self._builder_pos['random_error']     = self.random_error
        self._builder_pos['cover_all_errors'] = self.cover_all_errors
        self._builder_pos['error_burst']      = self.error_burst
        self._builder_pos['static_error']     = self.static_error
        self._builder_pos['min_max']          = self.min_max
        self._builder_pos['uncorrupted_msg']  = self.uncorrupted_msg
        self._builder_val['random_error_val'] = self.random_error_val
        self._builder_val['bit_error_val']    = self.bit_error_val
        
    def generate_error(self, error_pos_type, error_val_type='random_error_val'):
        error_pos_func = self._builder_pos.get(error_pos_type)
        error_val_func = self._builder_val.get(error_val_type)
        if not error_pos_func:
            raise ValueError(f"Not expected value for error_pos_type = {error_pos_type}.")
        if not error_val_func:
            raise ValueError(f"Not expected value for error_val_type = {error_val_type}.")
        error_pos = error_pos_func()
        error_val = error_val_func(len(error_pos))
        return error_pos, error_val 
    
    # Error positions functions
        
    def random_error(self):
        err_num = random.randint(1, self.T_LEN)
        err_positions = random.sample(range(0, self.N_LEN-1), err_num)
        return err_positions

    def cover_all_errors(self):
        err_positions = random.sample(range(0, self.N_LEN-self.T_LEN), self.cntr+1)
        self.cntr += 1
        return err_positions    

    def error_burst(self):
        base_pos = random.randint(0, self.N_LEN-self.T_LEN)
        err_positions = []
        for i in range (self.T_LEN):
            err_positions.append(base_pos+i)
        return err_positions

    def uncorrupted_msg(self):
        if self.cntr == 1:
            err_num = random.randint(1, self.T_LEN)
            err_positions = random.sample(range(0, self.N_LEN-1), err_num)
        else:
            err_positions = []
        self.cntr += 1
        return err_positions

    def static_error(self):
        err_positions = list(range(self.cntr+1))
        self.cntr += 1
        return err_positions
    
    def min_max(self):
        err_num = 1 if random.random() < 0.5 else (self.T_LEN)
        print(f"err_num = {err_num}")
        err_positions = random.sample(range(0, self.N_LEN-self.T_LEN), err_num)
        return err_positions    

    # Error values
    def random_error_val(self, err_num):
        err_values = []
        for i in range(err_num):
            err_values.append(random.randint(1, 2** self.SYMB_WIDTH-1))
        return err_values
    
    def bit_error_val(self, err_num):
        err_values = []
        for i in range(err_num):
            bit_position = random.randint(0, self.symb_width-1)
            error = 1 << bit_position
            
#err_gen = ErrorsBuilder(255, 8)
#
#print("RANDOM ERROR:")
#for _ in range(8):
#    print(err_gen.generate_error('random_error'))
#
#print("COVER_ALL:")
#for _ in range(8):
#    print(err_gen.generate_error('cover_all_errors'))
#
#print("ERROR_BURST:")
#for _ in range(8):
#    print(err_gen.generate_error('error_burst'))
#
#err_gen.cntr = 0
#print("STATIC_ERROR")
#for _ in range(8):
#    print(err_gen.generate_error('static_error'))
#
