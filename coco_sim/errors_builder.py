import random

class ErrorsBuilder():

    def __init__(self, N_LEN, T_LEN):
        self.err_positions = []
        self.cntr = 0
        self.T_LEN = T_LEN
        self.N_LEN = N_LEN
        self._builder = {}
        self.register_builder()
        
    def register_builder(self):
        self._builder['random_error']     = self.random_error
        self._builder['cover_all_errors'] = self.cover_all_errors
        self._builder['cover_all_errors'] = self.cover_all_errors
        self._builder['error_burst']      = self.error_burst
        self._builder['static_error']     = self.static_error
        self._builder['min_max']          = self.min_max
        self._builder['uncorrupted_msg']  = self.uncorrupted_msg
        
    def generate_error(self, error_type):
        error_func = self._builder.get(error_type)
        if not error_func:
            raise ValueError(f"Not expected value for error_type = {error_type}.")
        return error_func()
        
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
