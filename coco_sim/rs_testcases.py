import cocotb
from ref_msg import *
from rs_env import gen_env

@cocotb.test()
async def random_error(dut):
    stimulus = RandomErrors(10)
    test = gen_env(dut)
    test.set_if()
    test.build_env()
    test.gen_stimilus(stimulus)
    await test.run()
    test.post_run()

@cocotb.test()
async def cover_all_errors(dut):
    stimulus = CoverAllErrors()
    test = gen_env(dut)
    test.set_if()
    test.build_env()
    test.gen_stimilus(stimulus)
    await test.run()
    test.post_run()

@cocotb.test()
async def butch_errors(dut):
    stimulus = ErrorBurst()
    test = gen_env(dut)
    test.set_if()
    test.build_env()
    test.gen_stimilus(stimulus)
    await test.run()
    test.post_run()
    
@cocotb.test()
async def static_packet(dut):
    stimulus = StaticError()
    test = gen_env(dut)
    test.set_if()
    test.build_env()
    test.gen_stimilus(stimulus)
    await test.run()
    test.post_run()
