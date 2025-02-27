# Reed-Solomon Decoder Generator

## Setup Environment
These are the required tools to build the decoder:

- Clone project and update submodules

- **Ubuntu**
- **Chisel tools**: JDK, Scala, SBT
- **Python3**
- **Cocotb**
- **Reedsolo package**
- **Verilator** (if you want to check the waveforms)
- **Docker** (if you prefer to build and test the decoder in a container)

All tools are listed in the `Dockerfile`, which you can use to avoid installing everything manually.

## Docker Usage
To build and run the decoder in a Docker container, use the following commands:

```sh
docker build -t chisel-flow .
docker run -it -v <prj_folder>:/app -w /app chisel_flow
```

## Generate Decoder
To generate SystemVerilog files for the decoder, run the following commands:

```sh
runMain Rs.GenRsBlockRecovery --axis-clock 156.25 --core-clock 156.25 --symb-width 8 --bus-width 8 --poly 285 --fcr 0 --n-len 255 --k-len 239
runMain Rs.GenRsBlockRecovery --axis-clock 156.25 --core-clock 156.25 --symb-width 8 --bus-width 16 --poly 285 --fcr 0 --n-len 108 --k-len 106
```

## Run Cocotb Tests
To run Cocotb tests, use the following command:

```sh
python3 rs_decoder.py -l RsBlockRecovery
```
