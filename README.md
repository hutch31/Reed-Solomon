# Reed-Solomon Decoder Generator

## Setup Environment

I chose Ubuntu 22.04 as the OS because all the tools below work perfectly on it.

These are the required tools to build the decoder:

- **Chisel tools**: JDK, Scala, SBT

- **Python3**

- **Cocotb**

- **Reedsolo package**

- **Verilator** (if you want to check the waveforms)

- **Docker** (if you prefer to build and test the decoder in a container)

All tools are listed in the `Dockerfile`, which you can use to avoid installing everything manually. After setting up the environment, clone the project and update its submodules using the following commands:

```
git clone --recurse-submodules https://github.com/egorman44/Reed-Solomon.git
git submodule update --init --recursive
git submodule update --remote --recursive
```

## Build and Run Docker Container

To build a Docker container image and run a container from it, use the following commands:

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