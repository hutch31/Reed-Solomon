# Use the official Ubuntu base image
FROM ubuntu:22.04

# Set environment variables to non-interactive to avoid prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Install dependencies
RUN apt-get update && \
    apt-get install -y git help2man perl make autoconf g++ flex bison ccache libgoogle-perftools-dev numactl perl-doc && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install dependencies
RUN apt-get update && \
    apt-get install -y python3.10 python3-pip && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install cocotb and cocotb-bus
RUN pip install cocotb==1.8.1 cocotb-bus 

# Install latest reed-solo
RUN pip install --upgrade reedsolo

# Install Verilator
RUN git clone https://github.com/verilator/verilator.git && \
    cd verilator && \
    git checkout v5.022 && \
    autoconf && \
    ./configure && \
    make -j$(nproc) && \
    make install && \
    cd .. && \
    rm -rf verilator

# Set the working directory
WORKDIR /app

# Copy the current directory contents into the container at /app
COPY . /app

# Default command
CMD ["bash"]# Use an official Debian runtime as a parent image