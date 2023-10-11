FROM gcc:latest

# Set encoding
ENV LANG=C.UTF-8

# Install project dependencies
RUN apt-get update; \
  apt-get install -y --no-install-recommends \
  wget \
  make \
  pkg-config \
  valgrind \
  gdb

# Set the working directory
WORKDIR /app
