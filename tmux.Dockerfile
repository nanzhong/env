FROM ubuntu:18.04
ARG gitref=master
RUN apt-get update && apt-get -qy install \
    build-essential git automake pkg-config \
    libevent-dev \
    libncurses-dev    
RUN git clone https://github.com/tmux/tmux.git /root/tmux
WORKDIR /root/tmux
RUN git checkout $gitref
RUN ./autogen.sh
RUN ./configure --enable-static
RUN make -j$(nproc)
RUN make DESTDIR=/tmp/tmux install
RUN make install
