FROM ubuntu:18.04
ARG gitref=master
RUN apt-get update && apt-get -qy install \
    build-essential git autoconf pkg-config \
    texinfo \
    libgnutls28-dev \
    libncurses5-dev
RUN git clone https://git.savannah.gnu.org/git/emacs.git /root/emacs
WORKDIR /root/emacs
RUN git checkout $gitref
RUN ./autogen.sh
RUN ./configure --with-modules --with-file-notification=yes
RUN make -j$(nproc)
RUN make DESTDIR=/tmp/emacs install
RUN make install