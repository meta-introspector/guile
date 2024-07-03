from debian

WORKDIR /opt/guile
RUN apt update
#RUN apt install build-essential
ENV LD_LIBRARY_PATH /usr/local/lib

RUN  apt-get update -qq -y
RUN  apt-get install -qq -y \
  pkg-config       \
  libltdl7-dev     \
  libgmp-dev       \
  libunistring-dev \
  libffi-dev       \
  libgc-dev \
  gperf  \
  make \
  g++

#ADD configure /opt/guile/configure
#RUN ./configure
#RUN make