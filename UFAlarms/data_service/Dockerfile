
FROM ubuntu:14.04

# apt-get packages
RUN sudo apt-get update
RUN sudo apt-get -y upgrade
RUN sudo apt-get -y --fix-missing install git libtool autoconf automake pkg-config g++ software-properties-common curl nodejs npm

# libzmq & dependencies.
RUN apt-get install --reinstall make # shouldn't be required but 'make' for libsodium couldn't find it.
RUN git clone https://github.com/jedisct1/libsodium.git && cd libsodium && \
  ./autogen.sh && ./configure && make && make install
RUN git clone https://github.com/zeromq/libzmq.git && cd libzmq && \
  ./autogen.sh && ./configure && make && make install

# java 8
RUN echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections && \
  add-apt-repository -y ppa:webupd8team/java && \
  apt-get update && \
  apt-get install -y oracle-java8-installer && \
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /var/cache/oracle-jdk8-installer
ENV JAVA_HOME /usr/lib/jvm/java-8-oracle

# jzmq
RUN git clone https://github.com/zeromq/jzmq.git && cd jzmq && \
  ./autogen.sh && ./configure && make && make install

# leiningen
RUN sudo curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > /bin/lein
RUN sudo chmod a+x /bin/lein
ENV LEIN_ROOT=1
RUN lein upgrade

# nodejs must be named 'node' for tooling
RUN sudo mv /usr/bin/nodejs /usr/bin/node
RUN ln -s /usr/bin/node /usr/bin/nodejs

# GNU parallel
RUN wget http://ftp.gnu.org/gnu/parallel/parallel-20150722.tar.bz2
RUN tar jxf parallel-20150722.tar.bz2 && cd parallel-20150722 && ./configure && make && sudo make install

# elasticsearch
RUN wget https://download.elasticsearch.org/elasticsearch/elasticsearch/elasticsearch-1.4.2.deb
RUN sudo dpkg -i elasticsearch-1.4.2.deb
RUN sudo update-rc.d elasticsearch defaults 95 10

# add run script
ADD ./deploy/run.sh /root/run.sh
RUN chmod a+x /root/run.sh

# github deploy key & ssh config
ADD ./deploy/id_rsa /root/.ssh/id_rsa
RUN chmod 400 /root/.ssh/id_rsa
RUN echo "    IdentityFile /root/.ssh/id_rsa" >> /etc/ssh/ssh_config
RUN echo "Host github.com\n\tStrictHostKeyChecking no\n" >> ~/.ssh/config

# clone & cache dependencies for Farallones
RUN mkdir -p /src
RUN cd /src && git clone git@github.com:Xeralux/Farallones.git
RUN cd /src/Farallones/data_service && make

# expose ports
EXPOSE 3000
