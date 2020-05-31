FROM erlang:22-slim

RUN apt-get update \
    && apt-get install -y --no-install-recommends git ca-certificates watch

RUN mkdir /buildroot
WORKDIR /buildroot
COPY anoner anoner
COPY store store
COPY server server
COPY ./start.sh start.sh
COPY ./conf.config conf.config
RUN git clone --depth=1 https://github.com/mochi/mochiweb.git server/deps/mochiweb
WORKDIR /buildroot/server/deps/mochiweb
RUN ./rebar co
WORKDIR /buildroot
EXPOSE 8050

CMD ["tail", "-f", "/dev/null"]
# CMD ["./start.sh"]
