FROM erlang:slim

RUN mkdir /buildroot
WORKDIR /buildroot
COPY anoner anoner
COPY store store
COPY ./start.sh start.sh

CMD ["./start.sh"]
