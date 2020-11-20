FROM haskell:8.10.2 AS build

RUN mkdir /App
RUN cabal update

COPY vaquita.cabal /App
WORKDIR /App

RUN cabal build --only-dependencies

COPY . /App
RUN cabal install
RUN cd /root/.cabal/bin; cp $(readlink vaquita) /App/vaquita

FROM ubuntu:latest
RUN apt update; apt install -y ca-certificates
COPY --from=build /App/vaquita /bin/vaquita
EXPOSE 8080
ENTRYPOINT ["vaquita"]
