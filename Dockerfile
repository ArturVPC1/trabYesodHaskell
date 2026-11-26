FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    ghc libghc-yesod-dev libghc-persistent-sqlite-dev \
    libghc-persistent-template-dev libghc-http-conduit-dev \
    libghc-aeson-dev libghc-yesod-form-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY Main.hs .

RUN ghc -O1 -o agendamento Main.hs && \
    rm -f *.hi *.o

EXPOSE 3000

CMD ["./agendamento"]
