FROM haskell:8.2.2

LABEL maintainer tomw08@gmail.com

COPY . /opt/poker-server

RUN apt-get update && apt-get install -y libpq-dev

WORKDIR /opt/poker-server

ENV dbConnStr="host=127.0.0.1 port=5432 user=postgres dbname=postgres password=postgres"
ENV secret=wwaaifidsa9109f0dasfda-=2-13

RUN stack setup


# Cache some dependencies
RUN stack --resolver lts-11.13 build mtl wuss async wai-cors wai-logger listsafe split text transformers jwt warp MonadRandom random-shuffle wai websockets servant-server monad-loops servant-js lens aeson hedis monad-logger array stm generic-arbitrary ekg containers cryptohash bytestring persistent persistent-postgresql persistent-template servant servant-client servant-options

CMD ["bash"]
