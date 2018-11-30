FROM node

RUN apt-get update
RUN mkdir /app
WORKDIR /app
COPY . .
RUN npm i electron
RUN npm i
ENTRYPOINT ["npm start"]