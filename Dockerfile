FROM hseeberger/scala-sbt:11.0.6_1.3.10_2.13.1

WORKDIR /app

COPY ./ ./

RUN sbt clean assembly

ENTRYPOINT bash

#FROM openjdk:14-jdk

#docker build -t hello-scala .
#docker run -it hello-scala

# enable wsl-2 docker
# cp app/target/scala-2.13/app-assembly-0.1-SNAPSHOT.jar .