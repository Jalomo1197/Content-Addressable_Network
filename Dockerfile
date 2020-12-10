FROM hseeberger/scala-sbt:11.0.6_1.3.10_2.13.1

COPY ./ ./

RUN sbt compile clean package




#FROM openjdk:14-jdk

