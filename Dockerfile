FROM haskell:8.10.4 AS build-stage
# if you encounter "version `GLIBC_2.28' not found" error when running
# chat client executable, build with the following base image instead:
# FROM haskell:8.10.4-stretch AS build-stage
COPY . /project
WORKDIR /project
RUN stack install

FROM scratch AS export-stage
COPY --from=build-stage /root/.local/bin/simplex-chat /
