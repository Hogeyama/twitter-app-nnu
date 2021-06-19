
################################################################################
# Dependency
################################################################################

FROM haskell:8.10.4-buster as dependency
ENV LANG C.UTF-8
RUN apt-get update && apt-get install -y \
    postgresql-11 \
    postgresql-server-dev-11
RUN mkdir -p /opt/name-update-2434/bin
RUN mkdir -p /opt/name-update-2434/src
WORKDIR /opt/name-update-2434/src
COPY ./cabal.project.freeze /opt/name-update-2434/src/cabal.project.freeze
RUN cabal init -p name-update && cabal update && cabal install --lib \
  rio \
  generic-lens \
  microlens-platform \
  aeson \
  amazonka-dynamodb \
  dhall
COPY ./name-update.cabal /opt/name-update-2434/src/name-update.cabal
RUN cabal v2-build --only-dependencies

################################################################################
# Build
################################################################################

FROM dependency as builder

# Build & install application.
ARG GIT_REVISION=UNKNOWN
COPY Setup.hs /opt/name-update-2434/src/
COPY LICENSE /opt/name-update-2434/src/
COPY app /opt/name-update-2434/src/app
COPY lib /opt/name-update-2434/src/lib
COPY conf /opt/name-update-2434/src/conf
COPY test /opt/name-update-2434/src/test
RUN export GIT_REVISION="$GIT_REVISION" \
 && cabal v2-build \
 && cabal v2-test \
 && cabal v2-install --installdir=/opt/name-update-2434/bin --install-method=copy

################################################################################
# Run
################################################################################

FROM debian:buster-slim as runner
COPY --from=builder /opt/name-update-2434/ /opt/name-update-2434/
COPY --from=builder /usr/lib/x86_64-linux-gnu/* /usr/lib/x86_64-linux-gnu/
COPY --from=builder /lib/x86_64-linux-gnu/* /lib/x86_64-linux-gnu/
COPY --from=builder /usr/bin/curl /usr/bin/curl

# Add the apiuser and setup their PATH.
RUN useradd -ms /bin/bash apiuser
RUN chown -R apiuser:apiuser /opt/name-update-2434
RUN apt-get update && apt-get install -y ca-certificates
USER apiuser

WORKDIR /opt/name-update-2434/src
CMD /opt/name-update-2434/bin/name-update-2434
