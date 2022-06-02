################################################################################
# Build
################################################################################

FROM nixos/nix as builder
RUN mkdir -p /opt/name-update-2434/
WORKDIR /opt/name-update-2434/
COPY . ./
RUN nix-build
WORKDIR /opt/name-update-2434/result
CMD /opt/name-update-2434/result/name-update-2434

################################################################################
# TwitterBot
################################################################################

FROM debian:buster-slim as runner
RUN apt-get update && apt-get install -y ca-certificates
COPY --from=builder /opt/name-update-2434/result /opt/name-update-2434/result
WORKDIR /opt/name-update-2434/result
ENV LD_LIBRARY_PATH=/lib/x86_64-linux-gnu
CMD /opt/name-update-2434/result/name-update-2434
